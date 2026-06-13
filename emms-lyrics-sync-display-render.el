;;; emms-lyrics-sync-display-render.el --- Buffer-insert rendering functions  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-render.el
;; Created : 2026-06-13 04:25 UTC
;; Purpose : All buffer-insert rendering functions for the emms-lyrics-sync
;;           display subsystem.  Covers:
;;             • Cover art — synchronous ffmpeg extraction to a STABLE cache
;;               path (never a temp file) + sidecar search + memory cache.
;;             • Header rendering — 5-line metadata block:
;;                 Line 1: Artist[ - Composer]
;;                 Line 2: Album  (omitted entirely when absent)
;;                 Line 3: [Track. ]Title
;;                 Line 4: CODEC | bits/kHz | kbps kbps | channels
;;                 Line 5: elapsed / duration
;;               Each line is independently centered.  Elapsed position is
;;               recorded by inserting line 5 in two parts and calling
;;               (point) directly — no string-search.
;;             • ffprobe async augmentation — populates codec, sample-rate,
;;               bits-per-sample, bitrate, channel-layout.  Guarded by codec
;;               slot to prevent infinite redraw loop.
;;             • Lyrics rendering — FIXED HEIGHT of
;;               (context-before + 1 + context-after + 2) lines, always.
;;               Missing lines above/below are padded with blank lines so the
;;               waveform never moves.
;;               Current line: color only (no bold), amber #ffcb6b.
;;               Long lines are word-wrapped with equal left/right margins.
;;               When current line wraps to cur-height physical lines,
;;               n-after is reduced by (cur-height - 1) so total stays fixed.
;;             • A2 word-level render with buffer-position capture.
;;             • Overlay helpers for word-current and word-sung highlighting.
;;             • Elapsed-time incremental update.
;;               Marker invariant: elapsed-marker NIL type (stays at start),
;;               elapsed-end-marker T type (advances past inserted content).
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Load order within the display subsystem:
;;   1. emms-lyrics-sync-display-vars.el   ← customization, faces, state
;;   2. emms-lyrics-sync-display-render.el ← this file
;;   3. emms-lyrics-sync-display-redraw.el ← redraw orchestration
;;   4. emms-lyrics-sync-display.el        ← timer, hooks (entry point)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)

;;; ── Cover Art ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--find-cover-file (file-path)
  "Return a readable sidecar cover image path for FILE-PATH, or nil."
  (when file-path
    (let ((dir (file-name-directory (expand-file-name file-path))))
      (or (cl-loop for name in emms-lyrics-sync-display-cover-filenames
                   for p = (expand-file-name name dir)
                   when (file-readable-p p) return p)
          (car (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t))))))

(defun emms-lyrics-sync-display--cover-stable-path (file-path)
  "Return a stable on-disk cache path for the extracted cover of FILE-PATH.
Format: <emms-lyrics-sync-cache-dir>/covers/<md5-of-path>.jpg"
  (expand-file-name
   (concat (md5 (expand-file-name file-path)) ".jpg")
   (expand-file-name "covers" emms-lyrics-sync-cache-dir)))

(defun emms-lyrics-sync-display--extract-embedded-cover (file-path)
  "Extract embedded cover art from FILE-PATH via ffmpeg (synchronous).
Writes to a STABLE path under emms-lyrics-sync-cache-dir/covers/.
Returns the stable path on success, nil on failure.
The file is NEVER deleted — Emacs lazy-loads image pixels on first paint."
  (when (executable-find "ffmpeg")
    (let ((out (emms-lyrics-sync-display--cover-stable-path file-path)))
      (unless (file-exists-p out)
        (make-directory (file-name-directory out) t)
        (let ((ok (and (= 0 (call-process
                             "ffmpeg" nil nil nil
                             "-y" "-i" (expand-file-name file-path)
                             "-map" "0:v:0" "-frames:v" "1"
                             "-vcodec" "mjpeg" out))
                       (> (or (file-attribute-size (file-attributes out)) 0)
                          100))))
          (unless ok
            (ignore-errors (delete-file out))
            (setq out nil))))
      (and out (file-exists-p out) out))))

(defun emms-lyrics-sync-display--cover-image (file-path)
  "Return a (possibly cached) Emacs image for the cover of FILE-PATH, or nil."
  (when (and (display-graphic-p)
             (> emms-lyrics-sync-display-cover-height 0)
             file-path)
    (or (gethash file-path emms-lyrics-sync-display--cover-cache)
        (let* ((cover (or (emms-lyrics-sync-display--extract-embedded-cover file-path)
                          (emms-lyrics-sync-display--find-cover-file file-path)))
               (img   (when cover
                        (ignore-errors
                          (create-image cover nil nil
                                        :height emms-lyrics-sync-display-cover-height
                                        :scale  1.0)))))
          (when img
            (puthash file-path img emms-lyrics-sync-display--cover-cache))
          img))))

;;; ── ffprobe Tech-Info Augmentation ───────────────────────────────────────────

(defun emms-lyrics-sync-display--ffprobe-augment (track callback)
  "Augment TRACK tech fields asynchronously via ffprobe; call CALLBACK when done.
Calls CALLBACK immediately when ffprobe is unavailable, track has no
file-path, or track already has codec set (avoid re-running per track)."
  (let ((fp (emms-lyrics-sync-track-file-path track)))
    (if (not (and fp (executable-find "ffprobe")))
        (funcall callback)
      (if (emms-lyrics-sync-track-codec track)
          (funcall callback)
        (let ((buf (generate-new-buffer " *emms-ffprobe-tech*")))
          (make-process
           :name    "emms-lyrics-sync-ffprobe"
           :buffer  buf
           :command (list "ffprobe"
                          "-v"            "quiet"
                          "-print_format" "json"
                          "-show_streams"
                          "-show_format"
                          "-select_streams" "a:0"
                          (expand-file-name fp))
           :noquery t
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let ((json (with-current-buffer buf (buffer-string))))
                 (kill-buffer buf)
                 (condition-case nil
                     (let* ((obj    (json-parse-string
                                     json
                                     :object-type  'alist
                                     :null-object  nil
                                     :false-object nil))
                            (strs   (cdr (assq 'streams obj)))
                            (audio  (and strs (> (length strs) 0)
                                         (aref strs 0)))
                            (fmt    (cdr (assq 'format  obj))))
                       (when audio
                         ;; codec
                         (let ((cn (cdr (assq 'codec_name audio))))
                           (when (stringp cn)
                             (setf (emms-lyrics-sync-track-codec track)
                                   (upcase cn))))
                         ;; sample rate
                         (let ((sr (cdr (assq 'sample_rate audio))))
                           (when (stringp sr)
                             (setf (emms-lyrics-sync-track-sample-rate track)
                                   (string-to-number sr))))
                         ;; bits per sample
                         (let* ((b (cdr (assq 'bits_per_raw_sample audio)))
                                (n (if (stringp b) (string-to-number b)
                                     (or b 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bits-per-sample track) n)))
                         ;; bitrate: stream first, format fallback
                         (let* ((raw (or (cdr (assq 'bit_rate audio))
                                         (and fmt (cdr (assq 'bit_rate fmt)))))
                                (n   (if (stringp raw) (string-to-number raw)
                                       (or raw 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bitrate track)
                                   (round (/ n 1000.0)))))
                         ;; channels
                         (let ((layout (cdr (assq 'channel_layout audio)))
                               (count  (cdr (assq 'channels audio))))
                           (cond
                            ((and (stringp layout) (not (string-empty-p layout)))
                             (setf (emms-lyrics-sync-track-channels track) layout))
                            ((integerp count)
                             (setf (emms-lyrics-sync-track-channels track)
                                   (format "%d ch" count)))))))
                   (error nil))
                 (funcall callback))))))))))

;;; ── Header Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--sr-string (hz)
  "Format sample-rate HZ as \"44.1\" or \"48\" kHz for the tech line.
44100 → \"44.1\"   48000 → \"48\"   88200 → \"88.2\"   96000 → \"96\""
  (when (and (integerp hz) (> hz 0))
    (let* ((khz (/ hz 1000))
           (rem (% hz 1000)))
      (if (zerop rem)
          (number-to-string khz)
        (format "%d.%d" khz (/ rem 100))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the metadata header for TRACK at point.
Returns (elapsed-buf-start . elapsed-buf-end).

Layout (foobar2000 style, each line independently centered):
  Line 1: Artist[ - Composer]                        artist-face
  Line 2: Album          (omitted when absent)       album-face
  Line 3: [Track. ]Title                             title-face
  Line 4: CODEC | bits/kHz | kbps | channels         tech-face
           (omitted entirely when all fields nil)
  Line 5: elapsed / duration                         elapsed/tech-face

Face design note: NO :weight bold or :height on any face used here.
Both properties scale character pixel width in GUI Emacs, breaking
string-width–based centering.  Colour alone provides visual hierarchy."
  (let* ((artist   (emms-lyrics-sync-track-artist          track))
         (composer (emms-lyrics-sync-track-composer         track))
         (album    (emms-lyrics-sync-track-album            track))
         (trknum   (emms-lyrics-sync-track-track-number     track))
         (title    (emms-lyrics-sync-track-title            track))
         (codec    (emms-lyrics-sync-track-codec            track))
         (bps      (emms-lyrics-sync-track-bits-per-sample  track))
         (sr       (emms-lyrics-sync-track-sample-rate      track))
         (kbps     (emms-lyrics-sync-track-bitrate          track))
         (ch       (emms-lyrics-sync-track-channels         track))
         (dur      (emms-lyrics-sync-track-duration         track))
         (sr-s     (emms-lyrics-sync-display--sr-string sr))
         ;; ── Line 1: Artist[ - Composer] ──────────────────────────────────
         (l1 (concat (or artist "Unknown Artist")
                     (if composer (concat " - " composer) "")))
         ;; ── Line 2: Album (nil → skip line entirely) ─────────────────────
         (l2 (and (stringp album) (not (string-empty-p album)) album))
         ;; ── Line 3: [Track. ]Title ────────────────────────────────────────
         (l3 (concat (if (and trknum (not (string-empty-p trknum)))
                         (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; ── Line 4: tech fields (nil when all absent) ────────────────────
         (bps/sr (cond ((and bps sr-s) (format "%d/%s" bps sr-s))
                       (sr-s sr-s)
                       (t nil)))
         (kbps-s (when (and (integerp kbps) (> kbps 0))
                   (format "%d kbps" kbps)))
         (l4-parts (delq nil (list codec bps/sr kbps-s ch)))
         (l4 (and l4-parts (mapconcat #'identity l4-parts " | ")))
         ;; ── Line 5: elapsed / duration ────────────────────────────────────
         (elapsed-str (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str     (emms-lyrics-sync-display--format-time dur))
         (l5-suffix   (concat " / " dur-str))
         ;; Compute left pad for line 5 independently (for marker placement)
         (l5-full     (concat elapsed-str l5-suffix))
         (l5-pad      (max 0 (/ (- (emms-lyrics-sync-display--body-width)
                                    (string-width l5-full))
                                 2)))
         elapsed-start elapsed-end)
    ;; ── Insert line 1 ────────────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    ;; ── Insert line 2 (album) — only when present ────────────────────────────
    (when l2
      (insert (propertize (emms-lyrics-sync-display--center l2)
                          'face 'emms-lyrics-sync-album-face) "\n"))
    ;; ── Insert line 3 (title) ────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l3)
                        'face 'emms-lyrics-sync-title-face) "\n")
    ;; ── Insert line 4 (tech) — only when at least one field present ──────────
    (when l4
      (insert (propertize (emms-lyrics-sync-display--center l4)
                          'face 'emms-lyrics-sync-tech-face) "\n"))
    ;; ── Insert line 5 (elapsed / duration) in two parts ─────────────────────
    ;; Inserting in two parts lets us record (point) directly before and after
    ;; the elapsed text — no fragile string-search needed.
    (insert (propertize (make-string l5-pad ?\s)
                        'face 'emms-lyrics-sync-tech-face))
    (setq elapsed-start (point))
    (insert (propertize elapsed-str 'face 'emms-lyrics-sync-elapsed-face))
    (setq elapsed-end (point))
    (insert (propertize l5-suffix 'face 'emms-lyrics-sync-tech-face))
    (insert "\n")
    (cons elapsed-start elapsed-end)))

;;; ── Lyrics Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--wrap-and-center (text face)
  "Insert TEXT with FACE, word-wrapped and centered, at point.
Each physical line is independently centered.
Returns the number of physical lines inserted."
  (let* ((width  (emms-lyrics-sync-display--body-width))
         (words  (split-string (string-trim text) "[ \t]+" t))
         lines current-words current-w)
    ;; Greedy word-wrap
    (dolist (word words)
      (let ((w (string-width word)))
        (if (null current-words)
            (setq current-words (list word) current-w w)
          (if (> (+ current-w 1 w) width)
              (progn
                (push (nreverse current-words) lines)
                (setq current-words (list word) current-w w))
            (push word current-words)
            (setq current-w (+ current-w 1 w))))))
    (when current-words
      (push (nreverse current-words) lines))
    (let ((result (nreverse lines))
          (count  0))
      (dolist (line-words result)
        (let* ((line-str (mapconcat #'identity line-words " "))
               (pad      (max 0 (/ (- width (string-width line-str)) 2))))
          (insert (make-string pad ?\s))
          (insert (propertize line-str 'face face))
          (insert "\n")
          (setq count (1+ count))))
      (max 1 count))))

(defun emms-lyrics-sync-display--count-wrapped-lines (text)
  "Return the number of physical lines TEXT wraps to at current body width.
Uses the same greedy word-wrap algorithm as `wrap-and-center'."
  (let* ((width  (emms-lyrics-sync-display--body-width))
         (words  (split-string (string-trim text) "[ \t]+" t))
         (line-w 0)
         (count  1))
    (dolist (word words)
      (let ((w (string-width word)))
        (if (zerop line-w)
            (setq line-w w)
          (if (> (+ line-w 1 w) width)
              (setq count  (1+ count)
                    line-w w)
            (setq line-w (+ line-w 1 w))))))
    (max 1 count)))

(defun emms-lyrics-sync-display--render-plain-line (line face)
  "Insert LINE text with FACE, word-wrapped and centered."
  (emms-lyrics-sync-display--wrap-and-center
   (emms-lyrics-sync-line-text line) face))

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE centered at point, recording word positions.
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples."
  (let* ((words     (emms-lyrics-sync-line-words line))
         (line-text (emms-lyrics-sync-line-text  line))
         (width     (emms-lyrics-sync-display--body-width))
         (len       (string-width line-text))
         (pad       (max 0 (/ (- width len) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        ;; Insert with current-line-face as base; overlays paint sung/current
        (insert (propertize (emms-lyrics-sync-word-text word)
                            'face 'emms-lyrics-sync-current-line-face))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.

Total height is ALWAYS (n-before + n-after + 3) lines:
  top-pad blank lines  (n-before - available past lines)
  past lines           (up to n-before)
  1 blank line
  current line         (cur-height physical lines, 1 for A2)
  1 blank line
  future lines         (up to eff-n-after = n-after - (cur-height - 1))
  bottom-pad blank lines

When the current line wraps to cur-height > 1 physical lines, eff-n-after
is reduced by (cur-height - 1) so the grand total stays constant."
  (setq emms-lyrics-sync-display--word-positions nil)
  (let* ((n-bef        emms-lyrics-sync-display-context-before)
         (n-aft        emms-lyrics-sync-display-context-after)
         ;; Fixed total = n-bef + n-aft + 3 (2 blank + 1 current baseline)
         (fixed-total  (+ n-bef n-aft 3)))
    (cond
     ;; ── No lyrics ────────────────────────────────────────────────────────────
     ((null doc)
      (dotimes (_ fixed-total) (insert "\n")))

     ;; ── Plain (unsynced) text ─────────────────────────────────────────────────
     ((emms-lyrics-sync-lrc-doc-plain-p doc)
      (cl-loop for line across (emms-lyrics-sync-lrc-doc-lines doc) do
        (emms-lyrics-sync-display--render-plain-line
         line 'emms-lyrics-sync-future-line-face)))

     ;; ── Synced LRC ───────────────────────────────────────────────────────────
     (t
      (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
             (n       (length lines))
             (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms)))
        (if (< cur-idx 0)
            ;; Before first timestamp — top pad + blank current slot + preview
            (let* ((avail-fut (min n n-aft))
                   (bot-pad   (- n-aft avail-fut)))
              (dotimes (_ n-bef)        (insert "\n")) ; top pad
              (insert "\n")                            ; blank before slot
              (insert "\n")                            ; current slot (empty)
              (insert "\n")                            ; blank after slot
              (cl-loop for i from 0 below avail-fut do
                (emms-lyrics-sync-display--render-plain-line
                 (aref lines i) 'emms-lyrics-sync-future-line-face))
              (dotimes (_ (max 0 bot-pad)) (insert "\n")))

          ;; Normal case — compute wrapped height first, then slice
          (let* ((cur-line   (aref lines cur-idx))
                 (a2-p       (not (null (emms-lyrics-sync-line-words cur-line))))
                 ;; A2 lines are never wrapped (word-level render)
                 (cur-height (if a2-p 1
                               (emms-lyrics-sync-display--count-wrapped-lines
                                (emms-lyrics-sync-line-text cur-line))))
                 ;; Reduce n-after by extra lines the current lyric occupies
                 (eff-n-aft  (max 0 (- n-aft (1- cur-height))))
                 (start      (max 0      (- cur-idx n-bef)))
                 (end        (min (1- n) (+ cur-idx eff-n-aft)))
                 (avail-past (- cur-idx start))
                 (avail-fut  (- end cur-idx))
                 (top-pad    (- n-bef avail-past))
                 (bot-pad    (- eff-n-aft avail-fut)))
            ;; Top padding — blank lines when near track start
            (dotimes (_ (max 0 top-pad)) (insert "\n"))
            ;; Past lines
            (cl-loop for i from start below cur-idx do
              (emms-lyrics-sync-display--render-plain-line
               (aref lines i) 'emms-lyrics-sync-past-line-face))
            ;; Blank separator before current line
            (insert "\n")
            ;; Current line
            (if a2-p
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              (emms-lyrics-sync-display--wrap-and-center
               (emms-lyrics-sync-line-text cur-line)
               'emms-lyrics-sync-current-line-face))
            ;; Blank separator after current line
            (insert "\n")
            ;; Future lines
            (cl-loop for i from (1+ cur-idx) to end do
              (emms-lyrics-sync-display--render-plain-line
               (aref lines i) 'emms-lyrics-sync-future-line-face))
            ;; Bottom padding
            (dotimes (_ (max 0 bot-pad)) (insert "\n")))))))))

;;; ── Overlay Management ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--ensure-word-overlay ()
  "Return the word-highlight overlay, creating it if needed."
  (unless (overlayp emms-lyrics-sync-display--word-overlay)
    (setq emms-lyrics-sync-display--word-overlay
          (make-overlay 1 1 emms-lyrics-sync-display--buffer t nil))
    (overlay-put emms-lyrics-sync-display--word-overlay
                 'face 'emms-lyrics-sync-word-current-face)
    (overlay-put emms-lyrics-sync-display--word-overlay 'priority 10))
  emms-lyrics-sync-display--word-overlay)

(defun emms-lyrics-sync-display--clear-sung-overlays ()
  "Delete all sung-word overlays from `emms-lyrics-sync-display--sung-overlays'."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition current-word overlay and paint sung-word overlays for POS-MS."
  (when (and emms-lyrics-sync-display--word-positions
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((ov           (emms-lyrics-sync-display--ensure-word-overlay))
            (found-cursor nil))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (cl-loop for triple across emms-lyrics-sync-display--word-positions
                 for buf-start = (nth 0 triple)
                 for buf-end   = (nth 1 triple)
                 for word      = (nth 2 triple)
                 for w-start   = (emms-lyrics-sync-word-start-ms word)
                 for w-end     = (or (emms-lyrics-sync-word-end-ms word)
                                     most-positive-fixnum)
                 do
                 (cond
                  ;; Currently active word
                  ((and (>= pos-ms w-start) (< pos-ms w-end)
                        (not found-cursor))
                   (setq found-cursor t)
                   (move-overlay ov buf-start buf-end
                                 emms-lyrics-sync-display--buffer))
                  ;; Already sung
                  ((< w-end pos-ms)
                   (let ((sung (make-overlay buf-start buf-end
                                            emms-lyrics-sync-display--buffer)))
                     (overlay-put sung 'face
                                  'emms-lyrics-sync-word-sung-face)
                     (overlay-put sung 'priority 9)
                     (push sung
                           emms-lyrics-sync-display--sung-overlays)))))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────
;;
;; Marker invariant (set in full-redraw via copy-marker):
;;   elapsed-marker      — insertion-type NIL: stays at start of elapsed text.
;;   elapsed-end-marker  — insertion-type T:   advances past inserted content.
;;
;; After delete-region both markers collapse to start.
;; After insert, end-marker (T) advances to end of new text.
;; Next call deletes exactly the previous elapsed text.

(defun emms-lyrics-sync-display--update-elapsed (elapsed-s)
  "Replace only the elapsed-time text in the header for ELAPSED-S seconds."
  (when (and (markerp emms-lyrics-sync-display--elapsed-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-marker)
             (markerp emms-lyrics-sync-display--elapsed-end-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t)
            (new-str (emms-lyrics-sync-display--format-time elapsed-s)))
        (save-excursion
          (let ((start (marker-position
                        emms-lyrics-sync-display--elapsed-marker))
                (end   (marker-position
                        emms-lyrics-sync-display--elapsed-end-marker)))
            (goto-char start)
            (delete-region start end)
            (insert (propertize new-str
                                'face 'emms-lyrics-sync-elapsed-face))))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
