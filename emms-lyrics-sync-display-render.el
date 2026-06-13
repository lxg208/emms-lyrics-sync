;;; emms-lyrics-sync-display-render.el --- Buffer-insert rendering functions  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-render.el
;; Created : 2026-06-12 22:10 UTC
;; Purpose : All buffer-insert rendering functions for the emms-lyrics-sync
;;           display subsystem.  Covers:
;;             • Cover art — synchronous ffmpeg extraction to a STABLE cache
;;               path (never a temp file) + sidecar search + memory cache.
;;               Stable path prevents "Cannot find image file" errors caused
;;               by Emacs lazy-loading image pixels after a temp file is gone.
;;             • Header rendering — 3-line metadata block matching foobar2000
;;               waveform seekbar format:
;;                 Artist[ - Composer] - Album
;;                 [Track. ]Title
;;                 [CODEC | bits/kHz | kbps kbps | channels | ]elapsed / duration
;;               Elapsed text position is recorded by inserting the tech line
;;               in two parts and calling (point) directly — no string-search,
;;               no fragile offset arithmetic.
;;             • ffprobe async augmentation — populates codec, sample-rate,
;;               bits-per-sample, bitrate, channel-layout from the audio file.
;;               Uses nested if instead of cl-return-from to avoid cl-block
;;               scope issues.  Runs only once per track (guarded by codec slot).
;;             • Lyrics rendering — context window using lrc-seek directly.
;;             • A2 word-level render with buffer-position capture for overlays.
;;             • Overlay helpers for word-current and word-sung highlighting.
;;             • Elapsed-time incremental update.
;;               Marker invariant: elapsed-marker NIL type (stays at start),
;;               elapsed-end-marker T type (advances past inserted content).
;;               delete-region always removes exactly the current elapsed text;
;;               insert always lands at the correct start position.
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
Returns the stable path on success, nil on failure or absent ffmpeg.
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
  "Return a (possibly cached) Emacs image for the cover of FILE-PATH, or nil.
Resolution order: memory cache → stable embedded extract → sidecar file."
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
;;
;; EMMS backends typically do not populate info-codec, info-samplerate,
;; info-bits-per-sample, info-bitrate, or info-channels.  We augment the
;; emms-lyrics-sync-track struct asynchronously via ffprobe so those fields
;; are available for the header tech line.
;;
;; Called from full-redraw, but ONLY when track-codec is nil — this prevents
;; an infinite loop where the ffprobe completion callback fires full-redraw
;; which would otherwise call ffprobe again.

(defun emms-lyrics-sync-display--ffprobe-augment (track callback)
  "Augment TRACK tech fields asynchronously via ffprobe; call CALLBACK when done.
CALLBACK is called with no arguments after the struct is updated in-place.
Calls CALLBACK immediately (no-op) when:
  - ffprobe is unavailable or TRACK has no file-path
  - TRACK already has codec set (already augmented)"
  (let ((fp (emms-lyrics-sync-track-file-path track)))
    (if (not (and fp (executable-find "ffprobe")))
        (funcall callback)
      (if (emms-lyrics-sync-track-codec track)
          ;; Already augmented — skip ffprobe to avoid callback loop
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
                            (fmt    (cdr (assq 'format obj)))
                            ;; codec_name: upcase for display (flac → FLAC)
                            (codec  (let ((c (and audio
                                                  (cdr (assq 'codec_name audio)))))
                                      (and c (upcase c))))
                            ;; sample_rate arrives as a JSON string "44100"
                            (sr     (let ((s (and audio
                                                  (cdr (assq 'sample_rate audio)))))
                                      (and s (string-to-number s))))
                            ;; bits_per_raw_sample: ignore 0 (lossy codecs)
                            (bps    (let* ((b (and audio
                                                   (cdr (assq 'bits_per_raw_sample
                                                              audio))))
                                           (n (if (stringp b)
                                                  (string-to-number b)
                                                (or b 0))))
                                      (and (> n 0) n)))
                            ;; bitrate: prefer stream, fallback to format
                            (kbps   (let* ((raw (or (and audio
                                                         (cdr (assq 'bit_rate audio)))
                                                    (and fmt
                                                         (cdr (assq 'bit_rate fmt)))))
                                           (n   (if (stringp raw)
                                                    (string-to-number raw)
                                                  (or raw 0))))
                                      (and (> n 0) (round (/ n 1000.0)))))
                            ;; channels: prefer channel_layout string ("stereo")
                            ;;           over raw channel count integer
                            (ch     (let ((layout (and audio
                                                       (cdr (assq 'channel_layout
                                                                  audio))))
                                          (count  (and audio
                                                       (cdr (assq 'channels audio)))))
                                      (cond
                                       ((and (stringp layout)
                                             (not (string-empty-p layout)))
                                        layout)
                                       ((integerp count)
                                        (format "%d ch" count))
                                       (t nil)))))
                       (when codec (setf (emms-lyrics-sync-track-codec          track) codec))
                       (when sr    (setf (emms-lyrics-sync-track-sample-rate    track) sr))
                       (when bps   (setf (emms-lyrics-sync-track-bits-per-sample track) bps))
                       (when kbps  (setf (emms-lyrics-sync-track-bitrate        track) kbps))
                       (when ch    (setf (emms-lyrics-sync-track-channels       track) ch)))
                   (error nil))
                 (funcall callback))))))))))

;;; ── Header Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--sr-string (hz)
  "Format sample-rate HZ as \"44.1\" or \"48\" for the tech line.
Matches foobar2000 waveform seekbar format:
  44100 → \"44.1\"   48000 → \"48\"   88200 → \"88.2\"   96000 → \"96\""
  (when (and (integerp hz) (> hz 0))
    (let* ((khz  (/ hz 1000))
           (rem  (% hz 1000)))
      (if (zerop rem)
          (number-to-string khz)
        ;; First decimal digit only: 44100 → 44.1, 22050 → 22.0, 88200 → 88.2
        (format "%d.%d" khz (/ rem 100))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the 3-line metadata header for TRACK at point.
Returns (elapsed-buf-start . elapsed-buf-end) — exact buffer positions
bracketing the elapsed text, used by `emms-lyrics-sync-display--update-elapsed'.

The tech line is inserted in two parts so that (point) can be recorded
directly before and after the elapsed text — no string-search needed.

Header format (foobar2000 waveform seekbar style):
  Line 1: Artist[ - Composer] - Album               (centered)
  Line 2: [Track. ]Title                             (centered)
  Line 3: [CODEC | bits/kHz | kbps | channels | ]elapsed / duration (centered)"
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
         ;; ── Line 1: Artist[ - Composer] - Album ──────────────────────────
         (l1 (mapconcat #'identity
                        (delq nil (list (or artist "Unknown Artist")
                                        composer
                                        album))
                        " - "))
         ;; ── Line 2: [Track. ]Title ────────────────────────────────────────
         (l2 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; ── Line 3 static parts ───────────────────────────────────────────
         (sr-s     (emms-lyrics-sync-display--sr-string sr))
         (bps/sr   (cond ((and (integerp bps) (> bps 0) sr-s)
                          (format "%d/%s" bps sr-s))
                         (sr-s sr-s)
                         (t    nil)))
         (kbps-str (when (and (integerp kbps) (> kbps 0))
                     (format "%d kbps" kbps)))
         (static   (mapconcat #'identity
                              (delq nil (list codec bps/sr kbps-str ch))
                              " | "))
         ;; ── Line 3 dynamic parts ──────────────────────────────────────────
         (elapsed-str (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str     (emms-lyrics-sync-display--format-time dur))
         (l3-suffix   (concat " / " dur-str))
         (l3-prefix   (if (string-empty-p static) "" (concat static " | ")))
         (l3-full     (concat l3-prefix elapsed-str l3-suffix))
         (pad-chars   (max 0 (/ (- (emms-lyrics-sync-display--body-width)
                                    (string-width l3-full))
                                 2)))
         elapsed-start elapsed-end)
    ;; ── Insert line 1 ────────────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    ;; ── Insert line 2 ────────────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l2)
                        'face 'emms-lyrics-sync-title-face) "\n")
    ;; ── Insert line 3 in two parts so we record elapsed position directly ────
    (let ((tech 'emms-lyrics-sync-tech-face))
      (insert (propertize (make-string pad-chars ?\s) 'face tech))
      (unless (string-empty-p static)
        (insert (propertize (concat static " | ") 'face tech)))
      ;; Record start position immediately before elapsed text
      (setq elapsed-start (point))
      (insert (propertize elapsed-str 'face 'emms-lyrics-sync-elapsed-face))
      ;; Record end position immediately after elapsed text
      (setq elapsed-end (point))
      (insert (propertize l3-suffix 'face tech)))
    (insert "\n")
    (cons elapsed-start elapsed-end)))

;;; ── Lyrics Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE centered at point, recording word positions.
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples in
forward order, for use by `emms-lyrics-sync-display--update-word-overlays'."
  (let* ((words     (emms-lyrics-sync-line-words line))
         (line-text (emms-lyrics-sync-line-text  line))
         (width     (emms-lyrics-sync-display--body-width))
         (len       (string-width line-text))
         (pad       (max 0 (/ (- width len) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        (insert (emms-lyrics-sync-word-text word))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

(defun emms-lyrics-sync-display--render-plain-line (line face)
  "Insert LINE text with FACE, centered."
  (insert (propertize (emms-lyrics-sync-display--center
                       (emms-lyrics-sync-line-text line))
                      'face face)
          "\n"))

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.
Sets `emms-lyrics-sync-display--word-positions' when the current line is A2.

Uses `emms-lyrics-sync-lrc-seek' directly (binary search) rather than
`emms-lyrics-sync-lrc-context' to avoid the alist-key mismatch that caused
the always-show-first-N-lines regression."
  (setq emms-lyrics-sync-display--word-positions nil)
  (cond
   ;; No lyrics available
   ((null doc)
    (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                        'face 'emms-lyrics-sync-past-line-face) "\n"))
   ;; Plain (unsynced) text: show all lines
   ((emms-lyrics-sync-lrc-doc-plain-p doc)
    (cl-loop for line across (emms-lyrics-sync-lrc-doc-lines doc) do
      (emms-lyrics-sync-display--render-plain-line
       line 'emms-lyrics-sync-future-line-face)))
   ;; Synced LRC: context window around the active line
   (t
    (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
           (n       (length lines))
           (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms))
           (n-bef   emms-lyrics-sync-display-context-before)
           (n-aft   emms-lyrics-sync-display-context-after))
      (if (< cur-idx 0)
          ;; Before first timestamp: show upcoming lines as preview
          (cl-loop for i from 0 below (min n-aft n) do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-future-line-face))
        ;; Normal: render [start … cur-idx … end] context window
        (let ((start (max 0      (- cur-idx n-bef)))
              (end   (min (1- n) (+ cur-idx n-aft))))
          ;; Past lines (before current)
          (cl-loop for i from start below cur-idx do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-past-line-face))
          ;; Current line — blank lines for visual emphasis
          (insert "\n")
          (let ((cur-line (aref lines cur-idx)))
            (if (emms-lyrics-sync-line-words cur-line)
                ;; A2: per-word render with position capture
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              ;; Standard LRC: whole-line face
              (emms-lyrics-sync-display--render-plain-line
               cur-line 'emms-lyrics-sync-current-line-face)))
          (insert "\n")
          ;; Future lines (after current)
          (cl-loop for i from (1+ cur-idx) to end do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-future-line-face))))))))

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
                ((and (>= pos-ms w-start) (< pos-ms w-end) (not found-cursor))
                 (setq found-cursor t)
                 (move-overlay ov buf-start buf-end
                               emms-lyrics-sync-display--buffer))
                ;; Already sung
                ((< w-end pos-ms)
                 (let ((sung (make-overlay buf-start buf-end
                                          emms-lyrics-sync-display--buffer)))
                   (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
                   (push sung emms-lyrics-sync-display--sung-overlays))))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────
;;
;; Marker invariant (set in full-redraw via copy-marker):
;;   elapsed-marker      — insertion-type NIL: stays at start of elapsed text.
;;   elapsed-end-marker  — insertion-type T:   advances past inserted content.
;;
;; Update procedure:
;;   1. delete-region [elapsed-marker, elapsed-end-marker]
;;      → removes old elapsed text; both markers collapse to the same position.
;;   2. insert new elapsed text at elapsed-marker position (point after delete).
;;      → elapsed-marker (NIL) stays at start.
;;      → elapsed-end-marker (T) advances to end of new text.
;;
;; This invariant holds regardless of string length changes between updates.

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
          (let ((start (marker-position emms-lyrics-sync-display--elapsed-marker))
                (end   (marker-position emms-lyrics-sync-display--elapsed-end-marker)))
            (goto-char start)
            (delete-region start end)
            (insert (propertize new-str
                                'face 'emms-lyrics-sync-elapsed-face))))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
