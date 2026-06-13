;;; emms-lyrics-sync-display-render.el --- Buffer-insert rendering functions  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-render.el
;; Created : 2026-06-13 05:35 UTC
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
;;               emms-lyrics-sync-display-lyrics-height physical lines, always.
;;               Past and future lines are selected by accumulating PHYSICAL
;;               row counts (via count-wrapped-lines) until per-section budgets
;;               are exhausted.  A line that would overflow the remaining
;;               budget is not added; the remainder is padded with blank lines.
;;               This guarantees the waveform position never moves.
;;               Current line: color only (no bold), amber #ffcb6b.
;;               Long lines are word-wrapped with equal left/right margins.
;;               When current line wraps to cur-height physical lines,
;;               n-after is reduced by (cur-height - 1) so total stays fixed.
;;             • PRE-ROLL (before first timestamp): line 0 is shown in
;;               future-line-face at its current-slot position (n-bef blank
;;               rows above, separators on both sides).  When its timestamp
;;               fires, it turns amber in-place with zero positional shift.
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
                         (let ((cn (cdr (assq 'codec_name audio))))
                           (when (stringp cn)
                             (setf (emms-lyrics-sync-track-codec track)
                                   (upcase cn))))
                         (let ((sr (cdr (assq 'sample_rate audio))))
                           (when (stringp sr)
                             (setf (emms-lyrics-sync-track-sample-rate track)
                                   (string-to-number sr))))
                         (let* ((b (cdr (assq 'bits_per_raw_sample audio)))
                                (n (if (stringp b) (string-to-number b)
                                     (or b 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bits-per-sample
                                    track) n)))
                         (let* ((raw (or (cdr (assq 'bit_rate audio))
                                         (and fmt (cdr (assq 'bit_rate fmt)))))
                                (n   (if (stringp raw) (string-to-number raw)
                                       (or raw 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bitrate track)
                                   (round (/ n 1000.0)))))
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
  "Format sample-rate HZ as \"44.1\" or \"48\" kHz for the tech line."
  (when (and (integerp hz) (> hz 0))
    (let* ((khz (/ hz 1000))
           (rem (% hz 1000)))
      (if (zerop rem)
          (number-to-string khz)
        (format "%d.%d" khz (/ rem 100))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the metadata header for TRACK at point.
Returns (elapsed-buf-start . elapsed-buf-end) — exact buffer positions
of the elapsed text for marker-based incremental updates, or nil if
the elapsed line was not inserted.

Header layout:
  Line 1: Artist[ - Composer]             ← always
  Line 2: Album                           ← omitted when absent
  Line 3: [Track. ]Title                  ← always
  Line 4: CODEC | bits/kHz | kbps | ch   ← omitted when all fields nil
  Line 5: elapsed / duration              ← always

All lines centered independently using string-width (no :height/:weight
faces so character width equals string-width in all cases)."
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
         ;; Line 1: Artist[ - Composer]
         (l1 (concat (or artist "Unknown Artist")
                     (if (and composer (not (string-empty-p composer)))
                         (concat " - " composer)
                       "")))
         ;; Line 2: Album (nil → skip entirely)
         (l2 (when (and album (not (string-empty-p album))) album))
         ;; Line 3: [Track. ]Title
         (l3 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; Line 4: tech fields (nil → skip entirely)
         (sr-s   (emms-lyrics-sync-display--sr-string sr))
         (bps/sr (cond ((and bps sr-s) (format "%d/%s" bps sr-s))
                       (sr-s           sr-s)
                       (t              nil)))
         (kbps-s (when (and kbps (> kbps 0)) (format "%d kbps" kbps)))
         (l4     (let ((parts (delq nil (list codec bps/sr kbps-s ch))))
                   (when parts (mapconcat #'identity parts " | "))))
         ;; Line 5: elapsed / duration — split for marker recording
         (elapsed-str (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str     (emms-lyrics-sync-display--format-time dur))
         (l5-suffix   (concat " / " dur-str))
         elapsed-start elapsed-end)
    ;; ── Insert lines ─────────────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    (when l2
      (insert (propertize (emms-lyrics-sync-display--center l2)
                          'face 'emms-lyrics-sync-album-face) "\n"))
    (insert (propertize (emms-lyrics-sync-display--center l3)
                        'face 'emms-lyrics-sync-title-face) "\n")
    (when l4
      (insert (propertize (emms-lyrics-sync-display--center l4)
                          'face 'emms-lyrics-sync-tech-face) "\n"))
    ;; Line 5: insert in two halves, recording elapsed position via (point)
    (let* ((l5-full  (concat elapsed-str l5-suffix))
           (pad      (max 0 (/ (- (emms-lyrics-sync-display--body-width)
                                   (string-width l5-full))
                                2))))
      (insert (propertize (make-string pad ?\s) 'face 'emms-lyrics-sync-tech-face))
      (setq elapsed-start (point))
      (insert (propertize elapsed-str 'face 'emms-lyrics-sync-elapsed-face))
      (setq elapsed-end (point))
      (insert (propertize l5-suffix   'face 'emms-lyrics-sync-tech-face)))
    (insert "\n")
    (cons elapsed-start elapsed-end)))

;;; ── Lyrics: Wrapping Helper ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--count-wrapped-lines (text)
  "Return the number of physical screen lines TEXT occupies after word-wrap.
Uses the current window body width.  Never returns less than 1."
  (if (or (null text) (string-empty-p text))
      1
    (let* ((width  (emms-lyrics-sync-display--body-width))
           (words  (split-string text))
           (count  1)
           (used   0))
      (dolist (w words)
        (let ((wlen (1+ (string-width w)))) ; +1 for space
          (if (> (+ used wlen) width)
              (setq count (1+ count)
                    used  (string-width w))
            (setq used (+ used wlen)))))
      count)))

(defun emms-lyrics-sync-display--wrap-and-insert (text face)
  "Insert TEXT with FACE, word-wrapped and centered within body width."
  (if (or (null text) (string-empty-p (string-trim text)))
      (insert "\n")
    (let* ((width  (emms-lyrics-sync-display--body-width))
           (words  (split-string text))
           lines current-line current-width)
      ;; Greedy word-wrap
      (dolist (w words)
        (let ((wlen (string-width w)))
          (if (null current-line)
              (setq current-line (list w)
                    current-width wlen)
            (if (<= (+ current-width 1 wlen) width)
                (setq current-line  (append current-line (list w))
                      current-width (+ current-width 1 wlen))
              (push current-line lines)
              (setq current-line  (list w)
                    current-width wlen)))))
      (when current-line (push current-line lines))
      (setq lines (nreverse lines))
      ;; Insert each physical line, centered
      (dolist (seg lines)
        (let* ((seg-str (mapconcat #'identity seg " "))
               (pad     (max 0 (/ (- width (string-width seg-str)) 2))))
          (insert (make-string pad ?\s))
          (insert (propertize seg-str 'face face))
          (insert "\n"))))))

;;; ── Lyrics: A2 Word-Level Render ─────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE at point, centered, recording word positions.
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples."
  (let* ((words     (emms-lyrics-sync-line-words line))
         (line-text (emms-lyrics-sync-line-text  line))
         (width     (emms-lyrics-sync-display--body-width))
         (len       (string-width (or line-text "")))
         (pad       (max 0 (/ (- width len) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        ;; Insert with future-line-face as base; overlays paint sung/current
        (insert (propertize (emms-lyrics-sync-word-text word)
                            'face 'emms-lyrics-sync-future-line-face))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

;;; ── Lyrics: Context Window with Fixed Physical Height ────────────────────────

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.

Always occupies exactly `emms-lyrics-sync-display-lyrics-height' physical
screen lines regardless of line wrapping or playback position.

NORMAL PATH (cur-idx ≥ 0):
  Past and future lines are collected by accumulating PHYSICAL row counts
  (via `count-wrapped-lines') until per-section budgets are exhausted.
  A line that would overflow the remaining budget is excluded; the
  remainder is padded with blank lines.

  Total = n-before + 1 + cur-height + 1 + n-after - (cur-height-1)
        = n-before + n-after + 3  ← constant ✓

PRE-ROLL PATH (cur-idx < 0, before first timestamp):
  Line 0 is placed in its future current-slot position using
  `emms-lyrics-sync-future-line-face'.  When its timestamp fires, it
  turns amber (current-face) in-place with ZERO positional shift.

  Layout:
    n-before blank rows   (top-pad: no past lines yet)
    1 blank row           (separator before current slot)
    line[0] rows          (future-face, occupying the current-slot)
    1 blank row           (separator after current slot)
    lines[1..k] rows      (future-face, up to fut-budget physical rows)
    bot-pad blank rows

  Total = n-before + 1 + first-height + 1 + fut-budget
        = n-before + n-after + 3 ✓

Sets `emms-lyrics-sync-display--word-positions' when current line is A2."
  (setq emms-lyrics-sync-display--word-positions nil)
  (let* ((n-bef  emms-lyrics-sync-display-context-before)
         (n-aft  emms-lyrics-sync-display-context-after))
    (cond

     ;; ── No lyrics ──────────────────────────────────────────────────────────
     ((null doc)
      (let ((total emms-lyrics-sync-display-lyrics-height))
        ;; Centre "(no lyrics)" on the middle row, pad rest with blank lines
        (dotimes (_ (/ total 2)) (insert "\n"))
        (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                            'face 'emms-lyrics-sync-past-line-face) "\n")
        (let ((remaining (- total (/ total 2) 1)))
          (dotimes (_ (max 0 remaining)) (insert "\n")))))

     ;; ── Plain (unsynced) text ───────────────────────────────────────────────
     ((emms-lyrics-sync-lrc-doc-plain-p doc)
      (let ((lines (emms-lyrics-sync-lrc-doc-lines doc)))
        (cl-loop for line across lines do
          (emms-lyrics-sync-display--wrap-and-insert
           (emms-lyrics-sync-line-text line)
           'emms-lyrics-sync-future-line-face))))

     ;; ── Synced LRC ─────────────────────────────────────────────────────────
     (t
      (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
             (n       (length lines))
             (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms)))

        (if (< cur-idx 0)

            ;; ── Pre-roll: before first timestamp ────────────────────────────
            ;; Position line 0 in its current-slot so the display doesn't
            ;; jump when the first lyric fires.
            (if (= n 0)
                ;; Edge case: synced doc with no lines — fill with blanks
                (dotimes (_ emms-lyrics-sync-display-lyrics-height) (insert "\n"))

              (let* ((first-line   (aref lines 0))
                     (first-text   (emms-lyrics-sync-line-text first-line))
                     ;; Physical rows of line 0 (A2 lines never wrap)
                     (first-height (if (emms-lyrics-sync-line-words first-line)
                                       1
                                     (emms-lyrics-sync-display--count-wrapped-lines
                                      first-text)))
                     ;; fut-budget: how many physical future rows after line 0
                     (fut-budget   (max 0 (- n-aft (- first-height 1))))
                     ;; Collect lines[1..] up to fut-budget physical rows.
                     ;; A line that would exceed the remaining budget is skipped.
                     (future-lines
                      (let (acc (budget fut-budget))
                        (cl-loop for i from 1 below n
                                 for line = (aref lines i)
                                 for ltext = (emms-lyrics-sync-line-text line)
                                 for lphys = (if (emms-lyrics-sync-line-words line)
                                                 1
                                               (emms-lyrics-sync-display--count-wrapped-lines
                                                ltext))
                                 while (and (> budget 0) (<= lphys budget))
                                 do (push line acc)
                                    (cl-decf budget lphys))
                        (nreverse acc)))
                     (future-phys
                      (cl-loop for line in future-lines
                               sum (if (emms-lyrics-sync-line-words line)
                                       1
                                     (emms-lyrics-sync-display--count-wrapped-lines
                                      (emms-lyrics-sync-line-text line)))))
                     (bot-pad (max 0 (- fut-budget future-phys))))
                ;; Top padding — n-bef blank rows (no past lines yet)
                (dotimes (_ n-bef) (insert "\n"))
                ;; Blank separator before the current slot
                (insert "\n")
                ;; Line 0 in the current-slot position, shown in future-face.
                ;; When its timestamp fires it turns amber with no positional shift.
                (emms-lyrics-sync-display--wrap-and-insert
                 first-text 'emms-lyrics-sync-future-line-face)
                ;; Blank separator after the current slot
                (insert "\n")
                ;; Future lines (line 1 onward) — same positions they will hold
                ;; once line 0 becomes current
                (dolist (line future-lines)
                  (emms-lyrics-sync-display--wrap-and-insert
                   (emms-lyrics-sync-line-text line)
                   'emms-lyrics-sync-future-line-face))
                ;; Bottom padding
                (dotimes (_ bot-pad) (insert "\n"))))

          ;; ── Normal: render fixed-height context window ───────────────────
          (let* ((cur-line   (aref lines cur-idx))
                 (cur-text   (emms-lyrics-sync-line-text cur-line))
                 ;; Physical rows of the current line (A2 lines never wrap)
                 (cur-height (if (emms-lyrics-sync-line-words cur-line)
                                 1
                               (emms-lyrics-sync-display--count-wrapped-lines cur-text)))
                 ;; Reduce future budget when current line wraps
                 (fut-budget (max 0 (- n-aft (- cur-height 1))))
                 ;; ── Collect past lines ──────────────────────────────────────
                 ;; Walk backward from cur-idx, accumulate PHYSICAL rows.
                 ;; A line that would exceed the remaining budget is excluded.
                 ;; push+downfrom yields forward (oldest-first) order directly.
                 (past-lines
                  (let (acc (budget n-bef))
                    (cl-loop for i downfrom (1- cur-idx) to 0
                             for line = (aref lines i)
                             for ltext = (emms-lyrics-sync-line-text line)
                             for lphys = (if (emms-lyrics-sync-line-words line)
                                             1
                                           (emms-lyrics-sync-display--count-wrapped-lines
                                            ltext))
                             while (and (> budget 0) (<= lphys budget))
                             do (push line acc)
                                (cl-decf budget lphys))
                    acc))  ; already oldest-first due to push+downfrom
                 (past-phys
                  (cl-loop for line in past-lines
                           sum (if (emms-lyrics-sync-line-words line)
                                   1
                                 (emms-lyrics-sync-display--count-wrapped-lines
                                  (emms-lyrics-sync-line-text line)))))
                 (top-pad (max 0 (- n-bef past-phys)))
                 ;; ── Collect future lines ────────────────────────────────────
                 ;; Walk forward from cur-idx+1, accumulate physical rows.
                 (future-lines
                  (let (acc (budget fut-budget))
                    (cl-loop for i from (1+ cur-idx) below n
                             for line = (aref lines i)
                             for ltext = (emms-lyrics-sync-line-text line)
                             for lphys = (if (emms-lyrics-sync-line-words line)
                                             1
                                           (emms-lyrics-sync-display--count-wrapped-lines
                                            ltext))
                             while (and (> budget 0) (<= lphys budget))
                             do (push line acc)
                                (cl-decf budget lphys))
                    (nreverse acc)))
                 (future-phys
                  (cl-loop for line in future-lines
                           sum (if (emms-lyrics-sync-line-words line)
                                   1
                                 (emms-lyrics-sync-display--count-wrapped-lines
                                  (emms-lyrics-sync-line-text line)))))
                 (bot-pad (max 0 (- fut-budget future-phys))))
            ;; Top padding
            (dotimes (_ top-pad) (insert "\n"))
            ;; Past lines (oldest first, past-face)
            (dolist (line past-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-line-text line)
               'emms-lyrics-sync-past-line-face))
            ;; Blank separator before current
            (insert "\n")
            ;; Current line — A2 records word positions; others use current-face
            (if (emms-lyrics-sync-line-words cur-line)
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              (emms-lyrics-sync-display--wrap-and-insert
               cur-text 'emms-lyrics-sync-current-line-face))
            ;; Blank separator after current
            (insert "\n")
            ;; Future lines
            (dolist (line future-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-line-text line)
               'emms-lyrics-sync-future-line-face))
            ;; Bottom padding
            (dotimes (_ bot-pad) (insert "\n")))))))))

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
  "Delete all sung-word overlays."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition current-word and sung-word overlays for POS-MS."
  (when (and emms-lyrics-sync-display--word-positions
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((ov           (emms-lyrics-sync-display--ensure-word-overlay))
            (found-cursor nil))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (cl-loop
         for triple across emms-lyrics-sync-display--word-positions
         for buf-start = (nth 0 triple)
         for buf-end   = (nth 1 triple)
         for word      = (nth 2 triple)
         for w-start   = (emms-lyrics-sync-word-start-ms word)
         for w-end     = (or (emms-lyrics-sync-word-end-ms word)
                             most-positive-fixnum)
         do
         (cond
          ((and (>= pos-ms w-start) (< pos-ms w-end) (not found-cursor))
           (setq found-cursor t)
           (move-overlay ov buf-start buf-end
                         emms-lyrics-sync-display--buffer))
          ((< w-end pos-ms)
           (let ((sung (make-overlay buf-start buf-end
                                    emms-lyrics-sync-display--buffer)))
             (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
             (overlay-put sung 'priority 9)
             (push sung emms-lyrics-sync-display--sung-overlays)))))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────

(defun emms-lyrics-sync-display--update-elapsed (elapsed-s)
  "Replace only the elapsed-time text in the header for ELAPSED-S seconds.
Marker invariant:
  elapsed-marker     NIL-type → stays at start of elapsed text.
  elapsed-end-marker T-type   → advances past newly inserted text.
After delete-region both markers are at the same point; after insert the
end-marker advances to the new end — next call deletes exactly the right span."
  (when (and (markerp emms-lyrics-sync-display--elapsed-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-marker)
             (markerp emms-lyrics-sync-display--elapsed-end-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-end-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t)
            (new-str (emms-lyrics-sync-display--format-time elapsed-s)))
        (save-excursion
          (let ((start (marker-position emms-lyrics-sync-display--elapsed-marker))
                (end   (marker-position emms-lyrics-sync-display--elapsed-end-marker)))
            ;; Guard: both positions must be valid numbers
            (when (and (integerp start) (integerp end) (<= start end))
              (goto-char start)
              (delete-region start end)
              (insert (propertize new-str
                                  'face 'emms-lyrics-sync-elapsed-face)))))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
