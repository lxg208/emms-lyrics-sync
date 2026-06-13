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
;;               Current line: amber #ffcb6b (color only, no bold).
;;               PRE-ROLL (before first timestamp): line[0] is shown in
;;               future-line-face at the current-slot position so layout is
;;               identical to the normal path.  When its timestamp fires it
;;               turns amber in-place with zero positional shift.
;;             • A2 word-level render — Bug 2 fixes:
;;               - Centering uses concatenated word text (NOT raw line-text
;;                 which contains <mm:ss.cc> markup).
;;               - current-line-face (amber) is the base face for all word
;;                 text so the line reads amber for future/unplayed words.
;;               - word-sung-face overlay (green, priority 90) paints already-
;;                 sung words.
;;               - word-current-face overlay (yellow+underline, priority 100)
;;                 paints the active word.
;;               - When no word is active the cursor overlay is collapsed to
;;                 (1,1) so no stale highlight is visible.
;;               - ensure-word-overlay checks the overlay is still in the
;;                 correct buffer before reusing it.
;;             • reset-word-overlay — collapses the cursor overlay to (1,1)
;;               before any lyrics redraw to prevent stale highlighting.
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
the elapsed line was not inserted."
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
         (l1 (concat (or artist "Unknown Artist")
                     (if (and composer (not (string-empty-p composer)))
                         (concat " - " composer)
                       "")))
         (l2 (when (and album (not (string-empty-p album))) album))
         (l3 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         (sr-s   (emms-lyrics-sync-display--sr-string sr))
         (bps/sr (cond ((and bps sr-s) (format "%d/%s" bps sr-s))
                       (sr-s           sr-s)
                       (t              nil)))
         (kbps-s (when (and kbps (> kbps 0)) (format "%d kbps" kbps)))
         (l4     (let ((parts (delq nil (list codec bps/sr kbps-s ch))))
                   (when parts (mapconcat #'identity parts " | "))))
         (elapsed-str (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str     (emms-lyrics-sync-display--format-time dur))
         (l5-suffix   (concat " / " dur-str))
         elapsed-start elapsed-end)
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

;;; ── Lyrics: Wrapping Helpers ─────────────────────────────────────────────────

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
      (dolist (seg lines)
        (let* ((seg-str (mapconcat #'identity seg " "))
               (pad     (max 0 (/ (- width (string-width seg-str)) 2))))
          (insert (make-string pad ?\s))
          (insert (propertize seg-str 'face face))
          (insert "\n"))))))

;;; ── Lyrics: Visual Text Helper ───────────────────────────────────────────────

(defun emms-lyrics-sync-display--line-visual-text (line)
  "Return the visible display text of LINE, stripping A2 word-timestamp markup.
For A2 lines the `text' slot contains raw markup like
  \"<00:10.50>Hello, <00:11.00>it's <00:11.50>me\"
which gives wrong centering and wrong `count-wrapped-lines' results.
This function returns the concatenated word texts (\"Hello, it's me\")
for A2 lines and `line-text' unchanged for standard lines."
  (let ((words (emms-lyrics-sync-line-words line)))
    (if words
        (mapconcat #'emms-lyrics-sync-word-text words "")
      (emms-lyrics-sync-line-text line))))

;;; ── Lyrics: A2 Word-Level Render ─────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE at point, centered, recording word positions.

Base face is `current-line-face' (amber) so the line reads amber for
future/unplayed words — consistent with the non-A2 current-line color.
Sung-word and current-word overlays paint over the amber base:
  amber → green (sung) → yellow+underline (current word)

Centering uses the concatenated word texts, NOT the raw line-text which
contains <mm:ss.cc> markup and would give a wildly wrong pad value.

Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples."
  (let* ((words    (emms-lyrics-sync-line-words line))
         (vis-text (mapconcat #'emms-lyrics-sync-word-text words ""))
         (width    (emms-lyrics-sync-display--body-width))
         (pad      (max 0 (/ (- width (string-width vis-text)) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        ;; Amber base: overlays (sung=green, current=yellow+underline) paint on
        ;; top with priority 90/100, so base face only shows for future words.
        (insert (propertize (emms-lyrics-sync-word-text word)
                            'face 'emms-lyrics-sync-current-line-face))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

;;; ── Lyrics: Context Window with Fixed Physical Height ────────────────────────

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.
Always occupies exactly `emms-lyrics-sync-display-lyrics-height' physical
screen lines regardless of how many context lines are available or how
many physical rows the current line wraps to.

Budget algorithm (Bug 1 fix — physical rows, not logical line counts):
  1. cur-height = count-wrapped-lines(visual-text(cur-line)), or 1 for A2.
  2. fut-budget = n-after - (cur-height - 1).
  3. Walk BACKWARD from cur-idx-1: accumulate count-wrapped-lines until
     budget n-before is exhausted.  A line whose physical count would
     overflow the remaining budget is excluded entirely (not truncated).
  4. Walk FORWARD from cur-idx+1: same physical accumulation for fut-budget.
  5. top-pad = n-before - past-phys   blank lines
  6. bot-pad = fut-budget - fut-phys  blank lines

Total = n-before + 1 + cur-height + 1 + n-after - (cur-height-1)
      = n-before + n-after + 3  =  lyrics-height  (constant)

PRE-ROLL layout (cur-idx < 0): line[0] shown in future-face at the
current-slot position (n-bef blank rows above it, separators on both
sides).  When its timestamp fires the normal path renders it in amber
in-place — zero layout shift, waveform stays locked.

Sets `emms-lyrics-sync-display--word-positions' when current line is A2."
  (setq emms-lyrics-sync-display--word-positions nil)
  (let* ((n-bef  emms-lyrics-sync-display-context-before)
         (n-aft  emms-lyrics-sync-display-context-after))
    (cond

     ;; ── No lyrics ──────────────────────────────────────────────────────────
     ((null doc)
      (let ((total emms-lyrics-sync-display-lyrics-height))
        (dotimes (_ (/ total 2)) (insert "\n"))
        (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                            'face 'emms-lyrics-sync-past-line-face) "\n")
        (dotimes (_ (max 0 (- total (/ total 2) 1))) (insert "\n"))))

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

            ;; ── PRE-ROLL: before first timestamp ───────────────────────────
            ;; Line[0] occupies the current-slot position in future-face.
            ;; Layout is structurally identical to the normal path (same blank
            ;; separators and padding) so there is zero shift when cur-idx
            ;; becomes 0 and the normal branch takes over.
            (if (= n 0)
                ;; Empty lines vector — just fill with blanks
                (dotimes (_ emms-lyrics-sync-display-lyrics-height) (insert "\n"))
              (let* ((first-line   (aref lines 0))
                     (first-text   (emms-lyrics-sync-display--line-visual-text
                                    first-line))
                     (first-height (emms-lyrics-sync-display--count-wrapped-lines
                                    first-text))
                     (fut-budget   (max 0 (- n-aft (- first-height 1))))
                     ;; Future lines after line[0], physical-row budget
                     (future-data
                      (let* ((result nil) (phys 0) (budget fut-budget))
                        (cl-loop for i from 1 below n
                                 for line = (aref lines i)
                                 for lphys = (emms-lyrics-sync-display--count-wrapped-lines
                                              (emms-lyrics-sync-display--line-visual-text
                                               line))
                                 while (and (> budget 0) (<= lphys budget))
                                 do (push line result)
                                    (cl-decf budget lphys)
                                    (cl-incf phys lphys))
                        (cons (nreverse result) phys)))
                     (future-lines (car future-data))
                     (future-phys  (cdr future-data))
                     (bot-pad      (max 0 (- fut-budget future-phys))))
                ;; n-bef blank rows (mirrors top-pad of normal path)
                (dotimes (_ n-bef) (insert "\n"))
                ;; Blank separator before current-slot
                (insert "\n")
                ;; Line[0] in future-face — current-slot position
                (emms-lyrics-sync-display--wrap-and-insert
                 first-text 'emms-lyrics-sync-future-line-face)
                ;; Blank separator after current-slot
                (insert "\n")
                ;; Future lines (1..k)
                (dolist (line future-lines)
                  (emms-lyrics-sync-display--wrap-and-insert
                   (emms-lyrics-sync-display--line-visual-text line)
                   'emms-lyrics-sync-future-line-face))
                ;; Bottom padding
                (dotimes (_ bot-pad) (insert "\n"))))

          ;; ── NORMAL: fixed-height context window ────────────────────────────
          (let* ((cur-line    (aref lines cur-idx))
                 (cur-text    (emms-lyrics-sync-display--line-visual-text cur-line))
                 (cur-height  (if (emms-lyrics-sync-line-words cur-line)
                                  1   ; A2: single row (render-a2-line handles it)
                                (emms-lyrics-sync-display--count-wrapped-lines
                                 cur-text)))
                 (fut-budget  (max 0 (- n-aft (- cur-height 1))))

                 ;; Past lines — walk backward, accumulate physical rows.
                 ;; push+downfrom builds list in chronological (forward) order.
                 (past-data
                  (let* ((result nil) (phys 0) (budget n-bef))
                    (cl-loop for i downfrom (1- cur-idx) to 0
                             for line = (aref lines i)
                             for lphys = (emms-lyrics-sync-display--count-wrapped-lines
                                          (emms-lyrics-sync-display--line-visual-text
                                           line))
                             while (and (> budget 0) (<= lphys budget))
                             do (push line result)
                                (cl-decf budget lphys)
                                (cl-incf phys lphys))
                    (cons result phys)))
                 (past-lines  (car past-data))
                 (past-phys   (cdr past-data))
                 (top-pad     (max 0 (- n-bef past-phys)))

                 ;; Future lines — walk forward, accumulate physical rows.
                 ;; push in visit order then nreverse = chronological.
                 (future-data
                  (let* ((result nil) (phys 0) (budget fut-budget))
                    (cl-loop for i from (1+ cur-idx) below n
                             for line = (aref lines i)
                             for lphys = (emms-lyrics-sync-display--count-wrapped-lines
                                          (emms-lyrics-sync-display--line-visual-text
                                           line))
                             while (and (> budget 0) (<= lphys budget))
                             do (push line result)
                                (cl-decf budget lphys)
                                (cl-incf phys lphys))
                    (cons (nreverse result) phys)))
                 (future-lines (car future-data))
                 (future-phys  (cdr future-data))
                 (bot-pad      (max 0 (- fut-budget future-phys))))

            ;; Top padding
            (dotimes (_ top-pad) (insert "\n"))
            ;; Past lines
            (dolist (line past-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-display--line-visual-text line)
               'emms-lyrics-sync-past-line-face))
            ;; Blank before current
            (insert "\n")
            ;; Current line
            (if (emms-lyrics-sync-line-words cur-line)
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              (emms-lyrics-sync-display--wrap-and-insert
               cur-text 'emms-lyrics-sync-current-line-face))
            ;; Blank after current
            (insert "\n")
            ;; Future lines
            (dolist (line future-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-display--line-visual-text line)
               'emms-lyrics-sync-future-line-face))
            ;; Bottom padding
            (dotimes (_ bot-pad) (insert "\n")))))))))

;;; ── Overlay Management ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--ensure-word-overlay ()
  "Return the word-highlight overlay, creating it if needed.
Recreates the overlay when it has been deleted or is associated with a
different buffer (e.g. after the lyrics buffer was killed and recreated)."
  (unless (and (overlayp emms-lyrics-sync-display--word-overlay)
               (eq (overlay-buffer emms-lyrics-sync-display--word-overlay)
                   emms-lyrics-sync-display--buffer))
    ;; Delete stale overlay (if any) before creating a fresh one
    (when (overlayp emms-lyrics-sync-display--word-overlay)
      (delete-overlay emms-lyrics-sync-display--word-overlay))
    (setq emms-lyrics-sync-display--word-overlay
          (make-overlay 1 1 emms-lyrics-sync-display--buffer t nil))
    (overlay-put emms-lyrics-sync-display--word-overlay
                 'face 'emms-lyrics-sync-word-current-face)
    ;; Priority 100: above sung-word overlays (90) and text-property faces
    (overlay-put emms-lyrics-sync-display--word-overlay 'priority 100))
  emms-lyrics-sync-display--word-overlay)

(defun emms-lyrics-sync-display--reset-word-overlay ()
  "Collapse the word-current overlay to position (1,1) so it covers no text.
Called before every lyrics redraw to prevent a stale highlight appearing
at old buffer positions after delete-region + re-insert."
  (when (and (overlayp emms-lyrics-sync-display--word-overlay)
             (overlay-buffer emms-lyrics-sync-display--word-overlay))
    (move-overlay emms-lyrics-sync-display--word-overlay 1 1
                  emms-lyrics-sync-display--buffer)))

(defun emms-lyrics-sync-display--clear-sung-overlays ()
  "Delete all sung-word overlays."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition current-word and sung-word overlays for POS-MS.

For each word in `emms-lyrics-sync-display--word-positions':
  - word.end-ms <= pos-ms  → sung overlay (green, priority 90)
  - word.start-ms <= pos-ms < word.end-ms → cursor overlay (yellow+ul, p 100)
  - pos-ms < word.start-ms → no overlay (future word shows amber base)

When no word matches pos-ms the cursor overlay is collapsed to (1,1)
so no stale highlight remains visible."
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
          ;; Current word: pos-ms falls within this word's time span
          ((and (>= pos-ms w-start) (< pos-ms w-end) (not found-cursor))
           (setq found-cursor t)
           (move-overlay ov buf-start buf-end
                         emms-lyrics-sync-display--buffer))
          ;; Sung word: word has already ended
          ((< w-end pos-ms)
           (let ((sung (make-overlay buf-start buf-end
                                     emms-lyrics-sync-display--buffer)))
             (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
             ;; Priority 90: above text-property faces, below word-current (100)
             (overlay-put sung 'priority 90)
             (push sung emms-lyrics-sync-display--sung-overlays)))))
        ;; No word was current — collapse cursor overlay so it is invisible
        (unless found-cursor
          (emms-lyrics-sync-display--reset-word-overlay))))))

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
            (when (and (integerp start) (integerp end) (<= start end))
              (goto-char start)
              (delete-region start end)
              (insert (propertize new-str
                                  'face 'emms-lyrics-sync-elapsed-face)))))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
