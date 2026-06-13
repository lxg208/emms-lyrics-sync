;;; emms-lyrics-sync-waveform.el --- Waveform extraction and dual-channel SVG rendering  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-12 22:10 UTC
;; Purpose : Per-channel peak amplitude extraction from audio files via ffmpeg
;;           astats filter, and dual-channel SVG waveform rendering in the
;;           foobar2000 waveform-seekbar style:
;;             - Left channel bars grow upward from a horizontal centre line
;;             - Right channel bars grow downward from the centre line
;;             - Played portion coloured differently from remaining portion
;;             - White cursor line at current playback position
;;             - High resolution: one SVG bar per extracted chunk (default 1500)
;;           Falls back to a Unicode sparkline using max(L,R) in terminals.
;;
;;           Key invariant for display.el integration:
;;             `emms-lyrics-sync-waveform-insert' inserts separator \n FIRST,
;;             then sets `emms-lyrics-sync-display--waveform-marker' with
;;             insertion-type NIL, then renders bar content.
;;             NIL insertion-type means the marker stays at the start of bar
;;             content even when new content is inserted at that position.
;;             `update-waveform-cursor' in display.el relies on this so that
;;             delete-region + re-insert replaces exactly the bar region without
;;             drifting into the lyrics text above.
;;
;;           API change vs previous version:
;;             `emms-lyrics-sync-waveform--render-svg' now takes
;;             (data px-w px-h pos-s duration) instead of
;;             (data width-chars pos-s duration).
;;             Callers in display.el and display-render.el must be updated.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Public API:
;;   `emms-lyrics-sync-waveform-insert'     — called by display.el full-redraw
;;   `emms-lyrics-sync-waveform-invalidate' — clear cache entry for a file
;;
;; Internal render helpers (called by display.el update-waveform-cursor):
;;   `emms-lyrics-sync-waveform--render-svg'
;;   `emms-lyrics-sync-waveform--render-unicode'
;;   `emms-lyrics-sync-waveform--use-svg-p'
;;
;; Data format in cache:
;;   file-path → vector of [l-amp r-amp] float vectors, each in [0.0, 1.0],
;;   normalised so that peak(max(L,R) across all chunks) = 1.0.
;;
;; ffmpeg extraction:
;;   aresample=8000, astats reset=160 frames (~20 ms at 8 kHz),
;;   ametadata mode=print to stdout.  Parses lavfi.astats.1.Peak_level (left)
;;   and lavfi.astats.2.Peak_level (right) per chunk.
;;   For mono files only channel 1 is present; channel 2 amp defaults to
;;   channel 1 value so the waveform still renders symmetrically.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)

;;; ── Forward declarations (defined in emms-lyrics-sync-display-vars.el) ───────

(defvar emms-lyrics-sync-display--waveform-marker nil)
(defvar emms-lyrics-sync-display--buffer nil)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-waveform-height 2
  "Waveform height in character lines (SVG uses frame-char-height per line).
Default 2 gives a compact bar matching foobar2000 waveform seekbar proportions.
Increase for a taller display."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-render-mode 'auto
  "Waveform render mode.
`auto'    — SVG in graphical Emacs, Unicode sparkline in terminals.
`unicode' — always Unicode block characters.
`svg'     — always SVG (errors in terminal)."
  :type  '(choice (const auto) (const unicode) (const svg))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-chunks 1500
  "Number of amplitude samples to extract from the audio file.
Higher values give finer temporal resolution.  Extraction time scales
roughly linearly; 1500 chunks is fast enough for real-time use."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-played "#4a9eff"
  "SVG fill colour for the already-played portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-remaining "#888888"
  "SVG fill colour for the not-yet-played portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-cursor "#ffffff"
  "SVG stroke colour for the playback cursor line."
  :type  'color
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface emms-lyrics-sync-waveform-played-face
  '((((background dark))  :foreground "#4a9eff")
    (((background light)) :foreground "#0055cc"))
  "Face for the played portion of the Unicode waveform."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-waveform-remaining-face
  '((((background dark))  :foreground "#888888")
    (((background light)) :foreground "#aaaaaa"))
  "Face for the remaining portion of the Unicode waveform."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-waveform-cursor-face
  '((((background dark))  :foreground "#ffffff" :background "#4a9eff")
    (((background light)) :foreground "#000000" :background "#4a9eff"))
  "Face for the cursor column of the Unicode waveform."
  :group 'emms-lyrics-sync)

;;; ── Cache ────────────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-waveform--cache (make-hash-table :test #'equal)
  "Cache: file-path → vector of [l-amp r-amp] float vectors (nil = pending).")

(defvar emms-lyrics-sync-waveform--pending (make-hash-table :test #'equal)
  "Set of file-paths for which async ffmpeg extraction is running.")

;;; ── ffmpeg Extraction ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--placeholder (n-chunks)
  "Return a placeholder data vector of N-CHUNKS pairs, both channels at 0.5."
  (let ((v (make-vector n-chunks nil)))
    (dotimes (i n-chunks) (aset v i (vector 0.5 0.5)))
    v))

(defun emms-lyrics-sync-waveform--run-ffmpeg-async (file-path n-chunks callback)
  "Extract per-channel peak amplitudes from FILE-PATH asynchronously.
Uses ffmpeg astats with reset=160 frames at 8 kHz resample (~20 ms chunks).
Calls CALLBACK with a normalised vector of [l-amp r-amp] float vectors.
Falls back to a placeholder vector when ffmpeg is absent."
  (if (not (executable-find "ffmpeg"))
      (funcall callback (emms-lyrics-sync-waveform--placeholder n-chunks))
    (let* ((buf  (generate-new-buffer " *emms-waveform-ffmpeg*"))
           (proc (make-process
                  :name    "emms-lyrics-sync-waveform"
                  :buffer  buf
                  :command (list "ffmpeg"
                                 "-i" (expand-file-name file-path)
                                 "-filter:a"
                                 (concat "aresample=8000,"
                                         "astats=metadata=1:reset=160,"
                                         "ametadata=mode=print:file=-")
                                 "-map" "0:a:0"
                                 "-f"   "null" "-")
                  :stderr  buf
                  :noquery t)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (let ((raw (with-current-buffer buf (buffer-string))))
             (kill-buffer buf)
             (let* ((pairs (emms-lyrics-sync-waveform--parse-astats raw))
                    (vec   (if pairs
                               (emms-lyrics-sync-waveform--normalise-pairs
                                pairs n-chunks)
                             (emms-lyrics-sync-waveform--placeholder n-chunks))))
               (funcall callback vec)))))))))

(defun emms-lyrics-sync-waveform--parse-astats (output)
  "Parse per-channel Peak_level values from ffmpeg astats OUTPUT.
Returns a list of [left-amp right-amp] float vectors in linear amplitude.
For mono files (only channel 1), right-amp is set equal to left-amp."
  (let (result left-amp
        (case-fold-search t))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ;; Channel 1 (left) peak
           ((string-match
             "lavfi\\.astats\\.1\\.Peak_level=\\([-0-9.]+\\)" line)
            (let ((db (string-to-number (match-string 1 line))))
              (setq left-amp
                    (if (<= db -100.0) 0.0
                      (min 1.0 (expt 10.0 (/ db 20.0)))))))
           ;; Channel 2 (right) peak — flush a pair
           ((string-match
             "lavfi\\.astats\\.2\\.Peak_level=\\([-0-9.]+\\)" line)
            (when left-amp
              (let ((db (string-to-number (match-string 1 line))))
                (let ((r-amp (if (<= db -100.0) 0.0
                               (min 1.0 (expt 10.0 (/ db 20.0))))))
                  (push (vector left-amp r-amp) result)))
              (setq left-amp nil)))
           ;; Overall stats line — flush mono pair using left for both
           ((and left-amp
                 (string-match "lavfi\\.astats\\.Overall\\.Peak_level=" line))
            (push (vector left-amp left-amp) result)
            (setq left-amp nil))))
        (forward-line 1)))
    ;; Final pending mono chunk
    (when left-amp
      (push (vector left-amp left-amp) result))
    (nreverse result)))

(defun emms-lyrics-sync-waveform--normalise-pairs (pairs n-chunks)
  "Normalise PAIRS list to N-CHUNKS vector of [l r] float vectors in [0,1].
Normalises so that peak(max(L,R) across all chunks) = 1.0.
Resamples (up or down) to exactly N-CHUNKS entries."
  (when pairs
    (let* ((src-n  (length pairs))
           ;; Find global peak across both channels
           (peak   (cl-loop for p in pairs
                            maximize (max (aref p 0) (aref p 1))))
           (scale  (if (> peak 0.0) (/ 1.0 peak) 1.0))
           (result (make-vector n-chunks nil)))
      (dotimes (i n-chunks)
        (let* ((src-f  (* (/ (float i) n-chunks) src-n))
               (src-i  (min (floor src-f) (1- src-n)))
               (frac   (- src-f src-i))
               (src-i2 (min (1+ src-i) (1- src-n)))
               (p1     (nth src-i  pairs))
               (p2     (nth src-i2 pairs))
               (l      (min 1.0 (* scale (+ (* (- 1.0 frac) (aref p1 0))
                                             (* frac          (aref p2 0))))))
               (r      (min 1.0 (* scale (+ (* (- 1.0 frac) (aref p1 1))
                                             (* frac          (aref p2 1)))))))
          (aset result i (vector l r))))
      result)))

;;; ── Cache Population ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--ensure-loaded (file-path n-chunks callback)
  "Ensure waveform data for FILE-PATH is cached; call CALLBACK on completion.
No-op (no callback) when data already cached or extraction already pending.
Starts async extraction otherwise; CALLBACK is called once when it completes."
  (cond
   ((gethash file-path emms-lyrics-sync-waveform--cache) nil)
   ((gethash file-path emms-lyrics-sync-waveform--pending) nil)
   (t
    (puthash file-path t emms-lyrics-sync-waveform--pending)
    (emms-lyrics-sync-waveform--run-ffmpeg-async
     file-path n-chunks
     (lambda (vec)
       (remhash file-path emms-lyrics-sync-waveform--pending)
       (puthash file-path vec emms-lyrics-sync-waveform--cache)
       (funcall callback))))))

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform data for FILE-PATH, forcing re-extraction."
  (remhash file-path emms-lyrics-sync-waveform--cache)
  (remhash file-path emms-lyrics-sync-waveform--pending))

;;; ── Render Mode Detection ────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--use-svg-p ()
  "Return non-nil when SVG rendering is appropriate."
  (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
       (display-graphic-p)
       (image-type-available-p 'svg)
       (progn (ignore-errors (require 'svg nil t)) (fboundp 'svg-create))))

;;; ── Unicode Sparkline Renderer ───────────────────────────────────────────────

(defconst emms-lyrics-sync-waveform--blocks
  [?▁ ?▂ ?▃ ?▄ ?▅ ?▆ ?▇ ?█]
  "Block characters for the Unicode waveform sparkline (U+2581–U+2588).")

(defun emms-lyrics-sync-waveform--render-unicode (data width-chars pos-s duration)
  "Return a propertized Unicode sparkline string for DATA.
DATA is a vector of [l-amp r-amp] float vectors.
WIDTH-CHARS is the character width.  POS-S and DURATION are in seconds."
  (let* ((n       (length data))
         (cursor  (when (and duration (> duration 0))
                    (floor (* (/ pos-s (float duration)) width-chars))))
         (line    (make-string width-chars ?\s)))
    (cl-loop for col from 0 below width-chars do
      (let* ((src-i  (min (1- n) (floor (* (/ (float col) width-chars) n))))
             (pair   (aref data src-i))
             (amp    (max (aref pair 0) (aref pair 1)))
             (ch     (aref emms-lyrics-sync-waveform--blocks
                           (min 7 (floor (* amp 8)))))
             (face   (cond ((and cursor (= col cursor))
                            'emms-lyrics-sync-waveform-cursor-face)
                           ((and cursor (< col cursor))
                            'emms-lyrics-sync-waveform-played-face)
                           (t
                            'emms-lyrics-sync-waveform-remaining-face))))
        (aset line col ch)
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── SVG Dual-Channel Renderer ────────────────────────────────────────────────
;;
;; Design (foobar2000 waveform seekbar style):
;;   - Canvas is px-w × px-h pixels.
;;   - Horizontal centre line at y = px-h/2.
;;   - For each data chunk i, draw two bars from the centre:
;;       Left channel:  upward   from centre,  height = amp-L * (px-h/2)
;;       Right channel: downward from centre,  height = amp-R * (px-h/2)
;;   - Colour: played (< cursor-x) vs remaining (>= cursor-x)
;;   - White vertical cursor line at cursor-x.
;;   - Bar width = px-w / n-chunks (sub-pixel for high chunk counts).
;;     SVG fractional coordinates are used, so all 1500 bars render at
;;     full pixel-level resolution.

(defun emms-lyrics-sync-waveform--render-svg (data px-w px-h pos-s duration)
  "Return an SVG image object for DATA.
DATA is a vector of [l-amp r-amp] pairs.  PX-W × PX-H is pixel dimensions.
POS-S is playback position in seconds, DURATION is track length in seconds."
  (ignore-errors (require 'svg nil t))
  (when (fboundp 'svg-create)
    (let* ((n         (length data))
           (bar-w     (/ (float px-w) n))
           (half-h    (/ px-h 2.0))
           (cursor-x  (when (and duration (> duration 0))
                        (* (/ pos-s (float duration)) px-w)))
           (bg        (or (ignore-errors
                            (face-background 'default nil t))
                          "#2d2d2d"))
           (svg       (svg-create px-w px-h)))
      ;; Background
      (svg-rectangle svg 0 0 px-w px-h :fill bg)
      ;; Centre line (subtle)
      (svg-line svg 0 half-h px-w half-h
                :stroke "#444444" :stroke-width 1)
      ;; Draw bars for every chunk
      (cl-loop for i from 0 below n do
        (let* ((pair   (aref data i))
               (amp-l  (aref pair 0))
               (amp-r  (aref pair 1))
               (x      (* i bar-w))
               (w      (max 0.5 (- bar-w 0.5)))  ; slight gap between bars
               (played (and cursor-x (< x cursor-x)))
               (color  (if played
                           emms-lyrics-sync-waveform-color-played
                         emms-lyrics-sync-waveform-color-remaining))
               ;; Left channel: upward from centre
               (h-l    (max 1.0 (* amp-l half-h)))
               ;; Right channel: downward from centre
               (h-r    (max 1.0 (* amp-r half-h))))
          ;; Left channel bar (upper half)
          (svg-rectangle svg x (- half-h h-l) w h-l :fill color)
          ;; Right channel bar (lower half)
          (svg-rectangle svg x half-h w h-r :fill color)))
      ;; Cursor line (drawn last, on top)
      (when cursor-x
        (svg-line svg cursor-x 0 cursor-x px-h
                  :stroke emms-lyrics-sync-waveform-color-cursor
                  :stroke-width 1.5))
      (svg-image svg :scale 1.0))))

;;; ── Public Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-insert (file-path pos-s duration)
  "Insert the waveform display at point for FILE-PATH.
POS-S is the current playback position in seconds.
DURATION is the track duration in seconds (or nil).

Sets `emms-lyrics-sync-display--waveform-marker' with insertion-type NIL
AFTER inserting the separator newline and BEFORE inserting bar content.
NIL type keeps the marker anchored at the bar start regardless of
subsequent insert-image calls — critical for update-waveform-cursor.

Trigger flow:
  First call  → insert placeholder bar, start async ffmpeg extraction.
  On complete → `full-redraw' is called (same-track path) which calls
                `waveform-insert' again with real cached data.
  Subsequent  → insert real bar immediately from cache."
  (when file-path
    (let* ((char-w   (frame-char-width))
           (char-h   (frame-char-height))
           (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
           (px-w     (* win-chars char-w))
           (px-h     (* emms-lyrics-sync-waveform-height char-h))
           (data     (gethash file-path emms-lyrics-sync-waveform--cache))
           (use-svg  (emms-lyrics-sync-waveform--use-svg-p))
           ;; Use real data when cached, neutral placeholder otherwise
           (render-data (if (vectorp data)
                            data
                          (emms-lyrics-sync-waveform--placeholder win-chars)))
           (buf      (current-buffer)))
      ;; Kick off async extraction (no-op if already cached or pending)
      (unless (vectorp data)
        (emms-lyrics-sync-waveform--ensure-loaded
         file-path emms-lyrics-sync-waveform-chunks
         (lambda ()
           ;; Extraction complete — trigger same-track redraw if still current
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((track emms-lyrics-sync-core--current-track))
                 (when (and track
                            (equal (emms-lyrics-sync-track-file-path track)
                                   file-path)
                            (fboundp 'emms-lyrics-sync-display-on-track-change))
                   (emms-lyrics-sync-display-on-track-change))))))))
      ;; Insert separator newline, then anchor marker, then bar content.
      ;; Marker insertion-type NIL: stays at bar start regardless of inserts.
      (insert "\n")
      (setq emms-lyrics-sync-display--waveform-marker
            (copy-marker (point) nil))
      (if (and use-svg
               (not (eq emms-lyrics-sync-waveform-render-mode 'unicode)))
          (let ((img (emms-lyrics-sync-waveform--render-svg
                      render-data px-w px-h pos-s duration)))
            (when img (insert-image img))
            (insert "\n"))
        ;; Unicode sparkline fallback
        (insert (emms-lyrics-sync-waveform--render-unicode
                 render-data win-chars pos-s duration))
        (insert "\n")))))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
