;;; emms-lyrics-sync-waveform.el --- Waveform analysis and rendering  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-11 22:50 UTC
;; Purpose : Renders a waveform loudness bar at the bottom of the lyrics buffer.
;;           Two render modes:
;;             (1) Unicode sparkline — works in terminals and GUI;
;;                 uses block characters ▁▂▃▄▅▆▇█ to draw the waveform.
;;             (2) SVG image — GUI Emacs only; sharp, proportional, coloured.
;;           Waveform data is extracted asynchronously via ffmpeg
;;           `astats' or `volumedetect' filters; extraction result is cached
;;           per file path so it runs only once per file.
;;
;;           Diagnostic purpose: the waveform exposes loudness-war mastering
;;           (flat, clipped waveform at maximum height) at a glance, mirroring
;;           the waveform minibar in foobar2000.  The playback progress cursor
;;           moves with the 100 ms display timer tick.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Public API:
;;   `emms-lyrics-sync-waveform-insert'    — called by emms-lyrics-sync-display.el
;;   `emms-lyrics-sync-waveform-invalidate' — clear cache entry for a file
;;
;; ffmpeg (>=4.0) must be on PATH for waveform data extraction.
;; The function degrades gracefully when ffmpeg is absent:
;;   - If ffmpeg is missing, a placeholder bar of medium height is shown.
;;   - If SVG is unavailable (terminal), falls back to Unicode sparkline.
;;
;; Waveform data format:
;;   A vector of N floats in [0.0, 1.0] representing per-chunk RMS loudness
;;   normalised to the peak loudness.  N defaults to the display width so
;;   each element maps to exactly one character / SVG column.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-waveform-height 4
  "Height of the waveform in lines (Unicode mode) or pixels / char-height (SVG)."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-render-mode 'auto
  "Waveform render mode.
  `auto'    — SVG in graphical Emacs, Unicode sparkline in terminals.
  `unicode' — always use Unicode block characters.
  `svg'     — always SVG (errors in terminal)."
  :type  '(choice (const auto) (const unicode) (const svg))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-chunks 200
  "Number of amplitude samples to extract from the audio file.
Higher values give finer resolution; extraction takes proportionally longer."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-played "#4a9eff"
  "SVG / face colour for the already-played portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-remaining "#555555"
  "SVG / face colour for the remaining portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-cursor "#ffffff"
  "SVG / face colour for the playback cursor line."
  :type  'color
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface emms-lyrics-sync-waveform-played-face
  '((((background dark))  :foreground "#4a9eff")
    (((background light)) :foreground "#0055cc"))
  "Face for the played portion of the Unicode waveform."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-waveform-remaining-face
  '((((background dark))  :foreground "#555555")
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
  "Cache: file-path → waveform float vector (nil = extraction pending).")

(defvar emms-lyrics-sync-waveform--pending (make-hash-table :test #'equal)
  "Set of file-paths for which extraction is currently running.")

;;; ── ffmpeg Extraction ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--ffmpeg-chunks-cmd (file n-chunks)
  "Return a list of ffmpeg arguments to extract N-CHUNKS RMS amplitudes.
Uses the `astats' filter with reset per segment.  Output is parsed from
stderr where ffmpeg writes filter metadata."
  (list "ffmpeg"
        "-i" (expand-file-name file)
        "-filter:a"
        (format "aresample=8000,asetnsamples=n=%d:p=0,astats=metadata=1:reset=1"
                ;; samples per chunk — 8000 Hz / n-chunks * duration hack:
                ;; we over-request by using a fixed window of 8000 samples
                ;; (1 second at 8 kHz) for simplicity; duration is trimmed
                ;; post-hoc to exactly n-chunks by normalising the vector.
                8000)
        "-map" "0:a:0"
        "-f"   "null" "-"))

(defun emms-lyrics-sync-waveform--parse-volumedetect (output n-chunks)
  "Parse ffmpeg `volumedetect' OUTPUT into a normalised float vector of N-CHUNKS.
volumedetect is used as a simpler fallback when astats metadata parsing fails.
It only gives global stats, so we produce a constant vector — better than nothing."
  (let ((max-vol nil))
    (when (string-match "max_volume: \\([-0-9.]+\\) dB" output)
      (setq max-vol (string-to-number (match-string 1 output))))
    ;; Normalise: max_volume 0 dB → amplitude 1.0; −60 dB → amplitude ~0.0
    (let ((amp (if max-vol
                   (min 1.0 (max 0.01 (expt 10.0 (/ max-vol 20.0))))
                 0.5)))
      (make-vector n-chunks amp))))

(defun emms-lyrics-sync-waveform--run-ffmpeg-async (file-path n-chunks callback)
  "Extract waveform data from FILE-PATH asynchronously via ffmpeg.
Calls CALLBACK with a normalised float vector on success, or a
constant-0.5 placeholder vector on failure."
  (if (not (executable-find "ffmpeg"))
      (funcall callback (make-vector n-chunks 0.5))
    (let* ((buf  (generate-new-buffer " *emms-lyrics-sync-waveform*"))
           (proc (make-process
                  :name     "emms-lyrics-sync-waveform"
                  :buffer   buf
                  :command  (list "ffmpeg"
                                  "-i" (expand-file-name file-path)
                                  "-filter:a"
                                  (format "aresample=8000,asegment=duration=%.4f,astats=metadata=1:reset=1"
                                          ;; Aim for n-chunks segments; ffmpeg
                                          ;; uses duration per segment.
                                          ;; We estimate from a 4-minute default.
                                          ;; Actual duration unknown here — a
                                          ;; post-hoc trim adjusts the count.
                                          (/ 240.0 (float n-chunks)))
                                  "-map" "0:a:0" "-f" "null" "-")
                  :stderr   buf
                  :noquery  t)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (let ((raw (with-current-buffer buf (buffer-string))))
             (kill-buffer buf)
             ;; Parse RMS_level values from astats metadata lines
             (let* ((vals  (emms-lyrics-sync-waveform--parse-astats raw))
                    (vec   (if vals
                               (emms-lyrics-sync-waveform--normalise-trim vals n-chunks)
                             (emms-lyrics-sync-waveform--parse-volumedetect
                              raw n-chunks))))
               (funcall callback vec)))))))))

(defun emms-lyrics-sync-waveform--parse-astats (output)
  "Extract a list of RMS_level floats (linear) from ffmpeg astats OUTPUT.
Returns nil if no matching lines are found."
  (let (vals)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward
              "lavfi\\.astats\\.Overall\\.RMS_level=\\([-0-9.]+\\)" nil t)
        (let ((db (string-to-number (match-string 1))))
          ;; −91 dB is ffmpeg's -inf sentinel
          (unless (<= db -91.0)
            (push (expt 10.0 (/ db 20.0)) vals)))))
    (nreverse vals)))

(defun emms-lyrics-sync-waveform--normalise-trim (vals n)
  "Normalise list VALS to [0,1] relative to peak and return as a vector of N.
If (length vals) > N, downsample by averaging; if < N, upsample by repeating."
  (when vals
    (let* ((peak (apply #'max vals))
           (norm (if (zerop peak)
                     (mapcar (lambda (_) 0.5) vals)
                   (mapcar (lambda (v) (/ v peak)) vals)))
           ;; Resample to exactly N
           (src-n (length norm))
           (result (make-vector n 0.0)))
      (cl-loop for i from 0 below n do
        (let* ((src-f   (* (/ (float i) n) src-n))
               (src-i   (min (floor src-f) (1- src-n)))
               (src-i1  (min (1+ src-i) (1- src-n)))
               (frac    (- src-f src-i))
               (v       (+ (* (1- frac) (nth src-i  norm))
                           (* frac      (nth src-i1 norm)))))
          (aset result i (max 0.0 (min 1.0 v)))))
      result)))

;;; ── Cache Population ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--ensure-loaded (file-path n-chunks redraw-fn)
  "Ensure waveform data for FILE-PATH is in cache; call REDRAW-FN on completion.
If data is already cached, does nothing.  If extraction is pending, does nothing
(the sentinel will call REDRAW-FN).  Otherwise starts async extraction."
  (cond
   ((gethash file-path emms-lyrics-sync-waveform--cache)
    nil)
   ((gethash file-path emms-lyrics-sync-waveform--pending)
    nil)
   (t
    (puthash file-path t emms-lyrics-sync-waveform--pending)
    (emms-lyrics-sync-waveform--run-ffmpeg-async
     file-path n-chunks
     (lambda (vec)
       (remhash file-path emms-lyrics-sync-waveform--pending)
       (puthash file-path vec emms-lyrics-sync-waveform--cache)
       (funcall redraw-fn))))))

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform data for FILE-PATH, forcing re-extraction."
  (remhash file-path emms-lyrics-sync-waveform--cache)
  (remhash file-path emms-lyrics-sync-waveform--pending))

;;; ── Unicode Sparkline Renderer ───────────────────────────────────────────────

(defconst emms-lyrics-sync-waveform--blocks
  ;; 8 levels: ▁▂▃▄▅▆▇█  (U+2581 … U+2588)
  [?▁ ?▂ ?▃ ?▄ ?▅ ?▆ ?▇ ?█]
  "Block characters used for the Unicode waveform sparkline.")

(defun emms-lyrics-sync-waveform--amp-to-char (amp)
  "Map amplitude AMP (0.0–1.0) to a Unicode block character."
  (aref emms-lyrics-sync-waveform--blocks
        (min 7 (floor (* amp 8)))))

(defun emms-lyrics-sync-waveform--render-unicode (data width pos-s duration)
  "Return a propertized Unicode sparkline string.
DATA is a float vector, WIDTH is the character width, POS-S and DURATION
are playback position and track length in seconds."
  (let* ((n       (length data))
         ;; Cursor column index
         (cursor  (when (and duration (> duration 0))
                    (floor (* (/ pos-s duration) width))))
         (line    (make-string width ?\s)))
    (cl-loop for col from 0 below width do
      (let* ((src-idx (min (1- n) (floor (* (/ (float col) width) n))))
             (amp     (aref data src-idx))
             (ch      (emms-lyrics-sync-waveform--amp-to-char amp))
             (face    (cond ((and cursor (= col cursor))
                             'emms-lyrics-sync-waveform-cursor-face)
                            ((and cursor (< col cursor))
                             'emms-lyrics-sync-waveform-played-face)
                            (t
                             'emms-lyrics-sync-waveform-remaining-face))))
        (aset line col ch)
        ;; Apply face via text properties on the produced string
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── SVG Renderer ─────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--render-svg (data width-chars pos-s duration)
  "Return an SVG image object for the waveform.
DATA is a float vector, WIDTH-CHARS is the desired width in characters."
  (require 'svg)
  (let* ((char-w     (frame-char-width))
         (char-h     (frame-char-height))
         (px-w       (* width-chars char-w))
         (px-h       (* emms-lyrics-sync-waveform-height char-h))
         (n          (length data))
         (col-w      (/ (float px-w) width-chars))
         (cursor-x   (when (and duration (> duration 0))
                       (* (/ pos-s duration) px-w)))
         (svg        (svg-create px-w px-h)))
    ;; Background
    (svg-rectangle svg 0 0 px-w px-h
                   :fill (face-background 'default nil t))
    ;; Waveform columns
    (cl-loop for col from 0 below width-chars do
      (let* ((src-i  (min (1- n) (floor (* (/ (float col) width-chars) n))))
             (amp    (aref data src-i))
             (bar-h  (max 1 (round (* amp px-h))))
             (x      (round (* col col-w)))
             (y      (- px-h bar-h))
             (played (and cursor-x (< (* col col-w) cursor-x)))
             (color  (if played
                         emms-lyrics-sync-waveform-color-played
                       emms-lyrics-sync-waveform-color-remaining)))
        (svg-rectangle svg x y (max 1 (round (- col-w 1))) bar-h
                       :fill color)))
    ;; Cursor line
    (when cursor-x
      (svg-line svg (round cursor-x) 0 (round cursor-x) px-h
                :stroke emms-lyrics-sync-waveform-color-cursor
                :stroke-width 2))
    (svg-image svg :scale 1.0)))

;;; ── Public Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-insert (file-path pos-s duration)
  "Insert a waveform display at point for FILE-PATH.
POS-S is the current playback position in seconds (float).
DURATION is total track duration in seconds (float or nil).

On first call for a file, inserts a placeholder and starts async extraction;
the display buffer is refreshed automatically when data arrives.

On subsequent calls (data already cached), inserts the real waveform."
  (when file-path
    (let* ((width   (- (emms-lyrics-sync-display--body-width) 2))
           (use-svg (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
                         (display-graphic-p)
                         (fboundp 'svg-create)))
           (data    (gethash file-path emms-lyrics-sync-waveform--cache))
           (buf     (current-buffer)))
      ;; Trigger async extraction if not yet cached
      (unless data
        (emms-lyrics-sync-waveform--ensure-loaded
         file-path emms-lyrics-sync-waveform-chunks
         (lambda ()
           ;; When extraction finishes, trigger a full redraw if this
           ;; buffer is still displaying the same file.
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((track emms-lyrics-sync-core--current-track))
                 (when (and track
                            (string= (emms-lyrics-sync-track-file-path track)
                                     file-path))
                   (emms-lyrics-sync-display--full-redraw))))))))
      ;; Render with whatever data we have (real or placeholder)
      (let ((render-data (or data (make-vector width 0.5))))
        (if (and use-svg (not (eq emms-lyrics-sync-waveform-render-mode 'unicode)))
            (let ((img (emms-lyrics-sync-waveform--render-svg
                        render-data width pos-s duration)))
              (insert-image img)
              (insert "\n"))
          ;; Unicode sparkline
          (insert (emms-lyrics-sync-waveform--render-unicode
                   render-data width pos-s duration))
          (insert "\n"))))))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
