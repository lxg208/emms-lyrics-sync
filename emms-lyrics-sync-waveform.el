;;; emms-lyrics-sync-waveform.el --- Waveform analysis and rendering  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-12 15:56 UTC
;; Purpose : Waveform loudness extraction (async ffmpeg astats) and rendering
;;           for the emms-lyrics-sync display buffer.  Two render modes:
;;             (1) Unicode sparkline — ▁▂▃▄▅▆▇█ block characters.
;;             (2) SVG image        — GUI Emacs only; sharp, coloured bars.
;;           Waveform data is extracted once per file and cached in memory.
;;
;;           Public entry point: `emms-lyrics-sync-waveform-insert'
;;           Called by emms-lyrics-sync-display.el from full-redraw.
;;
;;           Key invariant (v3 fix):
;;             `emms-lyrics-sync-waveform-insert' inserts the separator \\n,
;;             sets `emms-lyrics-sync-display--waveform-marker' with insertion-
;;             type NIL (so the marker stays before content on all subsequent
;;             inserts), then renders bar content.
;;             Previously the marker had type T which caused it to advance past
;;             the inserted content, making delete-region in
;;             update-waveform-cursor find wf-start=point-max and append a new
;;             bar on every throttled tick instead of replacing the old one.
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
;; Render helpers (called by display-render.el, not by user code):
;;   `emms-lyrics-sync-waveform--render-unicode'
;;   `emms-lyrics-sync-waveform--render-svg'
;;
;; ffmpeg >= 4.0 must be on PATH for real waveform data.
;; Graceful degradation: placeholder bar of 0.5 amplitude shown until
;; extraction completes or if ffmpeg is absent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-waveform-height 4
  "Height of the waveform in lines (Unicode) or char-heights (SVG)."
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
  "Number of amplitude samples to extract from the audio file."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-played "#4a9eff"
  "Colour for the already-played portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-remaining "#555555"
  "Colour for the remaining portion of the waveform."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-cursor "#ffffff"
  "Colour for the playback cursor line."
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
  "Cache: file-path → normalised float vector (nil = not yet extracted).")

(defvar emms-lyrics-sync-waveform--pending (make-hash-table :test #'equal)
  "Set of file-paths for which extraction is currently running.")

;;; ── ffmpeg Extraction ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--run-ffmpeg-async (file-path n-chunks callback)
  "Extract waveform data from FILE-PATH asynchronously via ffmpeg.
Calls CALLBACK with a normalised float vector on success, or a
constant-0.5 placeholder vector on failure or if ffmpeg is absent."
  (if (not (executable-find "ffmpeg"))
      (funcall callback (make-vector n-chunks 0.5))
    (let* ((buf  (generate-new-buffer " *emms-lyrics-sync-waveform*"))
           (proc (make-process
                  :name    "emms-lyrics-sync-waveform"
                  :buffer  buf
                  :command (list "ffmpeg"
                                 "-i" (expand-file-name file-path)
                                 "-filter:a"
                                 "aresample=8000,astats=metadata=1:reset=400,ametadata=mode=print:file=-"
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
             (let* ((vals (emms-lyrics-sync-waveform--parse-astats raw))
                    (vec  (if vals
                              (emms-lyrics-sync-waveform--normalise-trim
                               vals n-chunks)
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
          (unless (<= db -91.0)             ; -91 dB is ffmpeg's -inf sentinel
            (push (expt 10.0 (/ db 20.0)) vals)))))
    (nreverse vals)))

(defun emms-lyrics-sync-waveform--parse-volumedetect (output n-chunks)
  "Parse ffmpeg volumedetect OUTPUT into a constant float vector of N-CHUNKS.
Fallback when astats metadata parsing finds no values."
  (let ((max-vol nil))
    (when (string-match "max_volume: \\([-0-9.]+\\) dB" output)
      (setq max-vol (string-to-number (match-string 1 output))))
    (let ((amp (if max-vol
                   (min 1.0 (max 0.01 (expt 10.0 (/ max-vol 20.0))))
                 0.5)))
      (make-vector n-chunks amp))))

(defun emms-lyrics-sync-waveform--normalise-trim (vals n)
  "Normalise list VALS to [0,1] relative to peak; resample to vector of N."
  (when vals
    (let* ((peak   (apply #'max vals))
           (norm   (if (zerop peak)
                       (mapcar (lambda (_) 0.5) vals)
                     (mapcar (lambda (v) (/ v peak)) vals)))
           (src-n  (length norm))
           (result (make-vector n 0.0)))
      (cl-loop for i from 0 below n do
        (let* ((src-f  (* (/ (float i) n) src-n))
               (src-i  (min (floor src-f) (1- src-n)))
               (src-i1 (min (1+ src-i) (1- src-n)))
               (frac   (- src-f src-i))
               (v      (+ (* (- 1.0 frac) (nth src-i  norm))
                          (* frac          (nth src-i1 norm)))))
          (aset result i (max 0.0 (min 1.0 v)))))
      result)))

;;; ── Cache Population ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--ensure-loaded (file-path n-chunks redraw-fn)
  "Ensure waveform data for FILE-PATH is in cache; call REDRAW-FN on completion.
No-op when data is already cached or extraction is already running."
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
       (funcall redraw-fn))))))

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform data for FILE-PATH, forcing re-extraction."
  (remhash file-path emms-lyrics-sync-waveform--cache)
  (remhash file-path emms-lyrics-sync-waveform--pending))

;;; ── Unicode Sparkline Renderer ───────────────────────────────────────────────

(defconst emms-lyrics-sync-waveform--blocks
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
  (let* ((n      (length data))
         (cursor (when (and duration (> duration 0))
                   (floor (* (/ pos-s duration) width))))
         (line   (make-string width ?\s)))
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
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── SVG Renderer ─────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--render-svg (data width-chars pos-s duration)
  "Return an SVG image object for the waveform, or nil if SVG is unavailable.
DATA is a float vector, WIDTH-CHARS is the desired width in characters."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (ignore-errors (require 'svg nil t) t)
             (fboundp 'svg-create))
    (let* ((char-w   (frame-char-width))
           (char-h   (frame-char-height))
           (px-w     (* width-chars char-w))
           (px-h     (* emms-lyrics-sync-waveform-height char-h))
           (n        (length data))
           (col-w    (/ (float px-w) width-chars))
           (cursor-x (when (and duration (> duration 0))
                       (* (/ pos-s duration) px-w)))
           (svg      (svg-create px-w px-h)))
      (svg-rectangle svg 0 0 px-w px-h
                     :fill (face-background 'default nil t))
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
      (when cursor-x
        (svg-line svg (round cursor-x) 0 (round cursor-x) px-h
                  :stroke emms-lyrics-sync-waveform-color-cursor
                  :stroke-width 2))
      (svg-image svg :scale 1.0))))

;;; ── Public Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-insert (file-path pos-s duration)
  "Insert a waveform display at point for FILE-PATH.
POS-S is the current playback position in seconds.
DURATION is total track duration in seconds or nil.

Inserts: \\n (separator) + waveform bar + \\n (trailing).
Sets `emms-lyrics-sync-display--waveform-marker' with insertion-type NIL
after the separator, so the marker stays before bar content regardless of
subsequent inserts.  This allows `update-waveform-cursor' to reliably
delete just the bar region via (delete-region wf-start point-max).

On first call for a file, renders a placeholder and starts async extraction.
On subsequent calls, renders the real cached data."
  (when file-path
    (let* ((width       (max 4 (- (emms-lyrics-sync-display--body-width) 2)))
           (use-svg     (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
                             (display-graphic-p)
                             (image-type-available-p 'svg)
                             (ignore-errors (require 'svg nil t) t)
                             (fboundp 'svg-create)))
           (data        (gethash file-path emms-lyrics-sync-waveform--cache))
           (buf         (current-buffer)))
      ;; Start async extraction on first call; callback triggers full-redraw.
      ;; Guard: only redraw if the same file is still current.
      (unless data
        (emms-lyrics-sync-waveform--ensure-loaded
         file-path emms-lyrics-sync-waveform-chunks
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((track emms-lyrics-sync-core--current-track))
                 (when (and track
                            (equal (emms-lyrics-sync-track-file-path track)
                                   file-path))
                   (emms-lyrics-sync-display--full-redraw))))))))
      ;; ── Render ──────────────────────────────────────────────────────────
      ;; Layout: [sep \n][←waveform-marker→][bar content][\n]
      ;; The separator \n is NOT part of the replaceable region; it is
      ;; inserted here and never deleted by update-waveform-cursor.
      ;; waveform-marker has insertion-type nil so it stays at the start
      ;; of [bar content] even after bar content is inserted at its position.
      (let ((render-data (or data (make-vector width 0.5))))
        (insert "\n")
        (setq emms-lyrics-sync-display--waveform-marker
              (copy-marker (point) nil))          ; nil = don't advance on insert
        (if (and use-svg
                 (not (eq emms-lyrics-sync-waveform-render-mode 'unicode)))
            (let ((img (emms-lyrics-sync-waveform--render-svg
                        render-data width pos-s duration)))
              (when img (insert-image img))
              (insert "\n"))
          (insert (emms-lyrics-sync-waveform--render-unicode
                   render-data width pos-s duration))
          (insert "\n"))))))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
