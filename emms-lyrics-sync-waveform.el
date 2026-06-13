;;; emms-lyrics-sync-waveform.el --- Waveform PNG generation and display  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-13 16:00 UTC
;; Purpose : Pixel-perfect waveform display using ffmpeg showwavespic filter.
;;
;;           Approach (replaces the old astats+SVG pipeline entirely):
;;             1. ffmpeg showwavespic decodes ALL PCM samples and renders
;;                a pixel-perfect waveform PNG at the exact window pixel size.
;;             2. Two PNGs are generated per track+size combination:
;;                  played.png    — waveform in played colour (blue #4a9eff)
;;                  remaining.png — waveform in remaining colour (grey #888888)
;;             3. At render time the two PNGs are cropped and inserted
;;                side-by-side at the cursor position using :crop image prop.
;;             4. PNGs cached to disk keyed by md5(file-path)+WxH.
;;
;;           IMPORTANT BUG FIX (listp→consp):
;;             In Elisp, (listp nil) → t because nil IS the empty list.
;;             The cache stores nil for "not cached", 'pending for "in
;;             progress", and a plist for "ready".
;;             Guard MUST be (consp cached) not (listp cached):
;;               (listp nil)      → t  ← WRONG: nil passes the guard!
;;               (consp nil)      → nil ← correct: nil is excluded
;;               (consp '(:k v))  → t  ← correct: plist passes
;;             Everywhere the cache value is tested, use consp.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)

;;; ── Forward declarations ─────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display--waveform-marker nil)
(defvar emms-lyrics-sync-display--buffer nil)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-waveform-height 2
  "Waveform height in character lines."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-render-mode 'auto
  "Waveform render mode: `auto', `unicode', or `png'."
  :type  '(choice (const auto) (const unicode) (const png))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-played "#4a9eff"
  "Waveform fill colour for the already-played portion."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-remaining "#888888"
  "Waveform fill colour for the not-yet-played portion."
  :type  'color
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-color-cursor "#ffffff"
  "Colour for the 2-pixel playback cursor line."
  :type  'color
  :group 'emms-lyrics-sync)

;;; ── Terminal fallback faces ──────────────────────────────────────────────────

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
;;
;; Values stored per file-path key:
;;   nil       — not yet requested (cache miss)
;;   'pending  — async ffmpeg processes running
;;   plist     — (:px-w W :px-h H :played PATH :remaining PATH) — ready
;;
;; CRITICAL: guards must use (consp val) NOT (listp val).
;;   (listp nil) → t — nil would pass a listp guard and then
;;   (plist-get nil :px-w) → nil → (= nil px-w) → wrong-type-argument crash.
;;   (consp nil) → nil — nil correctly excluded.

(defvar emms-lyrics-sync-waveform--png-cache (make-hash-table :test #'equal)
  "Cache: file-path → nil | 'pending | plist(:px-w :px-h :played :remaining).")

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--use-png-p ()
  "Return non-nil when PNG rendering is appropriate."
  (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
       (display-graphic-p)
       (executable-find "ffmpeg")))

;; Alias for callers that still use the old name.
(defalias 'emms-lyrics-sync-waveform--use-svg-p
  'emms-lyrics-sync-waveform--use-png-p)

(defun emms-lyrics-sync-waveform--get-bg-color ()
  "Return the current frame background colour as a hex string."
  (or (and (display-graphic-p)
           (ignore-errors (face-background 'default nil t)))
      "#2d2d2d"))

(defun emms-lyrics-sync-waveform--png-paths (file-path px-w px-h)
  "Return (played-path . remaining-path) for FILE-PATH at PX-W×PX-H."
  (let* ((dir  (expand-file-name "waveforms" emms-lyrics-sync-cache-dir))
         (base (concat (md5 (expand-file-name file-path))
                       (format "-%dx%d" px-w px-h))))
    (cons (expand-file-name (concat base "-played.png")    dir)
          (expand-file-name (concat base "-remaining.png") dir))))

;;; ── PNG Generation ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--generate-pngs-async
    (file-path px-w px-h bg-color callback)
  "Generate played and remaining PNG files for FILE-PATH asynchronously.
CALLBACK is called with (played-path remaining-path); either is nil on failure."
  (let* ((paths          (emms-lyrics-sync-waveform--png-paths file-path px-w px-h))
         (played-path    (car paths))
         (remaining-path (cdr paths))
         (done  0)
         (ok-p  nil)
         (ok-r  nil))
    (make-directory (file-name-directory played-path) t)
    (cl-flet
        ((file-ok (path)
           (and (file-exists-p path)
                (> (or (file-attribute-size (file-attributes path)) 0) 100)))
         (ffmpeg-cmd (out-path wave-color)
           (list "ffmpeg" "-y"
                 "-i" (expand-file-name file-path)
                 "-filter_complex"
                 (format (concat "color=c=%s:s=%dx%d[bg];"
                                 "[0:a]showwavespic=s=%dx%d"
                                 ":split_channels=1"
                                 ":colors=%s|%s[wf];"
                                 "[bg][wf]overlay")
                         bg-color px-w px-h
                         px-w px-h
                         wave-color wave-color)
                 "-frames:v" "1" out-path)))
      ;; ── Remaining (grey) ─────────────────────────────────────────────────
      (make-process
       :name    "emms-waveform-remaining"
       :buffer  nil
       :command (ffmpeg-cmd remaining-path
                            emms-lyrics-sync-waveform-color-remaining)
       :noquery t
       :sentinel
       (lambda (p _e)
         (when (memq (process-status p) '(exit signal))
           (setq ok-r (and (= 0 (process-exit-status p))
                           (file-ok remaining-path)))
           (cl-incf done)
           (when (= done 2)
             (funcall callback
                      (and ok-p played-path)
                      (and ok-r remaining-path))))))
      ;; ── Played (blue) ────────────────────────────────────────────────────
      (make-process
       :name    "emms-waveform-played"
       :buffer  nil
       :command (ffmpeg-cmd played-path
                            emms-lyrics-sync-waveform-color-played)
       :noquery t
       :sentinel
       (lambda (p _e)
         (when (memq (process-status p) '(exit signal))
           (setq ok-p (and (= 0 (process-exit-status p))
                           (file-ok played-path)))
           (cl-incf done)
           (when (= done 2)
             (funcall callback
                      (and ok-p played-path)
                      (and ok-r remaining-path)))))))))

;;; ── Placeholder (loading state) ─────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-placeholder (px-w px-h bg-color)
  "Insert a thin horizontal hairline SVG while PNGs are being generated.
Uses amplitude 0.0 so no bars are visible — just a faint centre line."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (ignore-errors (require 'svg nil t))
             (fboundp 'svg-create))
    (let* ((svg    (svg-create px-w px-h))
           (half-h (/ px-h 2.0)))
      (svg-rectangle svg 0 0 px-w px-h :fill bg-color)
      (svg-line svg 0 half-h px-w half-h
                :stroke "#444444" :stroke-width 1)
      (let ((img (svg-image svg :scale 1.0)))
        (when img (insert-image img))))))

;;; ── Composite Render (PNGs ready) ────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-composite
    (played-path remaining-path px-w px-h pos-s duration)
  "Insert the played+cursor+remaining composite from cached PNG files.
Uses Emacs :crop image property — no ffmpeg needed per tick."
  (let* ((cursor-x  (if (and (numberp duration) (> duration 0))
                        (min (1- px-w)
                             (max 0 (round (* (/ (float pos-s)
                                                 (float duration))
                                              px-w))))
                      0))
         (cursor-w  2)
         (after-x   (min px-w (+ cursor-x cursor-w)))
         (after-w   (max 0 (- px-w after-x))))
    ;; ── Played portion (left of cursor) ──────────────────────────────────
    (when (> cursor-x 0)
      (let ((img (create-image played-path nil nil
                               :crop    (list 0 0 cursor-x px-h)
                               :scale   1.0)))
        (when img (insert-image img))))
    ;; ── Cursor line (2px wide SVG) ────────────────────────────────────────
    (when (and (display-graphic-p)
               (image-type-available-p 'svg)
               (ignore-errors (require 'svg nil t))
               (fboundp 'svg-create))
      (let* ((svg (svg-create cursor-w px-h)))
        (svg-rectangle svg 0 0 cursor-w px-h
                       :fill emms-lyrics-sync-waveform-color-cursor)
        (let ((img (svg-image svg :scale 1.0)))
          (when img (insert-image img)))))
    ;; ── Remaining portion (right of cursor) ──────────────────────────────
    (when (> after-w 0)
      (let ((img (create-image remaining-path nil nil
                               :crop  (list after-x 0 after-w px-h)
                               :scale 1.0)))
        (when img (insert-image img))))))

;;; ── Insert at Point ──────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-at-point
    (file-path pos-s duration px-w px-h)
  "Insert waveform display for FILE-PATH at point.
Caller must already be inside the correct buffer with inhibit-read-only t.
Sets `emms-lyrics-sync-display--waveform-marker' with insertion-type NIL.

Cache guard uses CONSP not LISTP:
  (consp nil)        → nil  — cache miss correctly skips composite
  (consp 'pending)   → nil  — in-progress correctly skips composite
  (consp '(:px-w …)) → t   — ready plist correctly enters composite"
  (let* ((bg-color (emms-lyrics-sync-waveform--get-bg-color))
         ;; CRITICAL: use consp not listp — (listp nil) → t in Elisp!
         (cached   (gethash file-path emms-lyrics-sync-waveform--png-cache))
         (ready-p  (consp cached))    ; non-empty plist = ready
         (buf      (current-buffer)))
    ;; ── Separator + marker ───────────────────────────────────────────────
    (insert "\n")
    (setq emms-lyrics-sync-display--waveform-marker
          (copy-marker (point) nil))
    ;; ── Render ───────────────────────────────────────────────────────────
    (cond
     ;; PNGs cached AND dimensions match current window
     ((and ready-p
           (= (plist-get cached :px-w) px-w)
           (= (plist-get cached :px-h) px-h)
           (file-exists-p (plist-get cached :played))
           (file-exists-p (plist-get cached :remaining)))
      (emms-lyrics-sync-waveform--insert-composite
       (plist-get cached :played)
       (plist-get cached :remaining)
       px-w px-h pos-s duration))
     ;; PNGs cached but wrong size (window resized) — invalidate + regenerate
     (ready-p
      (remhash file-path emms-lyrics-sync-waveform--png-cache)
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h bg-color)
      (puthash file-path 'pending emms-lyrics-sync-waveform--png-cache)
      (emms-lyrics-sync-waveform--start-generation
       file-path px-w px-h bg-color buf))
     ;; Already generating — show placeholder
     ((eq cached 'pending)
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h bg-color))
     ;; Not started — show placeholder and kick off generation
     (t
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h bg-color)
      (puthash file-path 'pending emms-lyrics-sync-waveform--png-cache)
      (emms-lyrics-sync-waveform--start-generation
       file-path px-w px-h bg-color buf)))
    (insert "\n")))

(defun emms-lyrics-sync-waveform--start-generation
    (file-path px-w px-h bg-color buf)
  "Start async PNG generation for FILE-PATH; refresh display when done."
  (emms-lyrics-sync-waveform--generate-pngs-async
   file-path px-w px-h bg-color
   (lambda (played remaining)
     (if (and played remaining)
         (puthash file-path
                  (list :px-w px-w :px-h px-h
                        :played played :remaining remaining)
                  emms-lyrics-sync-waveform--png-cache)
       ;; Generation failed — remove pending so next track-change retries
       (remhash file-path emms-lyrics-sync-waveform--png-cache))
     ;; Trigger same-track redraw if still on this track
     (when (buffer-live-p buf)
       (let ((track emms-lyrics-sync-core--current-track))
         (when (and track
                    (equal (emms-lyrics-sync-track-file-path track)
                           file-path)
                    (fboundp 'emms-lyrics-sync-display-on-track-change))
           (emms-lyrics-sync-display-on-track-change)))))))

;;; ── Unicode Flat Bar (terminal fallback) ─────────────────────────────────────

(defun emms-lyrics-sync-waveform--render-unicode-flat
    (width-chars pos-s duration)
  "Return a propertized Unicode flat bar string (no amplitude data needed).
Used when PNG rendering is unavailable (terminal or no ffmpeg)."
  (let* ((cursor  (when (and (numberp duration) (> duration 0))
                    (min (1- width-chars)
                         (max 0 (floor (* (/ (float pos-s)
                                             (float duration))
                                          width-chars))))))
         (line    (make-string width-chars ?▄)))
    (cl-loop for col from 0 below width-chars do
      (let ((face (cond ((and cursor (= col cursor))
                         'emms-lyrics-sync-waveform-cursor-face)
                        ((and cursor (< col cursor))
                         'emms-lyrics-sync-waveform-played-face)
                        (t
                         'emms-lyrics-sync-waveform-remaining-face))))
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── Public API ───────────────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-insert (file-path pos-s duration)
  "Insert waveform display at point for FILE-PATH.
Public entry point — called by display-redraw.el full-redraw path.
Delegates to `--insert-at-point' (PNG) or `--render-unicode-flat' (terminal)."
  (when file-path
    (let* ((char-w    (frame-char-width))
           (char-h    (frame-char-height))
           (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
           (px-w      (* win-chars char-w))
           (px-h      (* emms-lyrics-sync-waveform-height char-h)))
      (cond
       ((emms-lyrics-sync-waveform--use-png-p)
        (emms-lyrics-sync-waveform--insert-at-point
         file-path pos-s duration px-w px-h))
       (t
        ;; Terminal fallback — insert \n + marker + bar ourselves
        (insert "\n")
        (setq emms-lyrics-sync-display--waveform-marker
              (copy-marker (point) nil))
        (insert (emms-lyrics-sync-waveform--render-unicode-flat
                 win-chars pos-s duration))
        (insert "\n"))))))

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform PNGs for FILE-PATH, forcing re-generation."
  (remhash file-path emms-lyrics-sync-waveform--png-cache))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
