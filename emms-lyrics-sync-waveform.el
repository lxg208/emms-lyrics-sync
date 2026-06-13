;;; emms-lyrics-sync-waveform.el --- Waveform PNG generation and display  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-13 16:00 UTC
;; Updated : 2026-06-13 18:40 UTC
;; Purpose : Pixel-perfect waveform using ffmpeg showwavespic filter.
;;
;;           Pipeline (replaces astats+SVG entirely):
;;             1. ffmpeg showwavespic → played.png + remaining.png (async)
;;             2. ffmpeg crop+hstack  → composite.png (sync, ~20ms, image only)
;;             3. clear-image-cache + create-image → display single PNG
;;
;;           Why not Emacs :crop:
;;             Two insert-image calls have pixel gaps between them.
;;             Emacs caches images by path — overwriting the file returns
;;             the stale cached image unless clear-image-cache is called.
;;             A single composite PNG avoids both problems.
;;
;;           Why not the old astats approach:
;;             astats at 8 kHz with reset=160 gives ~19 chunks/sec.
;;             For a 295s track that is only 5,658 data points.
;;             showwavespic decodes all PCM samples — same as foobar2000
;;             foo_wave_minibar_mod which is "fully software implemented"
;;             using a GDI pixel buffer at full sample rate.
;;
;;           CRITICAL — consp vs listp:
;;             Cache stores nil (miss) | 'pending | plist (ready).
;;             (listp nil) → t — nil passes a listp guard → crash.
;;             (consp nil) → nil — correct.
;;             Every cache guard uses consp.
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
(declare-function emms-lyrics-sync-display--window    "emms-lyrics-sync-display-vars")
(declare-function emms-lyrics-sync-display--body-width "emms-lyrics-sync-display-vars")

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
;; Values per file-path key:
;;   nil       — not yet requested
;;   'pending  — async ffmpeg running
;;   plist     — (:px-w W :px-h H :played PATH :remaining PATH) — ready
;;
;; CRITICAL: every guard uses (consp val) NOT (listp val).
;;   (listp nil) → t  — nil passes → (plist-get nil :px-w) → nil → crash.
;;   (consp nil) → nil — nil correctly excluded.

(defvar emms-lyrics-sync-waveform--png-cache (make-hash-table :test #'equal)
  "Cache: file-path → nil | 'pending | plist(:px-w :px-h :played :remaining).")

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--use-png-p ()
  "Return non-nil when PNG rendering is appropriate."
  (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
       (display-graphic-p)
       (executable-find "ffmpeg")))

(defalias 'emms-lyrics-sync-waveform--use-svg-p
  'emms-lyrics-sync-waveform--use-png-p)

(defun emms-lyrics-sync-waveform--get-bg-color ()
  "Return current frame background colour as a hex string."
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

(defun emms-lyrics-sync-waveform--composite-path (played-path)
  "Derive composite PNG path from PLAYED-PATH."
  (replace-regexp-in-string "-played\\.png\\'" "-composite.png" played-path))

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
      (cl-flet
          ((on-done ()
             (cl-incf done)
             (when (= done 2)
               (funcall callback
                        (and ok-p (file-ok played-path) played-path)
                        (and ok-r (file-ok remaining-path) remaining-path)))))
        ;; ── Remaining PNG ─────────────────────────────────────────────────
        (if (file-ok remaining-path)
            (progn (setq ok-r t) (on-done))
          (let ((buf (generate-new-buffer " *emms-wf-remaining*")))
            (make-process
             :name    "emms-wf-remaining"
             :buffer  buf
             :command (ffmpeg-cmd remaining-path
                                  emms-lyrics-sync-waveform-color-remaining)
             :noquery t
             :sentinel
             (lambda (p _)
               (when (memq (process-status p) '(exit signal))
                 (kill-buffer buf)
                 (setq ok-r (file-ok remaining-path))
                 (on-done))))))
        ;; ── Played PNG ────────────────────────────────────────────────────
        (if (file-ok played-path)
            (progn (setq ok-p t) (on-done))
          (let ((buf (generate-new-buffer " *emms-wf-played*")))
            (make-process
             :name    "emms-wf-played"
             :buffer  buf
             :command (ffmpeg-cmd played-path
                                  emms-lyrics-sync-waveform-color-played)
             :noquery t
             :sentinel
             (lambda (p _)
               (when (memq (process-status p) '(exit signal))
                 (kill-buffer buf)
                 (setq ok-p (file-ok played-path))
                 (on-done))))))))))

;;; ── Composite PNG ────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--make-composite-png
    (played-path remaining-path out-path px-w px-h cursor-x)
  "Create a single composite PNG synchronously via ffmpeg crop+hstack.
Left slice (0..cursor-x) from PLAYED-PATH (blue).
Right slice (cursor-x..px-w) from REMAINING-PATH (grey).
Returns t on success, nil on failure.

Why ffmpeg crop+hstack instead of Emacs :crop:
  1. Two insert-image calls leave a pixel gap at the join.
  2. Emacs caches images by path — overwriting the file returns the
     stale cached image even with a new create-image call unless
     clear-image-cache is called first.
  A single composite file avoids both issues."
  (let ((cursor-x (max 0 (min px-w cursor-x))))
    (cond
     ;; Nothing played yet — just copy remaining
     ((= cursor-x 0)
      (ignore-errors (copy-file remaining-path out-path t))
      (file-exists-p out-path))
     ;; Fully played — just copy played
     ((>= cursor-x px-w)
      (ignore-errors (copy-file played-path out-path t))
      (file-exists-p out-path))
     ;; Composite: crop left from played, right from remaining, hstack
     (t
      (let* ((remaining-w (- px-w cursor-x))
             (rc (call-process
                  "ffmpeg" nil nil nil
                  "-y"
                  "-i" played-path
                  "-i" remaining-path
                  "-filter_complex"
                  (format (concat "[0:v]crop=%d:%d:0:0[L];"
                                  "[1:v]crop=%d:%d:%d:0[R];"
                                  "[L][R]hstack=inputs=2[out]")
                          cursor-x      px-h
                          remaining-w   px-h   cursor-x)
                  "-map" "[out]"
                  out-path)))
        (and (= rc 0) (file-exists-p out-path)))))))

(defun emms-lyrics-sync-waveform--insert-composite
    (played-path remaining-path px-w px-h pos-s duration)
  "Create composite PNG and insert as a single image at point.
No newlines are inserted — the caller must add the trailing \\n.

Calls clear-image-cache before create-image to prevent Emacs from
returning a stale cached copy of the composite file."
  (let* ((cursor-x  (if (and duration (> duration 0))
                        (max 0 (min px-w
                                    (round (* (/ (float pos-s) duration) px-w))))
                      0))
         (comp-path (emms-lyrics-sync-waveform--composite-path played-path))
         (ok        (emms-lyrics-sync-waveform--make-composite-png
                     played-path remaining-path comp-path
                     px-w px-h cursor-x)))
    (when ok
      ;; Flush Emacs image cache for this path so overwritten file is reloaded.
      (ignore-errors (clear-image-cache comp-path))
      (let ((img (create-image comp-path nil nil :scale 1.0)))
        (when img
          (insert-image img))))))

;;; ── Placeholder (hairline while PNGs generate) ───────────────────────────────

(defun emms-lyrics-sync-waveform--insert-placeholder (px-w px-h)
  "Insert a hairline SVG placeholder at point.  No newlines inserted.
Shows a subtle centre line so the waveform area is not invisible."
  (ignore-errors (require 'svg nil t))
  (when (fboundp 'svg-create)
    (let* ((bg  (emms-lyrics-sync-waveform--get-bg-color))
           (svg (svg-create px-w px-h)))
      (svg-rectangle svg 0 0 px-w px-h :fill bg)
      (svg-line svg 0 (/ (float px-h) 2) px-w (/ (float px-h) 2)
                :stroke "#444444" :stroke-width 1)
      (let ((img (svg-image svg :scale 1.0)))
        (when img
          (insert-image img))))))

;;; ── Unicode Flat Bar (terminal fallback) ─────────────────────────────────────

(defun emms-lyrics-sync-waveform--render-unicode-flat (width-chars pos-s duration)
  "Return a propertized Unicode flat progress bar string for terminal use.
WIDTH-CHARS is the character width.  POS-S and DURATION are in seconds."
  (let* ((cursor (when (and duration (> duration 0))
                   (floor (* (/ pos-s (float duration)) width-chars))))
         (line   (make-string width-chars ?▄)))
    (cl-loop for col from 0 below width-chars do
      (let ((face (cond ((and cursor (= col cursor))
                         'emms-lyrics-sync-waveform-cursor-face)
                        ((and cursor (< col cursor))
                         'emms-lyrics-sync-waveform-played-face)
                        (t
                         'emms-lyrics-sync-waveform-remaining-face))))
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── Async Trigger ────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--trigger-redraw (buf file-path)
  "Trigger same-track redraw if BUF is live and FILE-PATH is still current."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((track emms-lyrics-sync-core--current-track))
        (when (and track
                   (equal (emms-lyrics-sync-track-file-path track) file-path)
                   (fboundp 'emms-lyrics-sync-display-on-track-change))
          (emms-lyrics-sync-display-on-track-change))))))

;;; ── Main Entry Point ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-at-point
    (file-path pos-s duration px-w px-h)
  "Insert waveform for FILE-PATH at point.

Invariant (relied upon by display-redraw.el):
  1. Insert separator \\n
  2. Set emms-lyrics-sync-display--waveform-marker (NIL insertion-type)
  3. Insert content (composite PNG or placeholder)
  4. Insert trailing \\n

Never call this from --update-waveform-cursor (would produce double \\n
because the separator \\n before the marker is preserved by delete-region)."
  (insert "\n")
  (setq emms-lyrics-sync-display--waveform-marker
        (copy-marker (point) nil))
  (let* ((cached (gethash file-path emms-lyrics-sync-waveform--png-cache))
         (buf    (current-buffer)))
    (cond
     ;; ── PNGs ready and dimensions match ────────────────────────────────
     ((and (consp cached)
           (= (plist-get cached :px-w) px-w)
           (= (plist-get cached :px-h) px-h)
           (file-exists-p (plist-get cached :played))
           (file-exists-p (plist-get cached :remaining)))
      (emms-lyrics-sync-waveform--insert-composite
       (plist-get cached :played)
       (plist-get cached :remaining)
       px-w px-h pos-s duration))

     ;; ── PNGs exist but window was resized ──────────────────────────────
     ((and (consp cached)
           (or (/= (plist-get cached :px-w) px-w)
               (/= (plist-get cached :px-h) px-h)))
      ;; Show placeholder, invalidate, regenerate for new size.
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h)
      (remhash file-path emms-lyrics-sync-waveform--png-cache)
      (puthash file-path 'pending emms-lyrics-sync-waveform--png-cache)
      (emms-lyrics-sync-waveform--generate-pngs-async
       file-path px-w px-h
       (emms-lyrics-sync-waveform--get-bg-color)
       (lambda (p r)
         (when (and p r)
           (puthash file-path
                    (list :px-w px-w :px-h px-h :played p :remaining r)
                    emms-lyrics-sync-waveform--png-cache))
         (emms-lyrics-sync-waveform--trigger-redraw buf file-path))))

     ;; ── Already generating ─────────────────────────────────────────────
     ((eq cached 'pending)
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h))

     ;; ── Not started — begin async generation ───────────────────────────
     (t
      (emms-lyrics-sync-waveform--insert-placeholder px-w px-h)
      (puthash file-path 'pending emms-lyrics-sync-waveform--png-cache)
      (emms-lyrics-sync-waveform--generate-pngs-async
       file-path px-w px-h
       (emms-lyrics-sync-waveform--get-bg-color)
       (lambda (p r)
         (when (and p r)
           (puthash file-path
                    (list :px-w px-w :px-h px-h :played p :remaining r)
                    emms-lyrics-sync-waveform--png-cache))
         (emms-lyrics-sync-waveform--trigger-redraw buf file-path))))))
  (insert "\n"))

;;; ── Public API ───────────────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform PNGs for FILE-PATH, forcing re-extraction."
  (remhash file-path emms-lyrics-sync-waveform--png-cache))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
