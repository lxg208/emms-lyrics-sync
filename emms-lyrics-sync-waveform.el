;;; emms-lyrics-sync-waveform.el --- Waveform PNG generation and display  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-waveform.el
;; Created : 2026-06-13 16:00 UTC
;; Updated : 2026-06-14 00:00 UTC
;; Changes :
;;   • --get-bg-color: auto-compensates +2/channel for ffmpeg YUV rounding
;;     (overlay filter converts through YUV internally, shifting bg by -2)
;;   • All other code identical to v10 balanced
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

(defcustom emms-lyrics-sync-waveform-height 4
  "Waveform height in character lines.
Default 4 gives ~80px at a typical font size, matching foobar2000
waveform seekbar proportions.  The pixel height is always:
  emms-lyrics-sync-waveform-height × frame-char-height
Increase for a taller display; decrease for a compact bar."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-render-mode 'auto
  "Waveform render mode: `auto', `unicode', or `png'."
  :type  '(choice (const auto) (const unicode) (const png))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-background-color nil
  "Background colour for waveform PNGs, as a hex string e.g. \"#292a2d\".
When nil (the default), the colour is read automatically from the
`default' face background at the time of PNG generation, with an
automatic +2-per-channel compensation for ffmpeg's YUV rounding.

Set this explicitly when the auto-detected colour is wrong:
  (setq emms-lyrics-sync-waveform-background-color \"#28343a\")

Changing this value invalidates all cached PNGs automatically."
  :type  '(choice (const :tag "Auto-detect from default face" nil)
                  (color :tag "Explicit hex color"))
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
  "Return the waveform background colour as a hex string.

Auto-compensates +2 per channel for ffmpeg's YUV rounding:
ffmpeg's overlay filter converts colour sources through YUV internally,
which shifts every channel value by -2.  Adding +2 here ensures the
rendered PNG background matches the actual frame background exactly.

Resolution order:
  1. `emms-lyrics-sync-waveform-background-color' when explicitly set.
  2. `face-background' of the `default' face (auto-detect from theme).
  3. Hard-coded fallback \"#2d2d2d\"."
  (let* ((raw (or emms-lyrics-sync-waveform-background-color
                  (and (display-graphic-p)
                       (ignore-errors (face-background 'default nil t)))
                  "#2d2d2d")))
    (if (string-match
         "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$"
         raw)
        (format "#%02x%02x%02x"
                (min 255 (+ (string-to-number (match-string 1 raw) 16) 2))
                (min 255 (+ (string-to-number (match-string 2 raw) 16) 2))
                (min 255 (+ (string-to-number (match-string 3 raw) 16) 2)))
      raw)))

(defun emms-lyrics-sync-waveform--png-paths (file-path px-w px-h)
  "Return (played-path . remaining-path) for FILE-PATH at PX-W×PX-H.
The background colour is included in the filename so that changing
`emms-lyrics-sync-waveform-background-color' automatically invalidates
old cached PNGs without a manual cache flush."
  (let* ((dir   (expand-file-name "waveforms" emms-lyrics-sync-cache-dir))
         (bg    (emms-lyrics-sync-waveform--get-bg-color))
         (bg-s  (replace-regexp-in-string "#" "" bg))
         (base  (concat (md5 (expand-file-name file-path))
                        (format "-%dx%d-%s" px-w px-h bg-s))))
    (cons (expand-file-name (concat base "-played.png")    dir)
          (expand-file-name (concat base "-remaining.png") dir))))

(defun emms-lyrics-sync-waveform--composite-path (played-path)
  "Derive composite PNG path from PLAYED-PATH."
  (replace-regexp-in-string "-played\\.png\\'" "-composite.png" played-path))

;;; ── Filter Complex Builder ───────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--filter-complex (px-w px-h bg-color wave-color)
  "Build an ffmpeg filter_complex for foobar-style dual-channel waveform.

Foobar2000 foo_wave_minibar_mod style: L channel grows UPWARD from a
shared centre line, R channel grows DOWNWARD from the same centre line.

Algorithm:
  1. Upmix to stereo (handles mono files transparently).
  2. Channelsplit → separate L and R audio streams.
  3. Render each channel at FULL px-h (showwavespic centres the waveform
     in the full canvas, so peaks extend both above and below centre).
  4. Composite each over a background plate.
  5. Crop TOP half of L    → only upward peaks remain.
  6. Crop BOTTOM half of R → only downward peaks remain.
  7. vstack the two halves → one shared centre line, zero gap by
     construction regardless of signal amplitude."
  (let* ((half-h (/ px-h 2)))
    (format (concat
             "[0:a]aformat=channel_layouts=stereo[ac];"
             "[ac]channelsplit=channel_layout=stereo[L][R];"
             "color=c=%s:s=%dx%d[bgL];"
             "color=c=%s:s=%dx%d[bgR];"
             "[L]showwavespic=s=%dx%d:split_channels=0:colors=%s[wfL_raw];"
             "[R]showwavespic=s=%dx%d:split_channels=0:colors=%s[wfR_raw];"
             "[bgL][wfL_raw]overlay[wfL];"
             "[bgR][wfR_raw]overlay[wfR];"
             "[wfL]crop=%d:%d:0:0[topL];"
             "[wfR]crop=%d:%d:0:%d[botR];"
             "[topL][botR]vstack")
            ;; bgL
            bg-color px-w px-h
            ;; bgR
            bg-color px-w px-h
            ;; wfL_raw
            px-w px-h wave-color
            ;; wfR_raw
            px-w px-h wave-color
            ;; crop topL: w h x y
            px-w half-h
            ;; crop botR: w h x y(=half-h)
            px-w half-h half-h)))

;;; ── PNG Generation ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--generate-png (file-path out-path px-w px-h
                                                  wave-color callback)
  "Generate a waveform PNG for FILE-PATH at PX-W×PX-H asynchronously.
WAVE-COLOR is the waveform bar colour (hex string).
Calls CALLBACK with t on success, nil on failure."
  (let* ((bg  (emms-lyrics-sync-waveform--get-bg-color))
         (fc  (emms-lyrics-sync-waveform--filter-complex
               px-w px-h bg wave-color))
         (dir (file-name-directory out-path)))
    (make-directory dir t)
    (let ((proc (make-process
                 :name    "emms-lyrics-sync-waveform-gen"
                 :buffer  nil
                 :command (list "ffmpeg" "-y"
                                "-i" (expand-file-name file-path)
                                "-filter_complex" fc
                                "-frames:v" "1"
                                out-path)
                 :noquery t
                 :sentinel
                 (lambda (p _event)
                   (when (memq (process-status p) '(exit signal))
                     (let ((ok (and (= (process-exit-status p) 0)
                                    (file-exists-p out-path)
                                    (> (or (file-attribute-size
                                            (file-attributes out-path)) 0)
                                       100))))
                       (funcall callback ok)))))))
      proc)))

;;; ── PNG Cache Population ─────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--ensure-pngs (file-path px-w px-h callback)
  "Ensure both played and remaining PNGs exist for FILE-PATH at PX-W×PX-H.
Calls CALLBACK with the cache plist when both are ready, nil on failure.
No-op if already cached or pending."
  (let ((cached (gethash file-path emms-lyrics-sync-waveform--png-cache)))
    (cond
     ;; Already fully cached at the right dimensions
     ((and (consp cached)
           (= (plist-get cached :px-w) px-w)
           (= (plist-get cached :px-h) px-h)
           (file-exists-p (plist-get cached :played))
           (file-exists-p (plist-get cached :remaining)))
      (funcall callback cached))
     ;; Already pending — callback will fire when generation completes
     ((eq cached 'pending) nil)
     ;; Need to generate
     (t
      (puthash file-path 'pending emms-lyrics-sync-waveform--png-cache)
      (let* ((paths          (emms-lyrics-sync-waveform--png-paths
                              file-path px-w px-h))
             (played-path    (car paths))
             (remaining-path (cdr paths))
             (done-count     0)
             (ok-count       0)
             (finish
              (lambda (ok)
                (cl-incf done-count)
                (when ok (cl-incf ok-count))
                (when (= done-count 2)
                  (if (= ok-count 2)
                      (let ((plist (list :px-w      px-w
                                         :px-h      px-h
                                         :played    played-path
                                         :remaining remaining-path)))
                        (puthash file-path plist
                                 emms-lyrics-sync-waveform--png-cache)
                        (funcall callback plist))
                    (puthash file-path nil
                             emms-lyrics-sync-waveform--png-cache)
                    (funcall callback nil))))))
        (emms-lyrics-sync-waveform--generate-png
         file-path played-path px-w px-h
         emms-lyrics-sync-waveform-color-played finish)
        (emms-lyrics-sync-waveform--generate-png
         file-path remaining-path px-w px-h
         emms-lyrics-sync-waveform-color-remaining finish))))))

;;; ── Composite Insertion ──────────────────────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-composite (played-path remaining-path
                                                      px-w px-h pos-s duration)
  "Insert a composite waveform image at point.
Combines PLAYED-PATH and REMAINING-PATH via ffmpeg crop+hstack into a
single composite PNG, then inserts it.  No Emacs :crop used."
  (let* ((cursor-x     (if (and duration (> duration 0))
                           (max 1 (min (1- px-w)
                                       (round (* (/ pos-s (float duration))
                                                 px-w))))
                         0))
         (remaining-w  (- px-w cursor-x))
         (composite    (emms-lyrics-sync-waveform--composite-path played-path))
         (fc           (format
                        "[0:v]crop=%d:%d:0:0[left];[1:v]crop=%d:%d:%d:0[right];[left][right]hstack=inputs=2"
                        cursor-x px-h
                        remaining-w px-h cursor-x))
         (result       (call-process
                        "ffmpeg" nil nil nil
                        "-y"
                        "-i" played-path
                        "-i" remaining-path
                        "-filter_complex" fc
                        "-frames:v" "1"
                        composite)))
    (when (and (= result 0)
               (file-exists-p composite)
               (> (or (file-attribute-size (file-attributes composite)) 0) 100))
      (clear-image-cache composite)
      (let ((img (create-image composite nil nil :scale 1.0)))
        (when img
          (insert-image img))))))

;;; ── Unicode Flat Bar (terminal fallback) ─────────────────────────────────────

(defun emms-lyrics-sync-waveform--render-unicode-flat (width-chars pos-s duration)
  "Return a propertized flat Unicode bar string (terminal fallback)."
  (let* ((cursor (when (and duration (> duration 0))
                   (floor (* (/ pos-s (float duration)) width-chars))))
         (line   (make-string width-chars ?─)))
    (cl-loop for col from 0 below width-chars do
      (let ((face (cond
                   ((and cursor (= col cursor))
                    'emms-lyrics-sync-waveform-cursor-face)
                   ((and cursor (< col cursor))
                    'emms-lyrics-sync-waveform-played-face)
                   (t
                    'emms-lyrics-sync-waveform-remaining-face))))
        (put-text-property col (1+ col) 'face face line)))
    line))

;;; ── Insert At Point (called by redraw.el) ────────────────────────────────────

(defun emms-lyrics-sync-waveform--insert-at-point (file-path pos-s duration
                                                     px-w px-h)
  "Insert waveform for FILE-PATH at point.
Sets `emms-lyrics-sync-display--waveform-marker' (NIL type) after
inserting the separator newline and before inserting bar content.

If PNGs are already cached: inserts composite immediately.
If not cached: inserts unicode flat bar placeholder, starts async
PNG generation, then triggers a same-track redraw on completion."
  (let* ((cached  (gethash file-path emms-lyrics-sync-waveform--png-cache))
         (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
         (buf     (current-buffer)))
    ;; Separator newline BEFORE marker
    (insert "\n")
    (setq emms-lyrics-sync-display--waveform-marker
          (copy-marker (point) nil))
    (cond
     ;; ── PNG ready ────────────────────────────────────────────────────────
     ((and (consp cached)
           (= (plist-get cached :px-w) px-w)
           (= (plist-get cached :px-h) px-h)
           (file-exists-p (plist-get cached :played))
           (file-exists-p (plist-get cached :remaining)))
      (emms-lyrics-sync-waveform--insert-composite
       (plist-get cached :played)
       (plist-get cached :remaining)
       px-w px-h pos-s duration)
      (insert "\n"))

     ;; ── PNG pending or wrong dimensions — show flat bar, start/wait ───
     (t
      (insert (emms-lyrics-sync-waveform--render-unicode-flat
               win-chars pos-s duration))
      (insert "\n")
      ;; Start generation only if not already pending
      (unless (eq cached 'pending)
        (emms-lyrics-sync-waveform--ensure-pngs
         file-path px-w px-h
         (lambda (plist)
           (when (and plist (buffer-live-p buf))
             (with-current-buffer buf
               (when (and emms-lyrics-sync-core--current-track
                          (equal (emms-lyrics-sync-track-file-path
                                  emms-lyrics-sync-core--current-track)
                                 file-path)
                          (fboundp 'emms-lyrics-sync-display-on-track-change))
                 (emms-lyrics-sync-display-on-track-change)))))))))))

;;; ── Public: Invalidate Cache ────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-waveform-invalidate (file-path)
  "Remove cached waveform PNGs for FILE-PATH, forcing re-generation."
  (remhash file-path emms-lyrics-sync-waveform--png-cache))

(provide 'emms-lyrics-sync-waveform)
;;; emms-lyrics-sync-waveform.el ends here
