;;; emms-lyrics-sync-display-anim.el --- No-lyrics breathing-bar animation  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-anim.el
;; Created : 2026-10-28
;; Updated : 2026-10-28  v3 — stop/pause, enabled flag, CPU throttle
;;   • No text labels in SVG — all info already in metadata window above
;;   • anim-pause / anim-resume — freeze on pause, resume on unpause
;;   • defcustom anim-enabled — set nil to show plain "(no lyrics)" instead
;;   • defcustom anim-tick-divisor — throttle SVG regen (default 3 = ~3 fps)
;;   • tick-update respects divisor and paused flag
;;   • render-initial returns t on success, nil when disabled/fallback
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)

;;; ── User Options ─────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-display-anim-enabled t
  "When non-nil (the default), show the breathing-bar animation for tracks
with no lyrics.  When nil, fall back to the plain \"(no lyrics)\" text.

  (setq emms-lyrics-sync-display-anim-enabled nil)"
  :type  'boolean
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-anim-background 'emacs-bg
  "Background color of the no-lyrics animation SVG image.

\\='emacs-bg  — match the Emacs \\='default\\=' face background exactly.
              The animation blends seamlessly into the buffer.  Recommended.

\\='derived   — \\='default\\=' face background lightened by 8 per channel.
              Gives a faint \\='panel\\=' effect while staying close to theme.

A hex string — fixed explicit color, e.g. \"#263238\"."
  :type '(choice (const :tag "Match Emacs background (recommended)" emacs-bg)
                 (const :tag "Derived (slightly lighter than bg)" derived)
                 (color :tag "Explicit hex color"))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-anim-ffprobe-delay 1.5
  "Seconds to wait after track start before running Phase-2 ffprobe analysis.

The delay lets the lyrics fetch pipeline finish first: if lyrics arrive,
the animation stops and the ffprobe callback is discarded.

For testing Phase-1 only (hash-seeded bars):
  (setq emms-lyrics-sync-display-anim-ffprobe-delay 15)"
  :type  'number
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-anim-tick-divisor 3
  "Regenerate the SVG animation image only every Nth 100 ms tick.

The 100 ms display timer calls `anim-tick-update' on every tick, but
SVG generation + librsvg parsing is the CPU hot path, especially over a
remote X11 connection (S195/Kronos).  This divisor throttles the update
rate while keeping the breathing frequency accurate (tick counter still
advances every 100 ms).

  1 — 10 fps  (full rate; most fluid; highest CPU)
  2 — 5 fps
  3 — ~3 fps  (default; comfortable on remote X11)
  5 — 2 fps   (very low; bars move visibly in steps)"
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── Constants ────────────────────────────────────────────────────────────────

(defconst emms-lyrics-sync-display-anim--n-bars 8
  "Number of frequency bars in the animation.")

;; No label color constant — all text has been removed from the SVG.
;; Artist and title are already shown in the metadata window above.

;;; ── State ────────────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display-anim--active nil
  "Non-nil when the no-lyrics animation is currently running.")

(defvar emms-lyrics-sync-display-anim--paused nil
  "Non-nil when the animation is frozen due to playback pause.
The overlay is preserved so the last frame stays visible; tick-update
is skipped while this is non-nil.")

(defvar emms-lyrics-sync-display-anim--tick 0
  "Tick counter incremented 10x/s; drives the breathing phase.")

(defvar emms-lyrics-sync-display-anim--base nil
  "Vector of N-BARS base heights (0.20-0.85) seeded from SHA-256(artist|title).")

(defvar emms-lyrics-sync-display-anim--phase nil
  "Vector of N-BARS phase offsets (radians) seeded from SHA-256(artist|title).")

(defvar emms-lyrics-sync-display-anim--ffprobe-heights nil
  "Vector of N-BARS frequency heights from Phase-2 ffprobe, or nil.")

(defvar emms-lyrics-sync-display-anim--overlay nil
  "Overlay whose `display' property holds the SVG image.
Updating the overlay never touches buffer content.")

(defvar emms-lyrics-sync-display-anim--ffprobe-timer nil
  "One-shot timer that launches Phase-2 ffprobe after track start.")

(defvar emms-lyrics-sync-display-anim--token 0
  "Generation counter.  Incremented on every track change to invalidate
stale async ffprobe callbacks from previous tracks.")

(defvar emms-lyrics-sync-display-anim--bar-color "#4DD0E1"
  "Bar color chosen deterministically from the track hash at seed time.
Drawn from `emms-lyrics-sync-display--placeholder-colors' when bound;
falls back to Material Cyan 300 (#4DD0E1).")

;;; ── Background color helper ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--get-bg-color ()
  "Return the hex background color for the animation SVG."
  (let* ((face-bg (and (display-graphic-p)
                       (ignore-errors (face-background 'default nil t))))
         (base    (or face-bg "#263238")))
    (pcase emms-lyrics-sync-display-anim-background
      ('emacs-bg base)
      ('derived
       (if (string-match
            "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$"
            base)
           (format "#%02x%02x%02x"
                   (min 255 (+ (string-to-number (match-string 1 base) 16) 8))
                   (min 255 (+ (string-to-number (match-string 2 base) 16) 8))
                   (min 255 (+ (string-to-number (match-string 3 base) 16) 8)))
         base))
      ((pred stringp) emms-lyrics-sync-display-anim-background)
      (_ base))))

;;; ── Trivial-doc detection ────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--trivial-doc-p (doc)
  "Return non-nil when DOC represents trivially empty or instrumental lyrics.

Triggers animation instead of real lyrics for:
  - nil doc
  - 1-2 lines where every line is blank or contains a known
    'pure music' / 'no lyrics' marker (Chinese or English)."
  (or (null doc)
      (let* ((lines (emms-lyrics-sync-lrc-doc-lines doc))
             (n     (if (vectorp lines) (length lines) 0)))
        (and (<= n 2)
             (cl-loop
              for i below n
              for line = (aref lines i)
              for text = (string-trim
                          (or (emms-lyrics-sync-line-text line) ""))
              always
              (or (string-empty-p text)
                  (string-match-p
                   (concat "纯音乐\\|此歌曲为没有填词\\|歌曲暂无歌词"
                           "\\|instrumental\\|no lyrics\\|pure music")
                   text)))))))

;;; ── Hash-seeded initialisation ───────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--seed (track)
  "Seed animation state from SHA-256(artist|title) of TRACK.
Resets tick, Phase-2 heights, picks bar color, and increments the
generation token."
  (let* ((artist (or (and track (emms-lyrics-sync-track-artist track)) ""))
         (title  (or (and track (emms-lyrics-sync-track-title  track)) ""))
         (hash   (secure-hash 'sha256 (concat artist "|" title)))
         (byte   (lambda (n)
                   (/ (float
                       (string-to-number
                        (substring hash (* n 2) (+ (* n 2) 2)) 16))
                      255.0)))
         (n emms-lyrics-sync-display-anim--n-bars))
    (setq emms-lyrics-sync-display-anim--base
          (vconcat
           (cl-loop for i below n
                    collect (+ 0.20 (* 0.65 (funcall byte i)))))
          emms-lyrics-sync-display-anim--phase
          (vconcat
           (cl-loop for i below n
                    collect (* 2.0 float-pi (funcall byte (+ n i)))))
          emms-lyrics-sync-display-anim--tick           0
          emms-lyrics-sync-display-anim--ffprobe-heights nil)
    ;; Pick bar color from placeholder-colors using hash byte 16
    (let* ((colors (or (and (boundp 'emms-lyrics-sync-display--placeholder-colors)
                            emms-lyrics-sync-display--placeholder-colors)
                       '("#4DD0E1")))
           (cidx   (mod (string-to-number (substring hash 32 34) 16)
                        (length colors))))
      (setq emms-lyrics-sync-display-anim--bar-color (nth cidx colors)))
    (cl-incf emms-lyrics-sync-display-anim--token)))

;;; ── Bar height computation ───────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--heights ()
  "Return a vector of N-BARS current display heights (0.0-1.0).

Uses Phase-2 ffprobe heights as the base when available; otherwise
uses the Phase-1 hash-seeded base heights.

Breathing envelope:
  h_i = base_i * (0.40 + 0.60 * sin(2pi * 0.3 * t + phi_i))

0.3 Hz wave (full cycle ~3.3 s), phase offsets give wave-like motion."
  (let* ((bases (or emms-lyrics-sync-display-anim--ffprobe-heights
                    emms-lyrics-sync-display-anim--base))
         (t-s   (* emms-lyrics-sync-display-anim--tick 0.1))
         (omega (* 2.0 float-pi 0.3))
         (n     emms-lyrics-sync-display-anim--n-bars))
    (vconcat
     (cl-loop for i below n
              for b = (aref bases i)
              for p = (aref emms-lyrics-sync-display-anim--phase i)
              collect (max 0.04
                           (min 1.0
                                (* b (+ 0.40
                                        (* 0.60
                                           (sin (+ (* omega t-s) p)))))))))))

;;; ── SVG helpers ──────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--xml-escape (s)
  "Return S with XML special characters replaced by entity references."
  (when s
    (dolist (pair '(("&" . "&amp;") ("<" . "&lt;")
                    (">" . "&gt;") ("\"" . "&quot;")))
      (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
    s))

(defun emms-lyrics-sync-display-anim--lavfi-quote (path)
  "Escape PATH for safe embedding in an ffprobe/ffmpeg lavfi filter spec."
  (let ((s path))
    (setq s (replace-regexp-in-string "\\\\" "\\\\\\\\" s t t))
    (setq s (replace-regexp-in-string "'"    "\\\\'"   s t t))
    (setq s (replace-regexp-in-string ":"    "\\\\:"   s t t))
    (setq s (replace-regexp-in-string ","    "\\\\,"   s t t))
    s))

(defun emms-lyrics-sync-display-anim--make-svg (heights px-w px-h)
  "Generate and return an SVG string for HEIGHTS at PX-W x PX-H pixels.

No text labels — artist and title are already shown in the metadata
window above; adding them here would duplicate information and cause
truncation for long classical music names.

Bars fill the full height with small top/bottom margins only."
  (let* ((n         (length heights))
         ;; Geometry — external overlay padding handles left visual margin
         (mx        (round (* px-w 0.025)))
         (my        (round (* px-h 0.08)))   ; bottom margin
         (mt        (round (* px-h 0.06)))   ; top margin
         (bar-area  (- px-h my mt))
         (usable-w  (- px-w (* 2 mx)))
         (cell-w    (/ (float usable-w) n))
         (bar-w     (max 4 (round (* cell-w 0.60))))
         ;; Colors
         (bg        (emms-lyrics-sync-display-anim--get-bg-color))
         (barc      emms-lyrics-sync-display-anim--bar-color)
         ;; Build bar SVG strings
         (bars
          (apply #'concat
                 (cl-loop
                  for i below n
                  for h      = (aref heights i)
                  for bar-px = (max 3 (round (* bar-area h)))
                  for cx     = (round (+ mx (* i cell-w) (/ cell-w 2.0)))
                  for x      = (- cx (/ bar-w 2))
                  for yb     = (- px-h my)
                  for yt     = (- yb bar-px)
                  for alpha  = (+ 0.55 (* 0.45 h))
                  for rx     = (round (/ bar-w 3.5))
                  for refl-h = (max 2 (round (* bar-px 0.12)))
                  collect
                  (format
                   (concat
                    "<rect x='%d' y='%d' width='%d' height='%d' rx='%d'"
                    " fill='%s' fill-opacity='%.2f'/>"
                    "<rect x='%d' y='%d' width='%d' height='%d' rx='%d'"
                    " fill='%s' fill-opacity='%.2f'/>")
                   x yt bar-w bar-px rx barc alpha
                   x yb bar-w refl-h rx barc (* alpha 0.22))))))
    (format
     (concat
      "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>"
      "<rect width='100%%' height='100%%' fill='%s'/>"
      ;; Baseline hairline
      "<rect x='%d' y='%d' width='%d' height='1' fill='%s' fill-opacity='0.22'/>"
      "%s"
      "</svg>")
     px-w px-h bg
     mx (- px-h my) usable-w barc
     bars)))

;;; ── Overlay management ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--kill-overlay ()
  "Delete the animation overlay if it is still alive."
  (when (and (overlayp emms-lyrics-sync-display-anim--overlay)
             (overlay-buffer emms-lyrics-sync-display-anim--overlay))
    (delete-overlay emms-lyrics-sync-display-anim--overlay))
  (setq emms-lyrics-sync-display-anim--overlay nil))

;;; ── Image-size helper ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-anim--px-size ()
  "Return (px-w . px-h) sized to the lyrics window body minus side padding.

Subtracts 2 x `emms-lyrics-sync-waveform-side-padding' from the column
count so the animation image has the same horizontal margins as the
waveform PNG below it.  The overlay's before-string provides the left
indent; the right side is implicit."
  (let* ((char-w  (frame-char-width))
         (char-h  (frame-char-height))
         (padding (or (bound-and-true-p emms-lyrics-sync-waveform-side-padding) 2))
         (win     (and (fboundp 'emms-lyrics-sync-display--window)
                       (emms-lyrics-sync-display--window)))
         (cols    (max 4 (- (if win (window-body-width  win) 65)
                            (* 2 padding))))
         (rows    (if win (window-body-height win) 12)))
    (cons (* cols char-w)
          (* rows char-h))))

;;; ── Public API ───────────────────────────────────────────────────────────────

;;;###autoload
(cl-defun emms-lyrics-sync-display-anim-render-initial (track)
  "Insert the Phase-1 animation image at point for TRACK.

Returns non-nil when the animation was started, nil when disabled or
when librsvg is unavailable (caller falls through to plain text).

Must be called with `inhibit-read-only' bound (inside --render-lyrics).

Graphical frames:
  1. Inserts one newline as the anchor character.
  2. Creates an overlay over that character.
  3. Sets overlay `before-string' to side-padding spaces (centering).
  4. Sets overlay `display' to the SVG image.
  Subsequent ticks update only the overlay `display' — no further
  buffer writes are needed.

Terminal frames: inserts a plain Unicode bar placeholder.

Schedules the Phase-2 ffprobe run at
`emms-lyrics-sync-display-anim-ffprobe-delay' seconds."
  ;; Bail out immediately when animation is disabled
  (unless emms-lyrics-sync-display-anim-enabled
    (cl-return-from emms-lyrics-sync-display-anim-render-initial nil))
  (emms-lyrics-sync-display-anim--seed track)
  (setq emms-lyrics-sync-display-anim--active t
        emms-lyrics-sync-display-anim--paused nil)
  (if (not (display-graphic-p))
      ;; ── Terminal fallback ──────────────────────────────────────────
      (insert
       (propertize
        (if (fboundp 'emms-lyrics-sync-display--center)
            (emms-lyrics-sync-display--center "  ♪  ♪  ♪  ")
          "   ♪  ♪  ♪")
        'face 'emms-lyrics-sync-future-line-face)
       "\n")
    ;; ── Graphical: SVG overlay ────────────────────────────────────────
    (let* ((size  (emms-lyrics-sync-display-anim--px-size))
           (px-w  (car size))
           (px-h  (cdr size))
           (hs    (emms-lyrics-sync-display-anim--heights))
           (svg   (emms-lyrics-sync-display-anim--make-svg hs px-w px-h))
           (img   (ignore-errors (create-image svg 'svg t :scale 1.0))))
      (if (null img)
          ;; librsvg unavailable or parse error — fall back to plain text
          (progn
            (setq emms-lyrics-sync-display-anim--active nil)
            (insert (propertize "(no lyrics)"
                                'face 'emms-lyrics-sync-past-line-face)
                    "\n")
            (cl-return-from emms-lyrics-sync-display-anim-render-initial nil))
        (let* ((padding      (or (bound-and-true-p
                                  emms-lyrics-sync-waveform-side-padding) 2))
               (anchor-start (point)))
          ;; One newline as anchor; evaporate t auto-cleans on delete-region.
          (insert "\n")
          (let ((ov (make-overlay anchor-start (point)
                                  (current-buffer) t nil)))
            (overlay-put ov 'display      img)
            ;; before-string provides left indent matching waveform padding
            (when (> padding 0)
              (overlay-put ov 'before-string (make-string padding ?\s)))
            (overlay-put ov 'evaporate    t)
            (overlay-put ov 'priority     50)
            (setq emms-lyrics-sync-display-anim--overlay ov))))))
  ;; Phase-2 scheduled regardless of SVG availability
  (emms-lyrics-sync-display-anim--schedule-ffprobe track)
  t)

;;;###autoload
(defun emms-lyrics-sync-display-anim-tick-update ()
  "Advance animation by one 100 ms tick and conditionally refresh the SVG.

The tick counter advances on every call so the breathing phase stays
accurate.  The SVG image is regenerated only every
`emms-lyrics-sync-display-anim-tick-divisor' ticks, throttling the
librsvg parsing CPU cost on remote X11 (S195/Kronos).

No-op when: inactive, paused, on a terminal frame, or overlay is gone."
  (when (and emms-lyrics-sync-display-anim--active
             (not emms-lyrics-sync-display-anim--paused)
             (display-graphic-p)
             (overlayp emms-lyrics-sync-display-anim--overlay)
             (overlay-buffer emms-lyrics-sync-display-anim--overlay))
    ;; Always advance tick (keeps breathing frequency correct)
    (cl-incf emms-lyrics-sync-display-anim--tick)
    ;; Only regenerate SVG every Nth tick
    (when (zerop (mod emms-lyrics-sync-display-anim--tick
                      (max 1 emms-lyrics-sync-display-anim-tick-divisor)))
      (let* ((size  (emms-lyrics-sync-display-anim--px-size))
             (px-w  (car size))
             (px-h  (cdr size))
             (hs    (emms-lyrics-sync-display-anim--heights))
             (svg   (emms-lyrics-sync-display-anim--make-svg hs px-w px-h))
             (img   (ignore-errors (create-image svg 'svg t :scale 1.0))))
        (when img
          (overlay-put emms-lyrics-sync-display-anim--overlay 'display img))))))

;;;###autoload
(defun emms-lyrics-sync-display-anim-pause ()
  "Freeze the animation on its current frame (called on playback pause).
The overlay and last image are preserved — the display is not blank."
  (setq emms-lyrics-sync-display-anim--paused t))

;;;###autoload
(defun emms-lyrics-sync-display-anim-resume ()
  "Unfreeze the animation (called when playback resumes).
Renders one frame immediately so the display updates without waiting
for the next tick-divisor boundary (up to 300 ms at divisor=3)."
  (setq emms-lyrics-sync-display-anim--paused nil)
  ;; Reset the tick counter to 0 so the very next tick-update call
  ;; (mod tick divisor) = 0 and triggers a render immediately.
  ;; This avoids duplicating the SVG generation logic here.
  (setq emms-lyrics-sync-display-anim--tick 0)
  (emms-lyrics-sync-display-anim-tick-update))

;;;###autoload
(defun emms-lyrics-sync-display-anim-stop ()
  "Stop the animation and release all resources.
Called when real lyrics arrive, playback stops, or track changes."
  (setq emms-lyrics-sync-display-anim--active nil
        emms-lyrics-sync-display-anim--paused nil)
  (when emms-lyrics-sync-display-anim--ffprobe-timer
    (cancel-timer emms-lyrics-sync-display-anim--ffprobe-timer)
    (setq emms-lyrics-sync-display-anim--ffprobe-timer nil))
  (emms-lyrics-sync-display-anim--kill-overlay))

;;; ── Phase 2: ffprobe spectral analysis ───────────────────────────────────────

(defun emms-lyrics-sync-display-anim--schedule-ffprobe (track)
  "Schedule Phase-2 ffprobe for TRACK.
Fires `emms-lyrics-sync-display-anim-ffprobe-delay' seconds after this
call.  If lyrics arrive before then, the animation stops and the
callback is a no-op (token guard)."
  (when emms-lyrics-sync-display-anim--ffprobe-timer
    (cancel-timer emms-lyrics-sync-display-anim--ffprobe-timer)
    (setq emms-lyrics-sync-display-anim--ffprobe-timer nil))
  (let ((fp (and track (emms-lyrics-sync-track-file-path track))))
    (when (and fp (executable-find "ffprobe"))
      (let ((token emms-lyrics-sync-display-anim--token))
        (setq emms-lyrics-sync-display-anim--ffprobe-timer
              (run-with-timer
               emms-lyrics-sync-display-anim-ffprobe-delay
               nil
               #'emms-lyrics-sync-display-anim--run-ffprobe
               fp token))))))

(defun emms-lyrics-sync-display-anim--run-ffprobe (file-path token)
  "Run aspectralstats on FILE-PATH for generation TOKEN.
Uses the ffprobe lavfi amovie+aspectralstats pipeline (~26 ms on a
local file).  The process filter kills the process as soon as the
first complete CSV line arrives, mirroring `| head -1' without sh."
  (when (and (= token emms-lyrics-sync-display-anim--token)
             emms-lyrics-sync-display-anim--active
             (file-readable-p file-path)
             (executable-find "ffprobe"))
    (let ((buf      (generate-new-buffer " *emms-anim-ffprobe*"))
          (got-line nil))
      (make-process
       :name    "emms-anim-ffprobe"
       :buffer  buf
       :command (list "ffprobe"
                      "-v"     "quiet"
                      "-f"     "lavfi"
                      "-i"     (format "amovie=%s,aspectralstats=measure=mean"
                                       (emms-lyrics-sync-display-anim--lavfi-quote
                                        file-path))
                      "-show_entries" "frame_tags"
                      "-of"    "csv=p=0")
       :noquery t
       :filter
       (lambda (proc string)
         (when (buffer-live-p (process-buffer proc))
           (with-current-buffer (process-buffer proc)
             (insert string)))
         (unless got-line
           (when (string-match-p "\n" string)
             (setq got-line t)
             (ignore-errors (delete-process proc)))))
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (unwind-protect
               (when (= token emms-lyrics-sync-display-anim--token)
                 (let* ((output (with-current-buffer buf (buffer-string)))
                        (hs     (emms-lyrics-sync-display-anim--parse-ffprobe
                                 output)))
                   (when hs
                     (setq emms-lyrics-sync-display-anim--ffprobe-heights hs)
                     (emms-lyrics-sync-display-anim-tick-update))))
             (ignore-errors (kill-buffer buf)))))))))

(defun emms-lyrics-sync-display-anim--parse-ffprobe (output)
  "Parse ffprobe aspectralstats CSV OUTPUT -> vector of N-BARS heights.

Strategy: extract all positive numbers, sample N-BARS evenly across
the full set, normalise to 0.15-0.90.
Returns nil if fewer than N-BARS positive values are found."
  (condition-case nil
      (let* ((tokens (split-string output "[^0-9.eE+-]+" t " \t\r\n"))
             (vals   (delq nil
                           (mapcar (lambda (s)
                                     (let ((v (string-to-number s)))
                                       (and (> v 0.0) v)))
                                   tokens)))
             (total  (length vals))
             (n      emms-lyrics-sync-display-anim--n-bars))
        (when (>= total n)
          (let* ((step (/ (float total) n))
                 (raw  (cl-loop for i below n
                                collect
                                (nth (min (1- total) (floor (* i step)))
                                     vals)))
                 (mx   (apply #'max raw))
                 (mn   (apply #'min raw))
                 (rng  (- mx mn)))
            (if (> rng 0.0)
                (vconcat
                 (mapcar (lambda (v)
                           (+ 0.15 (* 0.75 (/ (- v mn) rng))))
                         raw))
              (make-vector n 0.50)))))
    (error nil)))

(provide 'emms-lyrics-sync-display-anim)
;;; emms-lyrics-sync-display-anim.el ends here
