;;; emms-lyrics-sync-display.el --- Timer, buffer/window management, hooks  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display.el
;; Created : 2026-06-12 21:54 UTC
;; Purpose : Thin entry point for the emms-lyrics-sync display subsystem.
;;           Provides buffer/window management, the 100 ms timer tick,
;;           and the two integration hooks called by core.el:
;;             `emms-lyrics-sync-display-on-track-change'
;;             `emms-lyrics-sync-display-on-stop'
;;
;;           The tick drives:
;;             - Elapsed-time incremental update (every tick)
;;             - Lyrics context-window advance (when lrc-seek index changes)
;;             - A2 word-highlight overlay repositioning (every tick)
;;             - Waveform cursor update (throttled, every 10 ticks ~1/s)
;;               Skipped when lyrics already redrawn on the same tick.
;;
;;           Load order:
;;             1. emms-lyrics-sync-display-vars.el
;;             2. emms-lyrics-sync-display-render.el
;;             3. emms-lyrics-sync-display-redraw.el
;;             4. emms-lyrics-sync-display.el  ← this file
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)
(require 'emms-lyrics-sync-display-render)
(require 'emms-lyrics-sync-display-redraw)
(require 'emms-lyrics-sync-display-anim)

;;; ── Major mode ──────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'emms-lyrics-sync-display-hide)
    (define-key m (kbd "RET") #'emms-lyrics-sync-display--seek-at-point)
    (define-key m [double-mouse-1] #'emms-lyrics-sync-display--seek-at-point)
    m)
  "Keymap for `emms-lyrics-sync-display-mode'.")

(defun emms-lyrics-sync-display--apply-font ()
  "Apply `display-fontset' or `display-font-family' to current buffer.

Fontset takes precedence; font-family is the recommended path on
Emacs 32 where `create-fontset-from-fontset-spec' produces an
unusable fontset.

The base font height is `display-font-scale' × the frame's default
face height.  This value is stored in
`emms-lyrics-sync-display--base-height' for
`emms-lyrics-sync-display--apply-cjk-font' to multiply by
`display-cjk-scale'.

The family branch sets `face-remapping-alist' directly (rather than
`buffer-face-mode') so we can remap both `default' and
`emms-lyrics-sync-future-line-face'.  The latter explicitly inherits
from `default', and on some Emacs builds the inheritance resolves
against the frame default rather than the remapped default, so
without an explicit remap future lines render at baseline height
while current and past lines pick up the scaled height.  Result
without the fix: mixed Latin/CJK future lines jitter because CJK
gets the scaled height from `--apply-cjk-font' while Latin stays at
baseline."
  (when (bound-and-true-p buffer-face-mode)
    (buffer-face-mode -1))
  (cond
   ;; ── Fontset branch ──────────────────────────────────────────────────
   ((and emms-lyrics-sync-display-fontset
         (or (query-fontset emms-lyrics-sync-display-fontset t)
             (query-fontset
              (concat "-*-" emms-lyrics-sync-display-fontset) t)))
    (setq-local face-remapping-alist
                (list (cons 'default
                            (list :font emms-lyrics-sync-display-fontset))))
    (setq-local emms-lyrics-sync-display--base-height
                (face-attribute 'default :height)))

   (emms-lyrics-sync-display-fontset
    (message
     "emms-lyrics-sync: fontset %S not defined; falling back to font-family"
     emms-lyrics-sync-display-fontset))

   ;; ── Family branch — live path for your config ───────────────────────
   (emms-lyrics-sync-display-font-family
    (let* ((frame-h  (face-attribute 'default :height))
           (scale    (or emms-lyrics-sync-display-font-scale 1.0))
           (scaled-h (round (* frame-h scale))))
      (setq-local emms-lyrics-sync-display--base-height scaled-h)
      (setq-local buffer-face-mode-face
                  (list :family emms-lyrics-sync-display-font-family
                        :height scaled-h))
      (buffer-face-mode 1)))

   ;; ── Neither set ─────────────────────────────────────────────────────
   (t
    (setq-local face-remapping-alist nil)
    (setq-local emms-lyrics-sync-display--base-height
                (face-attribute 'default :height)))))

(define-derived-mode emms-lyrics-sync-display-mode special-mode "EMMS-Lyrics"
  "Major mode for the *emms-lyrics* display buffer."
  :group 'emms-lyrics-sync
  (setq-local cursor-type          nil
              truncate-lines       t
              line-spacing         0.15        ; ← was (if display-line-height 0 0.15)
              left-margin-width    0
              right-margin-width   0
	      left-fringe-width    0
              right-fringe-width   0
	      mode-line-format     nil)
  (emms-lyrics-sync-display--apply-font))

(with-eval-after-load 'emms-playlist-mode
  (set-keymap-parent emms-lyrics-sync-display-mode-map
                     emms-playlist-mode-map))

(defun emms-lyrics-sync-display--seek-at-point ()
  "Seek to the timestamp of the lyric line at point, starting playback if stopped."
  (interactive)
  (let ((ms (get-text-property (line-beginning-position) 'emms-lyrics-sync-start-ms)))
    (unless ms
      (user-error "No timestamp on this line"))
    (let* ((sec (/ ms 1000.0))
           (emms-track (emms-playlist-current-selected-track))
           (file (or emms-lyrics-sync-display--current-file-path
                     (and emms-track (emms-track-get emms-track 'name)))))
      (if (emms-lyrics-sync--currently-playing-p)
          ;; Playing or paused: seek immediately
          (progn
            (emms-lyrics-sync-core--mpv-seek sec)
            ;; Force immediate visual update
            (setq emms-lyrics-sync-core--mpv-position (float sec))
            (emms-lyrics-sync-display--tick)
            ;; Unpause if paused
            (when (bound-and-true-p emms-player-paused-p)
              (emms-pause))
            (message "Seeking to %s" (emms-lyrics-sync-display--format-time sec)))
        ;; Stopped: start the track and queue the seek
        (if (and file (file-exists-p file))
            (progn
              (setq emms-lyrics-sync-core--pending-seek (cons file sec))
              (emms-start)
              (message "Starting track and seeking to %s" (emms-lyrics-sync-display--format-time sec)))
          (user-error "Cannot seek: no file associated with current lyrics"))))))

(defun emms-lyrics-sync-display--waveform-click (event)
  "Seek to the clicked X coordinate on the waveform, starting playback if stopped."
  (interactive "e")
  (let* ((posn (event-start event))
         (x (car (posn-x-y posn)))
         (win (posn-window posn))
         (buf (window-buffer win))
         (px-w (buffer-local-value 'emms-lyrics-sync-waveform--last-px-w buf))
         (emms-track (emms-playlist-current-selected-track))
         (file (or emms-lyrics-sync-display--current-file-path
                   (and emms-track (emms-track-get emms-track 'name))))
         (dur-raw (or (and emms-lyrics-sync-core--current-track
                           (emms-lyrics-sync-track-duration emms-lyrics-sync-core--current-track))
                      (and emms-track (emms-track-get emms-track 'info-playing-time))))
         ;; Coerce to a positive number; guard against strings, 0, and nil
         (dur (cond ((numberp dur-raw) dur-raw)
                    ((and (stringp dur-raw)
                          (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" dur-raw))
                     (string-to-number dur-raw))
                    (t 0))))
    (when (and px-w (> (or px-w 0) 0) (> dur 0))
      (let* ((padding-px (* emms-lyrics-sync-waveform-side-padding (frame-char-width)))
             (rel-x (max 0 (- x padding-px)))
             (ratio (max 0.0 (min 1.0 (/ rel-x (float px-w)))))
             (sec   (* ratio dur)))
        (if (emms-lyrics-sync--currently-playing-p)
            (progn
              (emms-lyrics-sync-core--mpv-seek sec)
              (setq emms-lyrics-sync-core--mpv-position (float sec))
              (emms-lyrics-sync-display--tick)
              (when (bound-and-true-p emms-player-paused-p)
                (emms-pause))
              (message "Seeking to %s"
                       (emms-lyrics-sync-display--format-time sec)))
          (if (and file (file-exists-p file))
              (progn
                (setq emms-lyrics-sync-core--pending-seek (cons file sec))
                (emms-start)
                (message "Starting track and seeking to %s"
                         (emms-lyrics-sync-display--format-time sec)))
            (user-error "Cannot seek: no file associated with current waveform")))))))

;;;###autoload
(defun emms-lyrics-sync-display-refresh-font ()
  "Re-apply font settings to the lyrics buffer without restarting Emacs.
Call after changing `emms-lyrics-sync-display-fontset' or
`emms-lyrics-sync-display-font-family'."
  (interactive)
  (when (buffer-live-p emms-lyrics-sync-display--buffer)
    (with-current-buffer emms-lyrics-sync-display--buffer
      (emms-lyrics-sync-display--apply-font))))

;;; ── Buffer & Window Management ───────────────────────────────────────────────

(defun emms-lyrics-sync-display--get-buffer ()          ; ← KEEP unchanged
  "Return the lyrics buffer, creating and configuring it if needed."
  (unless (buffer-live-p emms-lyrics-sync-display--buffer)
    (setq emms-lyrics-sync-display--buffer
          (get-buffer-create emms-lyrics-sync-display-buffer-name))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (emms-lyrics-sync-display-mode)))
  emms-lyrics-sync-display--buffer)

;;; ── Waveform buffer ──────────────────────────────────────────────────────────

;; The waveform map already has wheel binds and 'q', just add the mouse-1 bind.
;; Note: The click map is attached directly to the image, so we don't strictly 
;; need to define it in the mode map, but it's good for fallback.
(defvar emms-lyrics-sync-waveform-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'emms-lyrics-sync-display-hide)
    ;; Bind left-click to our seek function
    (define-key m [mouse-1] #'emms-lyrics-sync-display--waveform-click)
    (define-key m [double-mouse-1] #'emms-lyrics-sync-display--waveform-click)
    m)
  "Keymap for `emms-lyrics-sync-waveform-mode'.

Player controls inherit from `emms-playlist-mode-map' (set via
`set-keymap-parent' after `emms-playlist-mode' loads), so pause,
next, previous, mute, volume, seek, and speed work identically to
the playlist and lyrics buffers.  F3 L bindings come from the
global `yxz11-emms-global-map' and are active here automatically
because the minor mode is global.")

(with-eval-after-load 'emms-playlist-mode
  (set-keymap-parent emms-lyrics-sync-waveform-mode-map
                     emms-playlist-mode-map))

(define-derived-mode emms-lyrics-sync-waveform-mode special-mode "EMMS-Waveform"
  "Major mode for the dedicated waveform buffer.

The buffer contains a single waveform image (or a Unicode flat bar
fallback).  Player controls inherit from `emms-playlist-mode-map'.
The mode line shows elapsed/duration plus compact artist/title."
  :group 'emms-lyrics-sync
  (setq-local cursor-type         nil
              truncate-lines      t
	      line-spacing        0
              left-margin-width   0
              right-margin-width  0
	      left-fringe-width   0
              right-fringe-width  0
              mode-line-format
              '((:eval emms-lyrics-sync-waveform--cached-mode-line)))
  (pixel-scroll-precision-mode -1)
  )

(defun emms-lyrics-sync-display--get-waveform-buffer ()   ; ← ADD
  "Return the waveform buffer, creating and configuring it if needed."
  (unless (buffer-live-p emms-lyrics-sync-waveform-buffer)
    (setq emms-lyrics-sync-waveform-buffer
          (get-buffer-create emms-lyrics-sync-waveform-buffer-name))
    (with-current-buffer emms-lyrics-sync-waveform-buffer
      (emms-lyrics-sync-waveform-mode)))
  emms-lyrics-sync-waveform-buffer)

(defun emms-lyrics-sync-display--format-waveform-mode-line (pos-ms track)
  "Format the waveform mode line for POS-MS and TRACK.

Centered within the window width.  Each segment uses the
corresponding metadata face; separators render in past-line-face.
Layout adapts to width:
  >= 50 cols:  elapsed/total │ artist — title
  >= 25 cols:  elapsed/total │ artist
  <  25 cols:  elapsed/total"
  (let* ((win       (get-buffer-window emms-lyrics-sync-waveform-buffer t))
         (width     (if win (window-total-width win) 65))
         (dur       (and track (emms-lyrics-sync-track-duration track)))
         (elapsed-s (propertize
                     (emms-lyrics-sync-display--format-time
                      (/ (or pos-ms 0) 1000.0))
                     'face 'emms-lyrics-sync-elapsed-face))
         (dur-s     (propertize
                     (emms-lyrics-sync-display--format-time dur)
                     'face 'emms-lyrics-sync-duration-face))
         (slash     (propertize "/" 'face 'emms-lyrics-sync-past-line-face))
         (pipe      (propertize " │ " 'face 'emms-lyrics-sync-past-line-face))
         (dash      (propertize " — " 'face 'emms-lyrics-sync-past-line-face))
         (time      (concat elapsed-s slash dur-s))
         ;; FIX: Check for nil before calling propertize
         (artist-raw (and track (emms-lyrics-sync-track-artist track)))
         (title-raw  (and track (emms-lyrics-sync-track-title track)))
         (artist     (and artist-raw (propertize
                                      artist-raw
                                      'face 'emms-lyrics-sync-artist-face)))
         (title      (and title-raw (propertize
                                     title-raw
                                     'face 'emms-lyrics-sync-title-face)))
         (content    (cond
                      ((and artist title (>= width 50))
                       (concat time pipe artist dash title))
                      ((and artist (>= width 25))
                       (concat time pipe artist))
                      (t time)))
         (content-w  (string-width content))
         (pad        (max 0 (/ (- width content-w) 2))))
    (concat (make-string pad ?\s) content)))

;;; ── Window management ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--group-visible-p ()       ; ← ADD
  "Return non-nil if the lyrics sidebar is currently displayed."
  (and (buffer-live-p emms-lyrics-sync-display--buffer)
       (get-buffer-window emms-lyrics-sync-display--buffer t)))

(defun emms-lyrics-sync-display--create-windows ()
  "Create metadata, lyrics, and (if enabled) waveform side windows."
  ;; 1. Slot 0: metadata (top, fixed height) — gets space first
  (display-buffer-in-side-window
   (emms-lyrics-sync-display--get-metadata-buffer)
   `((side          . ,emms-lyrics-sync-display-side)
     (slot          . 0)
     (window-width  . ,emms-lyrics-sync-display-width)
     (window-height . ,emms-lyrics-sync-display-metadata-lines)
     (preserve-size . (nil . t))))

  ;; 2. Slot 2: waveform (bottom, fixed height) — gets space second
  (when (and (boundp 'emms-lyrics-sync-waveform-height)
             (> emms-lyrics-sync-waveform-height 0))
    (display-buffer-in-side-window
     (emms-lyrics-sync-display--get-waveform-buffer)
     `((side          . ,emms-lyrics-sync-display-side)
       (slot          . 2)
       (window-height . ,(1+ emms-lyrics-sync-waveform-height))
       (preserve-size . (nil . t)))))

  ;; 3. Slot 1: lyrics (middle, flexible) — created last, takes remainder
  (display-buffer-in-side-window
   (emms-lyrics-sync-display--get-buffer)
   `((side          . ,emms-lyrics-sync-display-side)
     (slot          . 1)
     (window-width  . ,emms-lyrics-sync-display-width)
     (preserve-size . (nil . nil))))

  ;; Atomize the group
  (when-let* ((win (get-buffer-window emms-lyrics-sync-display--buffer t))
              (parent (window-parent win)))
    (window-make-atom parent))

  ;; ... existing display-buffer-in-side-window calls unchanged ...
  ;; ... existing window-make-atom unchanged ...

  ;; Enforce target heights.  `window-height' in the display action is
  ;; a hint, not a contract — Emacs grows the window to fit content or
  ;; to balance the layout.  Explicit resize guarantees the configured
  ;; height, matching the image size from `--cover-pixel-height'.
  (let ((mw (get-buffer-window emms-lyrics-sync-metadata-buffer t)))
    (when mw
      (let ((delta (- emms-lyrics-sync-display-metadata-lines
                      (window-total-height mw))))
        (unless (zerop delta)
          (ignore-errors (window-resize mw delta nil t))))))
  (when (and (boundp 'emms-lyrics-sync-waveform-height)
             (> emms-lyrics-sync-waveform-height 0))
    (let ((ww (get-buffer-window emms-lyrics-sync-waveform-buffer t)))
      (when ww
        (let ((delta (- (1+ emms-lyrics-sync-waveform-height)
                        (window-total-height ww))))
          (unless (zerop delta)
            (ignore-errors (window-resize ww delta nil t))))))))

(defcustom emms-lyrics-sync-display-auto-show-policy 'playlist-visible
  "When to open the lyrics sidebar automatically on track change.

\='always           — always open the sidebar on every new track.
\='playlist-visible — open only when the EMMS playlist buffer is visible
                     in any window on any frame.  If you are working in
                     another buffer and the playlist is buried, the sidebar
                     does not pop up.  If the sidebar is already open it is
                     always updated regardless of this setting.
\='never            — never open automatically; use \[emms-lyrics-sync-display-show]
                     to open manually."
  :type  '(choice (const :tag "Always"            always)
                  (const :tag "Playlist visible"  playlist-visible)
                  (const :tag "Never"             never))
  :group 'emms-lyrics-sync)

(defun emms-lyrics-sync-display--should-auto-show-p ()
  "Return non-nil if the sidebar should be opened on this track change.
If the sidebar is already visible the answer is always yes (we only gate
the *opening* decision, never the *update* decision)."
  (or
   ;; Sidebar already open — always update, never close it.
   (emms-lyrics-sync-display--group-visible-p)
   ;; Sidebar closed — consult policy.
   (pcase emms-lyrics-sync-display-auto-show-policy
     ('always           t)
     ('playlist-visible (get-buffer-window emms-playlist-buffer-name t))
     ('never            nil)
     (_                 t))))

;;;###autoload
(defun emms-lyrics-sync-display-show ()	; ← REPLACE
  "Show the lyrics sidebar.  Creates windows if not already visible."
  (interactive)
  (unless (emms-lyrics-sync-display--group-visible-p)
    (emms-lyrics-sync-display--create-windows)))

;;;###autoload
(defun emms-lyrics-sync-display-hide ()                   ; ← REPLACE
  "Hide the entire lyrics sidebar atomically."
  (interactive)
  (when-let* ((win (get-buffer-window emms-lyrics-sync-display--buffer t)))
    (delete-window (window-atom-root win))))

;;;###autoload
(defun emms-lyrics-sync-display-toggle ()                 ; ← REPLACE
  "Show the lyrics sidebar if hidden, hide it if visible."
  (interactive)
  (if (emms-lyrics-sync-display--group-visible-p)
      (emms-lyrics-sync-display-hide)
    (emms-lyrics-sync-display-show)))

;;; ── Timer ────────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--start-timer ()
  "Start the 100 ms display-update timer."
  (emms-lyrics-sync-display--stop-timer)
  (setq emms-lyrics-sync-display--timer
        (run-at-time nil emms-lyrics-sync-display-update-interval
                     #'emms-lyrics-sync-display--tick)))

(defun emms-lyrics-sync-display--stop-timer ()
  "Cancel the display-update timer."
  (when (timerp emms-lyrics-sync-display--timer)
    (cancel-timer emms-lyrics-sync-display--timer))
  (setq emms-lyrics-sync-display--timer nil))

(defun emms-lyrics-sync-display--tick ()
  "Periodic 100 ms callback: update elapsed, lyrics, and waveform."
  (when (or (emms-lyrics-sync-display--window)
            (get-buffer-window emms-lyrics-sync-waveform-buffer t)
            (get-buffer-window emms-lyrics-sync-metadata-buffer t))
    (condition-case err
        (let* ((result   emms-lyrics-sync-core--current-result)
               (doc      (when result (emms-lyrics-sync-result-doc result)))
               (pos-ms   (or (emms-lyrics-sync-core--playback-position-ms) 0))
               (new-idx  (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))

          ;; 2. Lyric line advance
          (unless (= new-idx emms-lyrics-sync-display--last-line-idx)
            (setq emms-lyrics-sync-display--last-line-idx new-idx)
            (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms))

          ;; 2b. Animation tick (fires only when doc is trivial/nil).
          ;; Safety net: if animation is active but playback has ended
          ;; (emms-player-stopped-hook not fired by persistent player),
          ;; stop the animation here rather than leaving it running.
          (when (bound-and-true-p emms-lyrics-sync-display-anim--active)
            (if (not (bound-and-true-p emms-player-playing-p))
                (emms-lyrics-sync-display-anim-stop)
              (emms-lyrics-sync-display-anim-tick-update)))

          ;; 3. A2 word overlays
          (emms-lyrics-sync-display--update-word-overlays pos-ms)

          ;; 4. Waveform once per second
          (cl-incf emms-lyrics-sync-display--tick-counter)
          (when (zerop (mod emms-lyrics-sync-display--tick-counter 10))
            (emms-lyrics-sync-display--redraw-waveform-window pos-ms)))
      (error
       (message "emms-lyrics-sync-display tick error: %S" err)))))

;;; ── Core Hook Integration ────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-on-track-change ()
  "Called by core.el when a new track starts or new lyrics arrive.

Two cases:
  NEW TRACK  — full redraw (cover+header+lyrics+waveform), restart timer,
               show window.
  SAME TRACK — lyrics+waveform partial update only (cover+header preserved).

Guards:
  - `current-file-path' is compared to detect same-track calls.
  - After a same-track update `last-line-idx' is synced to the current
    seek result so the next tick does not immediately fire another redraw."
  (let* ((track      emms-lyrics-sync-core--current-track)
         (fp         (and track (emms-lyrics-sync-track-file-path track)))
         (result     emms-lyrics-sync-core--current-result)
         (doc        (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms     (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (same-track (and fp
                          (equal fp
                                 emms-lyrics-sync-display--current-file-path))))
    (setq emms-lyrics-sync-display--current-file-path fp)
    (if same-track
	;; Same track: only lyrics need updating
	;; (metadata and waveform are already correct from the new-track render)
	(progn
	  (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms)
	  ;; Sync idx so next tick doesn't immediately redraw again
	  (setq emms-lyrics-sync-display--last-line-idx
		(if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))

      ;; New track: full redraw of everything
      (setq emms-lyrics-sync-display--last-line-idx -1
            emms-lyrics-sync-display--tick-counter  0)

      ;; Create windows FIRST so the render functions have accurate
      ;; window geometry (width for centering, height for the resize
      ;; that --create-windows performs).
      ;; Gate on auto-show policy: if the sidebar is already open it is
      ;; always shown; otherwise consult emms-lyrics-sync-display-auto-show-policy.
      (when (emms-lyrics-sync-display--should-auto-show-p)
        (emms-lyrics-sync-display-show))

      ;; 4c. Kill any running no-lyrics animation from previous track
      ;;     (must come before --full-redraw which calls --render-lyrics)
      (emms-lyrics-sync-display-anim-stop)

      ;; 1. Lyrics buffer (context window + word overlays)
      (emms-lyrics-sync-display--full-redraw)

      ;; 2. Metadata buffer (cover art + header)
      (when track
	(emms-lyrics-sync-display--render-metadata-buffer track pos-ms))

      ;; 3. Waveform buffer (composite image)
      (when (and track
		 (boundp 'emms-lyrics-sync-waveform-height)
		 (> emms-lyrics-sync-waveform-height 0))
	(emms-lyrics-sync-display--redraw-waveform-window pos-ms))

      ;; 4. Async tech-info augmentation (updates only the metadata buffer
      ;;    when ffprobe completes, ~100 ms later)
      (when (and track (fboundp 'emms-lyrics-sync-display--ffprobe-augment))
	(emms-lyrics-sync-display--ffprobe-augment
	 track
	 (lambda ()
	   (when (and (buffer-live-p emms-lyrics-sync-metadata-buffer)
                      (equal fp emms-lyrics-sync-display--current-file-path))
             (emms-lyrics-sync-display--render-metadata-buffer
              track
              (or (emms-lyrics-sync-core--playback-position-ms) 0))))))

      (emms-lyrics-sync-display--start-timer))))

(defun emms-lyrics-sync-display-on-stop ()
  "Called when playback stops (not on pause)."
  (emms-lyrics-sync-display-anim-stop)
  (when (and (boundp 'emms-lyrics-sync-waveform-height)
             (> emms-lyrics-sync-waveform-height 0))
    (emms-lyrics-sync-display--redraw-waveform-window 0))
  (setq emms-lyrics-sync-display--current-file-path nil)
  (emms-lyrics-sync-display--stop-timer))

(defvar emms-lyrics-sync-metadata-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q")           #'emms-lyrics-sync-display-hide)
    (define-key m (kbd "<wheel-up>")   #'ignore)
    (define-key m (kbd "<wheel-down>") #'ignore)
    m)
  "Keymap for `emms-lyrics-sync-metadata-mode'.
Wheel is disabled (metadata is fixed content).")

(define-derived-mode emms-lyrics-sync-metadata-mode special-mode "EMMS-Metadata"
  "Major mode for the dedicated cover/metadata buffer."
  :group 'emms-lyrics-sync
  (setq-local cursor-type        nil
              truncate-lines     t
              line-spacing       0	; ← was 0.15
              left-margin-width  0
              right-margin-width 0
              left-fringe-width  0
              right-fringe-width 0
              mode-line-format   nil
              buffer-read-only   t
	      ;; CRITICAL: Prevent HiDPI scaling from blowing up the image
              image-scaling-factor 1.0)
  (pixel-scroll-precision-mode -1)
  (emms-lyrics-sync-display--apply-font))

(defun emms-lyrics-sync-display--get-metadata-buffer ()
  "Return the metadata buffer, creating and configuring it if needed."
  (unless (buffer-live-p emms-lyrics-sync-metadata-buffer)
    (setq emms-lyrics-sync-metadata-buffer
          (get-buffer-create emms-lyrics-sync-display-metadata-buffer-name))
    (with-current-buffer emms-lyrics-sync-metadata-buffer
      (emms-lyrics-sync-metadata-mode)))
  emms-lyrics-sync-metadata-buffer)

(with-eval-after-load 'emms-playlist-mode
  (set-keymap-parent emms-lyrics-sync-metadata-mode-map
                     emms-playlist-mode-map))

(provide 'emms-lyrics-sync-display)
;;; emms-lyrics-sync-display.el ends here
