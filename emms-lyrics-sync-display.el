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

;;; ── Major mode ──────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'emms-lyrics-sync-display-hide)
    map)
  "Keymap for `emms-lyrics-sync-display-mode'.

Its parent is set to `emms-playlist-mode-map' so all EMMS player
controls work in this buffer.  `q' hides the entire sidebar group
atomically (both lyrics and waveform windows), not just this window.")

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

(defvar emms-lyrics-sync-waveform-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'emms-lyrics-sync-display-hide)
    m)
  "Keymap for `emms-lyrics-sync-waveform-mode'.

Player controls inherit from `emms-playlist-mode-map' (set via
`set-keymap-parent' after `emms-playlist-mode' loads), so pause,
next, previous, mute, volume, seek, and speed work identically to
the playlist and lyrics buffers.  F3 L bindings come from the
global `emms-lyrics-sync-mode-map' and are active here automatically
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
              left-margin-width   0
              right-margin-width  0
	      left-fringe-width   0
              right-fringe-width  0
              mode-line-format
              '((:eval emms-lyrics-sync-waveform--cached-mode-line))))

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
	 (width     (if win (window-total-width win) 65))    ; ← was window-body-width
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
         (artist    (and track (propertize
                                (emms-lyrics-sync-track-artist track)
                                'face 'emms-lyrics-sync-artist-face)))
         (title     (and track (propertize
                                (emms-lyrics-sync-track-title track)
                                'face 'emms-lyrics-sync-title-face)))
         (content   (cond
                     ((and artist title (>= width 50))
                      (concat time pipe artist dash title))
                     ((and artist (>= width 25))
                      (concat time pipe artist))
                     (t time)))
         (content-w (string-width content))
         (pad       (max 0 (/ (- width content-w) 2))))
    (concat (make-string pad ?\s) content)))

;;; ── Window management ────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--group-visible-p ()       ; ← ADD
  "Return non-nil if the lyrics sidebar is currently displayed."
  (and (buffer-live-p emms-lyrics-sync-display--buffer)
       (get-buffer-window emms-lyrics-sync-display--buffer t)))

(defun emms-lyrics-sync-display--create-windows ()        ; ← ADD
  "Create the lyrics and (if enabled) waveform side windows, atomized."
  (display-buffer-in-side-window
   (emms-lyrics-sync-display--get-buffer)
   `((side          . ,emms-lyrics-sync-display-side)
     (slot          . 0)
     (window-width  . ,emms-lyrics-sync-display-width)
     (preserve-size . (t . nil))))
  (when (and (boundp 'emms-lyrics-sync-waveform-height)
             (> emms-lyrics-sync-waveform-height 0))
    (display-buffer-in-side-window
     (emms-lyrics-sync-display--get-waveform-buffer)
     `((side          . ,emms-lyrics-sync-display-side)
       (slot          . 1)
       (window-height . ,(1+ emms-lyrics-sync-waveform-height))
       (preserve-size . (nil . t))))
    (when-let* ((win (get-buffer-window emms-lyrics-sync-display--buffer t))
                (parent (window-parent win)))
      (window-make-atom parent))))

;;;###autoload
(defun emms-lyrics-sync-display-show ()                   ; ← REPLACE
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
            (get-buffer-window emms-lyrics-sync-waveform-buffer t))
    (condition-case err
        (let* ((result   emms-lyrics-sync-core--current-result)
               (doc      (when result (emms-lyrics-sync-result-doc result)))
               (pos-ms   (or (emms-lyrics-sync-core--playback-position-ms) 0))
               (new-idx  (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))

          ;; 1. Elapsed time in lyrics buffer
          (when (emms-lyrics-sync-display--window)
            (emms-lyrics-sync-display--update-elapsed (/ pos-ms 1000.0)))

          ;; 2. Lyric line advance
          (unless (= new-idx emms-lyrics-sync-display--last-line-idx)
            (setq emms-lyrics-sync-display--last-line-idx new-idx)
            (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms))

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
        ;; Same track: only lyrics+waveform need updating
        (progn
          (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms)
          ;; Sync idx so next tick doesn't immediately redraw again
          (setq emms-lyrics-sync-display--last-line-idx
                (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))
      ;; New track: full redraw
      (setq emms-lyrics-sync-display--last-line-idx -1
            emms-lyrics-sync-display--tick-counter  0)
      (emms-lyrics-sync-display--full-redraw)
      ;; Within the `else` (new track) branch of on-track-change, after full-redraw:
      (when (and track
		 (boundp 'emms-lyrics-sync-waveform-height)
		 (> emms-lyrics-sync-waveform-height 0))
	(emms-lyrics-sync-display--redraw-waveform-window pos-ms))
      ;; Kick off async ffprobe tech-info augmentation.
      ;; When it completes it re-renders only the header tech line.
      (when (and track (fboundp 'emms-lyrics-sync-display--ffprobe-augment))
        (emms-lyrics-sync-display--ffprobe-augment
         track
         (lambda ()
           (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
                      ;; Still same track?
                      (equal fp emms-lyrics-sync-display--current-file-path))
             (with-current-buffer emms-lyrics-sync-display--buffer
               (let ((inhibit-read-only t)
                     (cur-pos-ms
                      (or (emms-lyrics-sync-core--playback-position-ms) 0)))
                 ;; Re-render just the header (cover + lyrics preserved)
                 ;; Simplest correct approach: full-redraw.
                 ;; This fires only once per track so it is not expensive.
                 (emms-lyrics-sync-display--full-redraw)))))))
      (emms-lyrics-sync-display--start-timer)
      (emms-lyrics-sync-display-show))))

(defun emms-lyrics-sync-display-on-stop ()
  "Called when playback stops (not on pause).

Resets the waveform to full-grey at 0:00, because after a stop the
user cannot return to the previous position (unless Phase 4's seek
feature is implemented).  Pause is handled separately and leaves the
display frozen at its current position."
  (when (and (boundp 'emms-lyrics-sync-waveform-height)
             (> emms-lyrics-sync-waveform-height 0))
    (emms-lyrics-sync-display--redraw-waveform-window 0))
  (setq emms-lyrics-sync-display--current-file-path nil)
  (emms-lyrics-sync-display--stop-timer))


(provide 'emms-lyrics-sync-display)
;;; emms-lyrics-sync-display.el ends here
