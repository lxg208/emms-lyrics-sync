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
    ;; Standard hide-window on q, regardless of what emms-playlist-mode-map
    ;; binds q to (it may hide the playlist, which we don't want in a side
    ;; window showing lyrics).
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `emms-lyrics-sync-display-mode'.

Its parent is set to `emms-playlist-mode-map' so all EMMS player
controls (pause, next, previous, mute, volume, seek, speed, etc.)
work in this buffer as they do in the playlist buffer.  The global
C-c L bindings from `emms-lyrics-sync-mode-map' also apply here.")

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
  "Major mode for the *emms-lyrics* display buffer.

Player controls inherit from `emms-playlist-mode-map'.  Global
C-c L bindings from `emms-lyrics-sync-mode-map' apply on top."
  :group 'emms-lyrics-sync
  (setq-local cursor-type        nil
              truncate-lines     t
              line-spacing       0.15
              left-margin-width  1
              right-margin-width 1)
  ;; Lyrics are read-only; hl-line highlight is visual noise on them,
  ;; especially since each line's last char can look highlighted even
  ;; when it is a plain space.
  (hl-line-mode -1)  
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

(defun emms-lyrics-sync-display--get-buffer ()
  "Return the lyrics buffer, creating and configuring it if needed."
  (unless (buffer-live-p emms-lyrics-sync-display--buffer)
    (setq emms-lyrics-sync-display--buffer
          (get-buffer-create emms-lyrics-sync-display-buffer-name))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (emms-lyrics-sync-display-mode)))
  emms-lyrics-sync-display--buffer)

;;;###autoload
(defun emms-lyrics-sync-display-show ()
  "Show the emms-lyrics-sync buffer in a side window."
  (interactive)
  (display-buffer
   (emms-lyrics-sync-display--get-buffer)
   `(display-buffer-in-side-window
     (side          . ,emms-lyrics-sync-display-side)
     (window-width  . ,emms-lyrics-sync-display-width)
     (preserve-size . (t . nil))
     (slot          . 0))))

;;;###autoload
(defun emms-lyrics-sync-display-hide ()
  "Hide the emms-lyrics-sync side window."
  (interactive)
  (when-let* ((win (emms-lyrics-sync-display--window)))
    (delete-window win)))

;;;###autoload
(defun emms-lyrics-sync-display-toggle ()
  "Toggle the emms-lyrics-sync side window."
  (interactive)
  (if (emms-lyrics-sync-display--window)
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
  "Periodic 100 ms callback: update elapsed, lyrics context, and waveform."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (emms-lyrics-sync-display--window))
    (condition-case err
        (let* ((result  emms-lyrics-sync-core--current-result)
               (doc     (when result (emms-lyrics-sync-result-doc result)))
               (pos-ms  (or (emms-lyrics-sync-core--playback-position-ms) 0))
               (new-idx (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))
               (lyrics-redrawn nil))

          ;; 1. Elapsed time — every tick
          (emms-lyrics-sync-display--update-elapsed (/ pos-ms 1000.0))

          ;; 2. Lyrics context — only when line index changes
          (unless (= new-idx emms-lyrics-sync-display--last-line-idx)
            (setq emms-lyrics-sync-display--last-line-idx new-idx
                  lyrics-redrawn t)
            (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms))

          ;; 3. A2 word overlays — every tick for smooth animation
          (emms-lyrics-sync-display--update-word-overlays pos-ms)

          ;; 4. Waveform cursor — throttled to ~1/s (every 10 ticks).
          ;;    Skipped when lyrics were just redrawn (redraw-lyrics-only
          ;;    already called render-waveform-cached which updated the bar).
          (cl-incf emms-lyrics-sync-display--tick-counter)
          (when (and (zerop (mod emms-lyrics-sync-display--tick-counter 10))
                     (not lyrics-redrawn))
            (emms-lyrics-sync-display--update-waveform-cursor pos-ms)))

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
  "Called by core.el when playback stops or is paused."
  ;; Reset so the next play — even of the same track — triggers a full redraw
  ;; and resets the waveform placeholder.
  (setq emms-lyrics-sync-display--current-file-path nil)
  (emms-lyrics-sync-display--stop-timer))

(provide 'emms-lyrics-sync-display)
;;; emms-lyrics-sync-display.el ends here
