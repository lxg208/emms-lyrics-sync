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

;;; ── Buffer & Window Management ───────────────────────────────────────────────

(defun emms-lyrics-sync-display--get-buffer ()
  "Return the lyrics buffer, creating and configuring it if needed."
  (unless (buffer-live-p emms-lyrics-sync-display--buffer)
    (setq emms-lyrics-sync-display--buffer
          (get-buffer-create emms-lyrics-sync-display-buffer-name))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (special-mode)
      (setq-local cursor-type        nil
                  truncate-lines     t
                  line-spacing       0.15
                  left-margin-width  1
                  right-margin-width 1)))
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
