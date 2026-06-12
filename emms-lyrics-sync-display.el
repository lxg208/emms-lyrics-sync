;;; emms-lyrics-sync-display.el --- Full redraw, timer, buffer/window management, hooks  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display.el
;; Created : 2026-06-12 15:56 UTC
;; Purpose : Top-level entry point for the emms-lyrics-sync display subsystem.
;;           Orchestrates full buffer redraws on track changes (cover + header
;;           + lyrics + waveform), drives the 100 ms timer tick (elapsed time,
;;           A2 word overlays, throttled waveform cursor update), manages the
;;           side window, and wires the two integration hooks called by core:
;;             `emms-lyrics-sync-display-on-track-change'
;;             `emms-lyrics-sync-display-on-stop'
;;
;; Fixes in this version:
;;
;;   full-redraw: does NOT set waveform-marker itself.  waveform-insert
;;     (called here) sets it with insertion-type NIL after the separator \\n.
;;     Previously full-redraw set the marker before calling waveform-insert,
;;     and waveform-insert set it again inside with type T, causing the marker
;;     to advance past all content and making update-waveform-cursor think the
;;     bar region was empty.
;;
;;   redraw-lyrics-only: calls emms-lyrics-sync-waveform-insert (not the
;;     removed render-waveform-cached helper) so the marker is always refreshed
;;     correctly after the lyrics region is deleted.  Guard changed from
;;     (fboundp 'emms-lyrics-sync-waveform--cache) to
;;     (fboundp 'emms-lyrics-sync-waveform-insert) which is an actual function.
;;
;;   update-waveform-cursor: deletes from wf-start to point-max and re-inserts
;;     only the bar + trailing \\n.  The separator \\n that precedes wf-start
;;     is owned by waveform-insert and is never touched here.
;;
;;   tick: lyrics-redrawn flag prevents update-waveform-cursor running on the
;;     same tick that redraw-lyrics-only already re-rendered waveform.
;;
;;   on-track-change same-track branch: syncs last-line-idx to lrc-seek result
;;     so the very next tick does not immediately fire another
;;     redraw-lyrics-only.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Load order within the display subsystem:
;;   1. emms-lyrics-sync-display-vars.el   ← customization, faces, state
;;   2. emms-lyrics-sync-display-render.el ← all buffer-insert render fns
;;   3. emms-lyrics-sync-display.el        ← this file (entry point)
;;
;; External callers only need (require 'emms-lyrics-sync-display).
;;
;; Full redraw (cover + header + lyrics + waveform):
;;   Triggered on every new track and by the waveform extraction callback.
;;
;; Partial redraw (lyrics + waveform only):
;;   Triggered when on-track-change fires for the same file path (lyrics
;;   arrived after initial nil-lyrics render), or by the tick when
;;   lrc-seek returns a new line index.
;;
;; Incremental update (no redraw):
;;   Every 100 ms: elapsed-time text via marker pair.
;;   Every 100 ms: A2 word overlay repositioned via move-overlay.
;;   Every ~1 s  : waveform cursor replaced via delete+re-insert of bar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)
(require 'emms-lyrics-sync-display-render)

;;; ── Buffer & Window Management ───────────────────────────────────────────────

(defun emms-lyrics-sync-display--get-buffer ()
  "Return the lyrics buffer, creating it if needed."
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

;;; ── Waveform Cursor Update ───────────────────────────────────────────────────

(defun emms-lyrics-sync-display--update-waveform-cursor (pos-ms)
  "Replace only the waveform bar region with an updated cursor at POS-MS.

The replaceable region is: waveform-marker … point-max.
The separator \\n before waveform-marker is owned by waveform-insert
and is never touched here.

No-op when:
  - waveform data is not yet cached (still placeholder)
  - the marker is invalid
  - waveform.el is not loaded"
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker)
             (fboundp 'emms-lyrics-sync-waveform--render-unicode))
    (let* ((track    emms-lyrics-sync-core--current-track)
           (fp       (and track (emms-lyrics-sync-track-file-path track)))
           (duration (and track (emms-lyrics-sync-track-duration track)))
           (data     (and fp
                          (boundp 'emms-lyrics-sync-waveform--cache)
                          (gethash fp emms-lyrics-sync-waveform--cache))))
      ;; Only update when real extracted data is present (not placeholder)
      (when (vectorp data)
        (with-current-buffer emms-lyrics-sync-display--buffer
          (let* ((inhibit-read-only t)
                 (wf-start  (marker-position
                             emms-lyrics-sync-display--waveform-marker))
                 (width     (max 4 (- (emms-lyrics-sync-display--body-width) 2)))
                 (pos-s     (/ pos-ms 1000.0))
                 (use-svg   (and (not (eq emms-lyrics-sync-waveform-render-mode 'unicode))
                                 (display-graphic-p)
                                 (image-type-available-p 'svg)
                                 (ignore-errors (require 'svg nil t) t)
                                 (fboundp 'svg-create))))
            (delete-region wf-start (point-max))
            (goto-char wf-start)
            (if (and use-svg (fboundp 'emms-lyrics-sync-waveform--render-svg))
                (let ((img (emms-lyrics-sync-waveform--render-svg
                            data width pos-s duration)))
                  (when img (insert-image img))
                  (insert "\n"))
              (when (fboundp 'emms-lyrics-sync-waveform--render-unicode)
                (insert (emms-lyrics-sync-waveform--render-unicode
                         data width pos-s duration))
                (insert "\n")))))))))

;;; ── Full Buffer Redraw ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--full-redraw ()
  "Erase and repopulate the entire lyrics buffer for the current track.

Cover centering: pixel padding is computed as
  floor((win-px-w - img-px-w) / 2 / char-px-w)
giving the correct character count with a single division.

Waveform: calls emms-lyrics-sync-waveform-insert which (a) sets
waveform-marker with insertion-type NIL and (b) triggers async
extraction if the file has not been processed yet."
  (let* ((track   emms-lyrics-sync-core--current-track)
         (result  emms-lyrics-sync-core--current-result)
         (doc     (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms  (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (buf     (emms-lyrics-sync-display--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)

        ;; ── Cover art ────────────────────────────────────────────────────
        (when track
          (let* ((fp      (emms-lyrics-sync-track-file-path track))
                 (img     (emms-lyrics-sync-display--cover-image fp)))
            (when img
              (let* ((img-px-w  (or (car (image-size img t)) 0))
                     (char-px-w (frame-char-width))
                     (win-px-w  (* (emms-lyrics-sync-display--body-width) char-px-w))
                     (pad-chars (max 0 (floor (/ (- win-px-w img-px-w)
                                                 2.0 char-px-w)))))
                (insert (make-string pad-chars ?\s))
                (insert-image img)
                (insert "\n\n")))))

        ;; ── Metadata header ──────────────────────────────────────────────
        (when track
          (let ((elapsed-range
                 (emms-lyrics-sync-display--render-header
                  track (/ pos-ms 1000.0))))
            (when (car elapsed-range)
              (setq emms-lyrics-sync-display--elapsed-marker
                    (copy-marker (car elapsed-range) t)
                    emms-lyrics-sync-display--elapsed-end-marker
                    (copy-marker (cdr elapsed-range) nil)))))
        (insert "\n")

        ;; ── Lyrics ───────────────────────────────────────────────────────
        (setq emms-lyrics-sync-display--lyrics-marker
              (copy-marker (point) nil))   ; nil = stays at lyrics start
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)

        ;; ── Waveform ─────────────────────────────────────────────────────
        ;; waveform-insert sets waveform-marker internally.
        ;; Do NOT set it here — that would race with the insertion-type.
        (when (and track (fboundp 'emms-lyrics-sync-waveform-insert))
          (emms-lyrics-sync-waveform-insert
           (emms-lyrics-sync-track-file-path track)
           (/ pos-ms 1000.0)
           (emms-lyrics-sync-track-duration track)))

        (goto-char (point-min))
        (setq emms-lyrics-sync-display--last-line-idx
              (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))))))

;;; ── Partial Redraw (lyrics + waveform only) ──────────────────────────────────

(defun emms-lyrics-sync-display--redraw-lyrics-only (doc pos-ms)
  "Replace lyrics and waveform sections for POS-MS.
Preserves cover art and header.  Re-renders waveform from cache with
an updated cursor — no additional ffmpeg extraction."
  (when (and (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)
        ;; Delete lyrics AND waveform (everything from lyrics-marker onward)
        (delete-region (marker-position emms-lyrics-sync-display--lyrics-marker)
                       (point-max))
        (goto-char (marker-position emms-lyrics-sync-display--lyrics-marker))
        ;; Re-render lyrics
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)
        ;; Re-render waveform — waveform-insert refreshes waveform-marker
        (when (and emms-lyrics-sync-core--current-track
                   (fboundp 'emms-lyrics-sync-waveform-insert))
          (let* ((track emms-lyrics-sync-core--current-track)
                 (fp    (emms-lyrics-sync-track-file-path track))
                 (dur   (emms-lyrics-sync-track-duration track)))
            (emms-lyrics-sync-waveform-insert fp (/ pos-ms 1000.0) dur)))))))

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
  "Periodic 100 ms callback: update elapsed, lyrics context, word overlays,
and (throttled) waveform cursor."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (emms-lyrics-sync-display--window))
    (condition-case err
        (let* ((result         emms-lyrics-sync-core--current-result)
               (doc            (when result (emms-lyrics-sync-result-doc result)))
               (pos-ms         (or (emms-lyrics-sync-core--playback-position-ms) 0))
               (new-idx        (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))
               (lyrics-redrawn nil))

          ;; ── Elapsed time — every tick ───────────────────────────────────
          (emms-lyrics-sync-display--update-elapsed (/ pos-ms 1000.0))

          ;; ── Lyrics context — only when line index advances ──────────────
          (unless (= new-idx emms-lyrics-sync-display--last-line-idx)
            (setq emms-lyrics-sync-display--last-line-idx new-idx
                  lyrics-redrawn t)
            (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms))

          ;; ── A2 word overlays — every tick ───────────────────────────────
          (emms-lyrics-sync-display--update-word-overlays pos-ms)

          ;; ── Waveform cursor — throttled, skip if lyrics just redrawn ────
          (cl-incf emms-lyrics-sync-display--tick-counter)
          (when (and (zerop (mod emms-lyrics-sync-display--tick-counter 10))
                     (not lyrics-redrawn))
            (emms-lyrics-sync-display--update-waveform-cursor pos-ms)))
      (error
       (message "emms-lyrics-sync-display tick error: %S" err)))))

;;; ── Core Hook Integration ────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-on-track-change ()
  "Called by `emms-lyrics-sync-core' when a new track starts or lyrics arrive.

New track  — full redraw (cover + header + lyrics + waveform), timer restart.
Same track — partial redraw (lyrics + waveform only; cover/header preserved).

After the same-track partial update, last-line-idx is synced to the current
lrc-seek result so the very next tick does not immediately fire another
redraw-lyrics-only."
  (let* ((track      emms-lyrics-sync-core--current-track)
         (fp         (and track (emms-lyrics-sync-track-file-path track)))
         (result     emms-lyrics-sync-core--current-result)
         (doc        (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms     (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (same-track (and fp
                          (equal fp emms-lyrics-sync-display--current-file-path))))
    (setq emms-lyrics-sync-display--current-file-path fp)
    (if same-track
        (progn
          ;; Partial update — preserve cover/header
          (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms)
          ;; Sync idx so next tick doesn't immediately re-fire
          (setq emms-lyrics-sync-display--last-line-idx
                (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))
      ;; New track — full redraw
      (setq emms-lyrics-sync-display--last-line-idx -1
            emms-lyrics-sync-display--tick-counter  0)
      (emms-lyrics-sync-display--full-redraw)
      (emms-lyrics-sync-display--start-timer)
      (emms-lyrics-sync-display-show))))

(defun emms-lyrics-sync-display-on-stop ()
  "Called by `emms-lyrics-sync-core' when playback stops or is paused."
  ;; Reset so the next play — even of the same track — triggers a full redraw.
  (setq emms-lyrics-sync-display--current-file-path nil)
  (emms-lyrics-sync-display--stop-timer))

(provide 'emms-lyrics-sync-display)
;;; emms-lyrics-sync-display.el ends here
