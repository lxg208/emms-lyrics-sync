;;; emms-lyrics-sync-display-redraw.el --- Full and partial buffer redraw  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-redraw.el
;; Created : 2026-06-13 05:35 UTC
;; Purpose : Buffer redraw orchestration for the emms-lyrics-sync display
;;           subsystem.  Provides four functions called by display.el:
;;
;;             `emms-lyrics-sync-display--render-waveform-cached'
;;               Inserts waveform from cache (no ffmpeg).  Sets
;;               waveform-marker with insertion-type NIL.  Used by both
;;               full-redraw (via waveform-insert callback) and
;;               redraw-lyrics-only.
;;
;;             `emms-lyrics-sync-display--update-waveform-cursor'
;;               Replaces only the waveform bar region.  Uses
;;               waveform-marker (NIL type) as boundary.  Throttled to
;;               ~1/s by the tick function in display.el.
;;
;;             `emms-lyrics-sync-display--full-redraw'
;;               Erases and rebuilds entire buffer: cover → header →
;;               lyrics → waveform.  Sets elapsed markers, lyrics-marker
;;               and waveform-marker.
;;
;;             `emms-lyrics-sync-display--redraw-lyrics-only'
;;               Replaces only the lyrics+waveform region.  Cover and
;;               header are preserved.  Uses lyrics-marker (NIL type)
;;               as the cut boundary.
;;
;;           Marker invariants — CRITICAL, do not break:
;;
;;           `elapsed-marker' — copy-marker with insertion-type NIL.
;;             Stays at the START of the elapsed text regardless of
;;             subsequent inserts.  Used as the start of delete-region.
;;
;;           `elapsed-end-marker' — copy-marker with insertion-type T.
;;             ADVANCES past inserted content.  After delete-region
;;             both markers collapse to start; after insert the end-marker
;;             advances to mark the new end.  Next call deletes exactly
;;             the previous elapsed text.  MUST be T — if NIL the marker
;;             stays at start and delete-region deletes nothing, causing
;;             the "0:260:250:250:..." accumulation bug.
;;
;;           `lyrics-marker' — copy-marker with insertion-type NIL.
;;             Marks the header/lyrics boundary.  redraw-lyrics-only
;;             calls (delete-region lyrics-marker (point-max)) then
;;             re-inserts from that point.  NIL type means the marker
;;             does NOT advance when content is inserted at its position,
;;             so subsequent calls always delete from the same boundary.
;;
;;           `waveform-marker' — copy-marker with insertion-type NIL.
;;             Set by render-waveform-cached AFTER the separator \n,
;;             BEFORE waveform content.  NIL type means the marker stays
;;             at the bar start regardless of insert-image / insert at
;;             that position.  update-waveform-cursor uses
;;             (delete-region waveform-marker (point-max)) to replace
;;             only the bar, never the lyrics above it.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Load order within the display subsystem:
;;   1. emms-lyrics-sync-display-vars.el   ← defcustom, defface, state, utils
;;   2. emms-lyrics-sync-display-render.el ← per-element insert functions
;;   3. emms-lyrics-sync-display-redraw.el ← this file
;;   4. emms-lyrics-sync-display.el        ← timer, hooks (thin entry point)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)
(require 'emms-lyrics-sync-display-render)

;;; ── Forward declaration ──────────────────────────────────────────────────────
(declare-function emms-lyrics-sync-display--get-buffer
                  "emms-lyrics-sync-display")

;;; ── Waveform: Cached Render ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-waveform-cached (pos-ms)
  "Insert waveform at point using only cached data; always sets waveform-marker.

Invariant: inserts separator \\n FIRST, then sets waveform-marker with
insertion-type NIL, then inserts bar content."
  (let* ((track    emms-lyrics-sync-core--current-track)
         (fp       (and track (emms-lyrics-sync-track-file-path track)))
         (duration (and track (emms-lyrics-sync-track-duration track)))
         (data     (and fp
                        (boundp 'emms-lyrics-sync-waveform--cache)
                        (gethash fp emms-lyrics-sync-waveform--cache)))
         (char-w   (frame-char-width))
         (char-h   (frame-char-height))
         (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
         (px-w     (* win-chars char-w))
         (px-h     (* (if (boundp 'emms-lyrics-sync-waveform-height)
                          emms-lyrics-sync-waveform-height 2)
                      char-h))
         (pos-s    (/ (float pos-ms) 1000.0))
         (render-data
          (if (vectorp data)
              data
            (let ((v (make-vector win-chars nil)))
              (dotimes (i win-chars) (aset v i (vector 0.5 0.5)))
              v))))
    (insert "\n")
    (setq emms-lyrics-sync-display--waveform-marker
          (copy-marker (point) nil))
    (cond
     ((and (fboundp 'emms-lyrics-sync-waveform--use-svg-p)
           (emms-lyrics-sync-waveform--use-svg-p)
           (fboundp 'emms-lyrics-sync-waveform--render-svg))
      (let ((img (emms-lyrics-sync-waveform--render-svg
                  render-data px-w px-h pos-s duration)))
        (when img (insert-image img)))
      (insert "\n"))
     ((fboundp 'emms-lyrics-sync-waveform--render-unicode)
      (insert (emms-lyrics-sync-waveform--render-unicode
               render-data win-chars pos-s duration))
      (insert "\n")))))

;;; ── Waveform: Cursor Update ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--update-waveform-cursor (pos-ms)
  "Replace only the waveform bar with an updated playback cursor at POS-MS."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker))
    (let* ((track    emms-lyrics-sync-core--current-track)
           (fp       (and track (emms-lyrics-sync-track-file-path track)))
           (duration (and track (emms-lyrics-sync-track-duration track)))
           (data     (and fp
                          (boundp 'emms-lyrics-sync-waveform--cache)
                          (gethash fp emms-lyrics-sync-waveform--cache))))
      (when (vectorp data)
        (with-current-buffer emms-lyrics-sync-display--buffer
          (let* ((inhibit-read-only t)
                 (wf-start  (marker-position
                             emms-lyrics-sync-display--waveform-marker))
                 (char-w    (frame-char-width))
                 (char-h    (frame-char-height))
                 (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
                 (px-w      (* win-chars char-w))
                 (px-h      (* (if (boundp 'emms-lyrics-sync-waveform-height)
                                   emms-lyrics-sync-waveform-height 2)
                               char-h))
                 (pos-s     (/ (float pos-ms) 1000.0)))
            ;; Guard: wf-start must be a valid buffer position
            (when (and (integerp wf-start)
                       (<= wf-start (point-max)))
              (delete-region wf-start (point-max))
              (goto-char wf-start)
              (cond
               ((and (fboundp 'emms-lyrics-sync-waveform--use-svg-p)
                     (emms-lyrics-sync-waveform--use-svg-p)
                     (fboundp 'emms-lyrics-sync-waveform--render-svg))
                (let ((img (emms-lyrics-sync-waveform--render-svg
                            data px-w px-h pos-s duration)))
                  (when img (insert-image img)))
                (insert "\n"))
               ((fboundp 'emms-lyrics-sync-waveform--render-unicode)
                (insert (emms-lyrics-sync-waveform--render-unicode
                         data win-chars pos-s duration))
                (insert "\n"))))))))))

;;; ── Full Buffer Redraw ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--full-redraw ()
  "Erase and repopulate the entire lyrics buffer for the current track."
  (let* ((track  emms-lyrics-sync-core--current-track)
         (result emms-lyrics-sync-core--current-result)
         (doc    (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (buf    (emms-lyrics-sync-display--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)

        ;; ── Cover art ─────────────────────────────────────────────────────
        (when track
          (let* ((fp  (emms-lyrics-sync-track-file-path track))
                 (img (emms-lyrics-sync-display--cover-image fp)))
            (when img
              (let* ((img-w    (or (car (image-size img t)) 0))
                     (char-w   (frame-char-width))
                     (win-px-w (* (emms-lyrics-sync-display--body-width) char-w))
                     (pad-ch   (max 0 (floor (/ (- win-px-w img-w)
                                                2.0 char-w)))))
                (insert (make-string pad-ch ?\s))
                (insert-image img)
                (insert "\n\n")))))

        ;; ── Metadata header ───────────────────────────────────────────────
        (when track
          (let ((erange (emms-lyrics-sync-display--render-header
                         track (/ pos-ms 1000.0))))
            ;; elapsed-marker: NIL type — stays at start of elapsed text
            ;; elapsed-end-marker: T type — advances past inserted text
            ;; CRITICAL: end-marker MUST be T or elapsed accumulates
            (setq emms-lyrics-sync-display--elapsed-marker
                  (copy-marker (car erange) nil)
                  emms-lyrics-sync-display--elapsed-end-marker
                  (copy-marker (cdr erange) t))))
        (insert "\n")

        ;; ── Lyrics marker (NIL type — never advances) ─────────────────────
        (setq emms-lyrics-sync-display--lyrics-marker
              (copy-marker (point) nil))

        ;; ── Lyrics ────────────────────────────────────────────────────────
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)

        ;; ── Waveform ──────────────────────────────────────────────────────
        (when (and track (fboundp 'emms-lyrics-sync-waveform-insert))
          (emms-lyrics-sync-waveform-insert
           (emms-lyrics-sync-track-file-path track)
           (/ pos-ms 1000.0)
           (emms-lyrics-sync-track-duration track)))

        (goto-char (point-min))
        (setq emms-lyrics-sync-display--last-line-idx
              (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))))))

;;; ── Partial Redraw: Lyrics + Waveform ────────────────────────────────────────

(defun emms-lyrics-sync-display--redraw-lyrics-only (doc pos-ms)
  "Replace lyrics and waveform without touching cover or header."
  (when (and (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let* ((inhibit-read-only t)
             (lm-pos (marker-position emms-lyrics-sync-display--lyrics-marker)))
        ;; Guard: marker position must be valid
        (when (and (integerp lm-pos) (<= lm-pos (point-max)))
          (emms-lyrics-sync-display--clear-sung-overlays)
          (setq emms-lyrics-sync-display--word-positions nil)
          (delete-region lm-pos (point-max))
          (goto-char lm-pos)
          (emms-lyrics-sync-display--render-lyrics doc pos-ms)
          (emms-lyrics-sync-display--render-waveform-cached pos-ms))))))

(provide 'emms-lyrics-sync-display-redraw)
;;; emms-lyrics-sync-display-redraw.el ends here
