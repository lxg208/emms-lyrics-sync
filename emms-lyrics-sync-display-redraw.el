;;; emms-lyrics-sync-display-redraw.el --- Full and partial buffer redraw  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-redraw.el
;; Created : 2026-06-13 16:00 UTC
;; Purpose : Buffer redraw orchestration for the emms-lyrics-sync display
;;           subsystem.  Updated to use the PNG-based waveform renderer
;;           (showwavespic) instead of the old astats+SVG approach.
;;
;;           CRITICAL BUG FIX — consp vs listp:
;;             The waveform PNG cache stores three distinct values:
;;               nil       — not yet requested
;;               'pending  — async ffmpeg processes running
;;               plist     — (:px-w W :px-h H :played PATH :remaining PATH)
;;
;;             In Elisp, nil IS the empty list, so:
;;               (listp nil)      → t   ← WRONG: nil passes the guard!
;;               (consp nil)      → nil ← correct: nil is excluded
;;               (consp 'pending) → nil ← correct: symbol excluded
;;               (consp '(:k v))  → t   ← correct: plist passes
;;
;;             Any guard written as (listp cached) would pass when cached
;;             is nil (cache miss), then try (plist-get nil :px-w) → nil,
;;             then (= nil px-w) → (wrong-type-argument number-or-marker-p nil).
;;             This was the exact error seen in the backtrace.
;;             ALL cache guards in this file use consp.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

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
  "Insert waveform at point using the PNG cache; always sets waveform-marker.

Invariant: inserts separator \\n FIRST, then sets waveform-marker with
insertion-type NIL, then inserts bar content.

Delegates entirely to `emms-lyrics-sync-waveform--insert-at-point' which
handles the marker, placeholder, and PNG composite internally."
  (let* ((track     emms-lyrics-sync-core--current-track)
         (fp        (and track (emms-lyrics-sync-track-file-path track)))
         (duration  (and track (emms-lyrics-sync-track-duration track)))
         (char-w    (frame-char-width))
         (char-h    (frame-char-height))
         (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
         (px-w      (* win-chars char-w))
         (px-h      (* (if (boundp 'emms-lyrics-sync-waveform-height)
                           emms-lyrics-sync-waveform-height 2)
                       char-h))
         (pos-s     (/ (float pos-ms) 1000.0)))
    (cond
     ;; ── PNG mode ─────────────────────────────────────────────────────────
     ((and fp
           (fboundp 'emms-lyrics-sync-waveform--use-png-p)
           (emms-lyrics-sync-waveform--use-png-p)
           (fboundp 'emms-lyrics-sync-waveform--insert-at-point))
      (emms-lyrics-sync-waveform--insert-at-point
       fp pos-s duration px-w px-h))
     ;; ── Unicode flat bar fallback ─────────────────────────────────────────
     ((fboundp 'emms-lyrics-sync-waveform--render-unicode-flat)
      (insert "\n")
      (setq emms-lyrics-sync-display--waveform-marker
            (copy-marker (point) nil))
      (insert (emms-lyrics-sync-waveform--render-unicode-flat
               win-chars pos-s duration))
      (insert "\n")))))

;;; ── Waveform: Cursor Update ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--update-waveform-cursor (pos-ms)
  "Replace only the waveform bar with an updated playback cursor at POS-MS.

CACHE GUARD: uses (consp cached) not (listp cached).
  (listp nil) → t — would crash on (plist-get nil :px-w).
  (consp nil) → nil — correctly skips when cache is empty."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker))
    (let* ((track    emms-lyrics-sync-core--current-track)
           (fp       (and track (emms-lyrics-sync-track-file-path track)))
           (duration (and track (emms-lyrics-sync-track-duration track)))
           ;; MUST be consp — (listp nil) → t would pass a nil cache hit
           (cached   (and fp
                          (boundp 'emms-lyrics-sync-waveform--png-cache)
                          (gethash fp emms-lyrics-sync-waveform--png-cache))))
      ;; consp: only a non-empty plist passes — nil and 'pending both excluded
      (when (and fp (consp cached))
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
            (when (and (integerp wf-start)
                       (<= wf-start (point-max)))
              (delete-region wf-start (point-max))
              (goto-char wf-start)
              (cond
               ;; PNG dimensions match — fast path (no ffmpeg)
               ((and (fboundp 'emms-lyrics-sync-waveform--use-png-p)
                     (emms-lyrics-sync-waveform--use-png-p)
                     (= (plist-get cached :px-w) px-w)
                     (= (plist-get cached :px-h) px-h)
                     (file-exists-p (plist-get cached :played))
                     (file-exists-p (plist-get cached :remaining))
                     (fboundp 'emms-lyrics-sync-waveform--insert-composite))
                (emms-lyrics-sync-waveform--insert-composite
                 (plist-get cached :played)
                 (plist-get cached :remaining)
                 px-w px-h pos-s duration)
                (insert "\n"))
               ;; Window resized or PNG unavailable — full re-insert
               ((and fp (fboundp 'emms-lyrics-sync-waveform--insert-at-point))
                (emms-lyrics-sync-waveform--insert-at-point
                 fp pos-s duration px-w px-h))
               ;; Terminal fallback
               ((fboundp 'emms-lyrics-sync-waveform--render-unicode-flat)
                (insert (emms-lyrics-sync-waveform--render-unicode-flat
                         win-chars pos-s duration))
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
        (emms-lyrics-sync-display--reset-word-overlay)
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

        ;; ── Header ────────────────────────────────────────────────────────
        (when track
          (let ((erange (emms-lyrics-sync-display--render-header
                         track (/ pos-ms 1000.0))))
            (setq emms-lyrics-sync-display--elapsed-marker
                  (copy-marker (car erange) nil)
                  emms-lyrics-sync-display--elapsed-end-marker
                  (copy-marker (cdr erange) t))))
        (insert "\n")

        ;; ── Lyrics marker ─────────────────────────────────────────────────
        (setq emms-lyrics-sync-display--lyrics-marker
              (copy-marker (point) nil))

        ;; ── Lyrics ────────────────────────────────────────────────────────
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)

        ;; ── Waveform ──────────────────────────────────────────────────────
        (when track
          (let ((fp  (emms-lyrics-sync-track-file-path track))
                (dur (emms-lyrics-sync-track-duration   track)))
            (if (and fp
                     (fboundp 'emms-lyrics-sync-waveform--use-png-p)
                     (emms-lyrics-sync-waveform--use-png-p)
                     (fboundp 'emms-lyrics-sync-waveform--insert-at-point))
                (let* ((char-w    (frame-char-width))
                       (char-h    (frame-char-height))
                       (win-chars (max 4 (emms-lyrics-sync-display--body-width)))
                       (px-w      (* win-chars char-w))
                       (px-h      (* (if (boundp 'emms-lyrics-sync-waveform-height)
                                         emms-lyrics-sync-waveform-height 2)
                                     char-h))
                       (pos-s     (/ (float pos-ms) 1000.0)))
                  (emms-lyrics-sync-waveform--insert-at-point
                   fp pos-s dur px-w px-h))
              ;; Terminal / no ffmpeg fallback
              (when (fboundp 'emms-lyrics-sync-waveform-insert)
                (emms-lyrics-sync-waveform-insert
                 (and track (emms-lyrics-sync-track-file-path track))
                 (/ pos-ms 1000.0)
                 (and track (emms-lyrics-sync-track-duration track)))))))

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
        (when (and (integerp lm-pos) (<= lm-pos (point-max)))
          (emms-lyrics-sync-display--clear-sung-overlays)
          (emms-lyrics-sync-display--reset-word-overlay)
          (setq emms-lyrics-sync-display--word-positions nil)
          (delete-region lm-pos (point-max))
          (goto-char lm-pos)
          (emms-lyrics-sync-display--render-lyrics doc pos-ms)
          (emms-lyrics-sync-display--render-waveform-cached pos-ms))))))

(provide 'emms-lyrics-sync-display-redraw)
;;; emms-lyrics-sync-display-redraw.el ends here
