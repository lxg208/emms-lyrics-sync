;;; emms-lyrics-sync-display-redraw.el --- Full and partial buffer redraw  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-redraw.el
;; Created : 2026-06-12 21:54 UTC
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
;;               lyrics → waveform.  Sets both lyrics-marker and
;;               waveform-marker with insertion-type NIL.
;;
;;             `emms-lyrics-sync-display--redraw-lyrics-only'
;;               Replaces only the lyrics+waveform region.  Cover and
;;               header are preserved.  Uses lyrics-marker (NIL type)
;;               as the cut boundary.
;;
;;           Marker invariants — CRITICAL, do not break:
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
;;           Both markers MUST use NIL insertion-type.  T (advancing)
;;           type caused the accumulation bug: markers drifted past
;;           inserted content, so delete-region deleted nothing and each
;;           redraw appended another copy.
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
;; get-buffer is defined in display.el (loaded after this file).
;; Declared here so the byte-compiler does not warn.
(declare-function emms-lyrics-sync-display--get-buffer
                  "emms-lyrics-sync-display")

;;; ── Waveform: Cached Render ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-waveform-cached (pos-ms)
  "Insert waveform at point using only cached data; always sets waveform-marker.

Does NOT trigger ffmpeg extraction.  Uses real data when cached, a neutral
placeholder (0.5 amplitude, both channels) otherwise, so waveform-marker
is always valid and pointing at real content after this call.

Invariant: inserts separator \\n FIRST, then sets waveform-marker with
insertion-type NIL, then inserts bar content.  NIL type keeps the marker
anchored at the start of bar content even after insert-image / insert add
content at that position — critical for update-waveform-cursor."
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
                          emms-lyrics-sync-waveform-height 4)
                      char-h))
         (pos-s    (/ (float pos-ms) 1000.0))
         ;; Placeholder: vector of [l r] pairs, both 0.5
         (render-data
          (if (vectorp data)
              data
            (let ((v (make-vector win-chars nil)))
              (dotimes (i win-chars) (aset v i (vector 0.5 0.5)))
              v))))
    ;; Separator — waveform-marker goes AFTER this newline
    (insert "\n")
    ;; NIL insertion-type: marker stays here even when content inserted at point
    (setq emms-lyrics-sync-display--waveform-marker
          (copy-marker (point) nil))
    ;; Render
    (cond
     ;; SVG path (GUI + rsvg/svg support)
     ((and (fboundp 'emms-lyrics-sync-waveform--use-svg-p)
           (emms-lyrics-sync-waveform--use-svg-p)
           (fboundp 'emms-lyrics-sync-waveform--render-svg))
      (let ((img (emms-lyrics-sync-waveform--render-svg
                  render-data px-w px-h pos-s duration)))
        (when img (insert-image img)))
      (insert "\n"))
     ;; Unicode sparkline fallback
     ((fboundp 'emms-lyrics-sync-waveform--render-unicode)
      (insert (emms-lyrics-sync-waveform--render-unicode
               render-data win-chars pos-s duration))
      (insert "\n")))))

;;; ── Waveform: Cursor Update ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--update-waveform-cursor (pos-ms)
  "Replace only the waveform bar with an updated playback cursor at POS-MS.

Uses waveform-marker (insertion-type NIL) as the deletion boundary.
NIL type means the marker stays fixed at the same position regardless of
what was previously inserted there, so delete-region always removes
exactly the bar content and the replacement is inserted at the same spot.

No-op when:
  - Display buffer is not live
  - waveform-marker is invalid or its buffer was killed
  - No real extracted data is cached (avoids clobbering the placeholder
    bar with an identical one on every throttled tick)"
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker))
    (let* ((track    emms-lyrics-sync-core--current-track)
           (fp       (and track (emms-lyrics-sync-track-file-path track)))
           (duration (and track (emms-lyrics-sync-track-duration track)))
           (data     (and fp
                          (boundp 'emms-lyrics-sync-waveform--cache)
                          (gethash fp emms-lyrics-sync-waveform--cache))))
      ;; Only update when real extracted data is present
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
                                   emms-lyrics-sync-waveform-height 4)
                               char-h))
                 (pos-s     (/ (float pos-ms) 1000.0)))
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
              (insert "\n")))))))))

;;; ── Full Buffer Redraw ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--full-redraw ()
  "Erase and repopulate the entire lyrics buffer for the current track.

Sequence:
  1. Erase buffer, clear all overlays.
  2. Cover art (centred; loaded from stable cache path, never a temp file).
  3. Metadata header — records elapsed marker pair (NIL insertion-type).
  4. Blank separator line.
  5. lyrics-marker set here with insertion-type NIL.
  6. Lyrics context window.
  7. Waveform — waveform-insert sets waveform-marker (NIL type) and
     kicks off async ffmpeg extraction if the file is not yet cached.
     When extraction finishes the callback calls full-redraw again so
     the real waveform replaces the placeholder.

After this function returns, both lyrics-marker and waveform-marker are
valid, stable, and have NIL insertion-type."
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

        ;; ── 1. Cover art ─────────────────────────────────────────────────
        (when track
          (let* ((fp  (emms-lyrics-sync-track-file-path track))
                 (img (emms-lyrics-sync-display--cover-image fp)))
            (when img
              (let* ((img-w    (or (car (image-size img t)) 0))
                     (char-w   (frame-char-width))
                     (win-px-w (* (emms-lyrics-sync-display--body-width)
                                  char-w))
                     (pad-ch   (max 0
                                    (floor (/ (- win-px-w img-w)
                                              2.0 char-w)))))
                (insert (make-string pad-ch ?\s))
                (insert-image img)
                (insert "\n\n")))))

        ;; ── 2. Metadata header ────────────────────────────────────────────
        (when track
          (let ((erange (emms-lyrics-sync-display--render-header
                         track (/ pos-ms 1000.0))))
            (when (car erange)
              ;; NIL insertion-type so elapsed update never drifts
              (setq emms-lyrics-sync-display--elapsed-marker
                    (copy-marker (car erange) nil)
                    emms-lyrics-sync-display--elapsed-end-marker
                    (copy-marker (cdr erange) nil)))))
        (insert "\n")

        ;; ── 3. Lyrics marker — NIL insertion-type ─────────────────────────
        ;; redraw-lyrics-only relies on this marker staying at the
        ;; header/lyrics boundary regardless of subsequent inserts.
        (setq emms-lyrics-sync-display--lyrics-marker
              (copy-marker (point) nil))

        ;; ── 4. Lyrics ────────────────────────────────────────────────────
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)

        ;; ── 5. Waveform ──────────────────────────────────────────────────
        ;; waveform-insert: sets waveform-marker (NIL), starts async ffmpeg
        ;; extraction if data not yet cached.  Its completion callback calls
        ;; full-redraw again to replace the placeholder with real data.
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
  "Replace lyrics and waveform without touching cover or header.

Uses lyrics-marker (NIL insertion-type) as the cut boundary:
  1. delete-region lyrics-marker → point-max  (removes old lyrics+waveform)
  2. goto-char lyrics-marker
  3. render-lyrics   (re-inserts lyrics at current position)
  4. render-waveform-cached  (re-inserts waveform from cache; sets
                               waveform-marker with NIL type)

Because lyrics-marker has NIL type it does not advance when content is
inserted at its position, so the boundary is always the header/lyrics
line regardless of how many times this function has been called.

Calls render-waveform-cached (not waveform-insert) so no additional
ffmpeg extraction is triggered — only the cursor position is updated."
  (when (and (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)
        ;; Delete old lyrics + waveform.
        ;; Marker stays anchored at boundary (NIL type).
        (delete-region
         (marker-position emms-lyrics-sync-display--lyrics-marker)
         (point-max))
        (goto-char
         (marker-position emms-lyrics-sync-display--lyrics-marker))
        ;; Re-render lyrics
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)
        ;; Re-render waveform from cache; sets waveform-marker (NIL type)
        (emms-lyrics-sync-display--render-waveform-cached pos-ms)))))

(provide 'emms-lyrics-sync-display-redraw)
;;; emms-lyrics-sync-display-redraw.el ends here
