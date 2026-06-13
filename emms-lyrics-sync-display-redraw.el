;;; emms-lyrics-sync-display-redraw.el --- Full and partial buffer redraw  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-redraw.el
;; Created : 2026-06-13 16:00 UTC
;; Updated : 2026-06-13 18:40 UTC
;; Changes :
;;   • --update-waveform-cursor: fast path calls --insert-composite (no \n)
;;     then adds \n — CORRECT.  Fallback no longer calls --insert-at-point
;;     (which would produce a double \n because the separator \n before the
;;     waveform-marker is preserved by delete-region).  Fallback now inserts
;;     unicode flat bar + \n instead.
;;   • All cache guards use (consp cached) not (listp cached).
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

Invariant: delegates entirely to `emms-lyrics-sync-waveform--insert-at-point'
which inserts: separator \\n → waveform-marker (NIL) → content → trailing \\n."
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
     ;; ── Unicode flat bar fallback (terminal) ──────────────────────────────
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

Marker invariant:
  waveform-marker is NIL-type — it stays at the bar start.
  delete-region removes from wf-start to (point-max), preserving the
  separator \\n that sits BEFORE wf-start.  We then insert new content
  followed by \\n.  We must NOT call --insert-at-point here because that
  function inserts its own separator \\n which would produce a double \\n.

Cache guard: uses (consp cached) not (listp cached).
  (listp nil) → t — nil passes, (plist-get nil :px-w) → nil → crash.
  (consp nil) → nil — correctly excluded."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker))
    (let* ((track    emms-lyrics-sync-core--current-track)
           (fp       (and track (emms-lyrics-sync-track-file-path track)))
           (duration (and track (emms-lyrics-sync-track-duration track)))
           ;; consp: nil (cache miss) and 'pending both excluded correctly
           (cached   (and fp
                          (boundp 'emms-lyrics-sync-waveform--png-cache)
                          (gethash fp emms-lyrics-sync-waveform--png-cache))))
      (when (consp cached)
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
               ;; ── PNG fast path: dimensions match, files exist ───────────
               ((and (fboundp 'emms-lyrics-sync-waveform--use-png-p)
                     (emms-lyrics-sync-waveform--use-png-p)
                     (= (plist-get cached :px-w) px-w)
                     (= (plist-get cached :px-h) px-h)
                     (file-exists-p (plist-get cached :played))
                     (file-exists-p (plist-get cached :remaining))
                     (fboundp 'emms-lyrics-sync-waveform--insert-composite))
                ;; --insert-composite inserts image only (no newlines)
                (emms-lyrics-sync-waveform--insert-composite
                 (plist-get cached :played)
                 (plist-get cached :remaining)
                 px-w px-h pos-s duration)
                (insert "\n"))

               ;; ── Window resized or PNG unavailable ─────────────────────
               ;; Do NOT call --insert-at-point here — it adds a separator
               ;; \n which would produce a double \n (the separator before
               ;; waveform-marker is preserved by delete-region above).
               ;; Instead show unicode flat bar; the next full redraw
               ;; (triggered by PNG regeneration callback) will fix it.
               ((fboundp 'emms-lyrics-sync-waveform--render-unicode-flat)
                (insert (emms-lyrics-sync-waveform--render-unicode-flat
                         win-chars pos-s duration))
                (insert "\n"))

               ;; ── Last resort ───────────────────────────────────────────
               (t
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

        ;; ── Metadata header ───────────────────────────────────────────────
        (when track
          (let ((erange (emms-lyrics-sync-display--render-header
                         track (/ pos-ms 1000.0))))
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
        (emms-lyrics-sync-display--render-waveform-cached pos-ms)

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
