;;; emms-lyrics-sync-display-redraw.el --- Full and partial buffer redraw  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-redraw.el
;; Created : 2026-06-13 16:00 UTC
;; Updated : 2026-06-13 18:40 UTC
;; Changes :
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

	;; ── CJK font text properties (header + lyrics) ────────────────────
        (emms-lyrics-sync-display--apply-cjk-font (point-min) (point-max))

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
      (save-excursion
      (let* ((inhibit-read-only t)
             (lm-pos (marker-position emms-lyrics-sync-display--lyrics-marker)))
        (when (and (integerp lm-pos) (<= lm-pos (point-max)))
          (emms-lyrics-sync-display--clear-sung-overlays)
          (emms-lyrics-sync-display--reset-word-overlay)
          (setq emms-lyrics-sync-display--word-positions nil)
          (delete-region lm-pos (point-max))
          (goto-char lm-pos)
          (emms-lyrics-sync-display--render-lyrics doc pos-ms)
	  ;; ── CJK font text properties for the reinserted lyrics ─────────
          (emms-lyrics-sync-display--apply-cjk-font lm-pos (point-max))))))))

;;; ── Waveform buffer renderer ─────────────────────────────────────────────────

(defun emms-lyrics-sync-display--redraw-waveform-window (pos-ms)
  "Erase and repaint the waveform buffer with a fresh composite at POS-MS.

Unlike the previous in-buffer marker-and-partial-redraw approach,
this function operates on a dedicated buffer that contains nothing
but the waveform image.  No coordination with cover, header, or
lyric content is required: the buffer is erased and repainted in
one pass.

Reuses the existing `emms-lyrics-sync-waveform--composite-at-point'
and `--ensure-pngs' machinery.  On first track play, when the PNGs
do not yet exist, `--ensure-pngs' starts async generation and the
callback re-invokes this function when they are ready.

Updates the cached mode line string (see
`emms-lyrics-sync-waveform--cached-mode-line') so the mode line
persists correctly after playback stops."
  (when (and (buffer-live-p emms-lyrics-sync-waveform-buffer)
             (fboundp 'emms-lyrics-sync-waveform--composite-at-point))
    (with-current-buffer emms-lyrics-sync-waveform-buffer
      (let* ((inhibit-read-only t)
             (track     emms-lyrics-sync-core--current-track)
             (dur       (and track (emms-lyrics-sync-track-duration track)))
             (fp        (and track (emms-lyrics-sync-track-file-path track)))
             (win       (get-buffer-window (current-buffer) t))
             (char-w    (frame-char-width))
             (char-h    (frame-char-height))
	     ;; win-chars now subtracts the padding on both sides:
	     (win-chars (if win
			    (max 4 (- (window-body-width win)
				      (* 2 emms-lyrics-sync-waveform-side-padding)))
			  60))
	     ;; px-w stays as win-chars × char-w (same as before)
             (px-w      (let ((w (* win-chars char-w)))
                          (if (oddp w) (1+ w) w)))
	     (px-h      (let* ((raw (* emms-lyrics-sync-waveform-height char-h))
			       (rem (mod raw 4)))
			  (if (zerop rem) raw (+ raw (- 4 rem)))))
             (pos-s     (/ (float pos-ms) 1000.0)))
        (setq-local emms-lyrics-sync-waveform--cached-mode-line
                    (emms-lyrics-sync-display--format-waveform-mode-line
                     pos-ms track))
        (erase-buffer)
	;; NEW: left padding; right padding is implicit because win-chars
	;; was reduced by 2× padding, so the image doesn't reach the body edge.
	(when (and win (> emms-lyrics-sync-waveform-side-padding 0))
	  (insert (make-string emms-lyrics-sync-waveform-side-padding ?\s)))
        (cond
         ;; ── PNG mode ─────────────────────────────────────────────────────
         ((and fp (emms-lyrics-sync-waveform--use-png-p))
          (let ((cached (gethash fp emms-lyrics-sync-waveform--png-cache)))
            (cond
             ((and (consp cached)
                   (= (plist-get cached :px-w) px-w)
                   (= (plist-get cached :px-h) px-h)
                   (file-exists-p (plist-get cached :played))
                   (file-exists-p (plist-get cached :remaining)))
              (let ((cursor-x (if (and dur (> dur 0))
                                  (max 1 (min (1- px-w)
                                              (round (* (/ pos-s (float dur))
                                                        px-w))))
                                0)))
                (emms-lyrics-sync-waveform--composite-at-point
                 (plist-get cached :played)
                 (plist-get cached :remaining)
                 cursor-x px-h pos-s dur)))
             (t
              (insert (emms-lyrics-sync-waveform--render-unicode-flat
                       win-chars pos-s dur))
              (unless (eq cached 'pending)
                (emms-lyrics-sync-waveform--ensure-pngs
                 fp px-w px-h
                 (lambda (plist)
                   (when (and plist
                              (buffer-live-p emms-lyrics-sync-waveform-buffer))
                     (with-current-buffer emms-lyrics-sync-waveform-buffer
                       (emms-lyrics-sync-display--redraw-waveform-window
                        (or (emms-lyrics-sync-core--playback-position-ms)
                            0)))))))))))
         ;; ── Unicode fallback (terminal or no ffmpeg) ─────────────────────
         (t
          (insert (emms-lyrics-sync-waveform--render-unicode-flat
                   win-chars pos-s dur))))
        (force-mode-line-update)))))

(provide 'emms-lyrics-sync-display-redraw)
;;; emms-lyrics-sync-display-redraw.el ends here
