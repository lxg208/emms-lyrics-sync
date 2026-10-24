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
  "Render the lyrics buffer for the current track."
  (let* ((result emms-lyrics-sync-core--current-result)
         (doc    (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (buf    (emms-lyrics-sync-display--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-display--clear-sung-overlays)
        (emms-lyrics-sync-display--reset-word-overlay)
        (setq emms-lyrics-sync-display--word-positions nil)
        (setq emms-lyrics-sync-display--lyrics-marker
              (copy-marker (point) nil))
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)
        (emms-lyrics-sync-display--apply-cjk-font (point-min) (point-max))
        (goto-char (point-min))
        (setq emms-lyrics-sync-display--last-line-idx
              (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))))))

;;; ── Partial Redraw: Lyrics + Waveform ────────────────────────────────────────

(defun emms-lyrics-sync-display--redraw-lyrics-only (doc pos-ms)
  "Replace lyrics without touching cover or header."
  (when (and (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let* ((inhibit-read-only t)
             (lm-pos (marker-position emms-lyrics-sync-display--lyrics-marker))
             (cur-idx (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))) ; <-- Added this
        (when (and (integerp lm-pos) (<= lm-pos (point-max)))
          (emms-lyrics-sync-display--clear-sung-overlays)
          (emms-lyrics-sync-display--reset-word-overlay)
          (setq emms-lyrics-sync-display--word-positions nil)
          (delete-region lm-pos (point-max))
          (goto-char lm-pos)
          (emms-lyrics-sync-display--render-lyrics doc pos-ms)
          (emms-lyrics-sync-display--apply-cjk-font lm-pos (point-max))
          (when (>= cur-idx 0)
            (emms-lyrics-sync-display--auto-scroll cur-idx)))))))

;;; ── Waveform buffer renderer ─────────────────────────────────────────────────

(defun emms-lyrics-sync-display--insert-flat-line (win-chars pos-s dur)
  "Insert a vertically centered flat line placeholder."
  (let ((above (max 0 (/ (1- emms-lyrics-sync-waveform-height) 2))))
    (when (> above 0)
      (insert (make-string above ?\n)))
    (when (and (get-buffer-window (current-buffer) t)
               (> (or emms-lyrics-sync-waveform-side-padding 0) 0))
      (insert (make-string emms-lyrics-sync-waveform-side-padding ?\s)))
    (insert (emms-lyrics-sync-waveform--render-unicode-flat
             win-chars pos-s dur))))

(defun emms-lyrics-sync-display--redraw-waveform-window (pos-ms)
  "Erase and repaint the waveform buffer with a fresh composite at POS-MS."
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
             (win-chars (if win
                            (max 4 (- (window-body-width win)
                                      (* 2 emms-lyrics-sync-waveform-side-padding)))
                          60))
             (px-w      (let ((w (* win-chars char-w)))
                          (if (oddp w) (1+ w) w)))
             ;; Floor to a multiple of 4 so the image never overflows the body
             (px-h      (let* ((raw (* emms-lyrics-sync-waveform-height char-h))
                               (rem (mod raw 4)))
                          (- raw rem)))
             (pos-s     (/ (float pos-ms) 1000.0)))
        (setq-local emms-lyrics-sync-waveform--cached-mode-line
                    (emms-lyrics-sync-display--format-waveform-mode-line
                     pos-ms track))
        (erase-buffer)
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
              ;; Success: pad left, then insert image
              (when (and win (> emms-lyrics-sync-waveform-side-padding 0))
                (insert (make-string emms-lyrics-sync-waveform-side-padding ?\s)))

	      ;; Record the exact start of the image for click calculations
	      (setq-local emms-lyrics-sync-waveform--img-start (copy-marker (point) nil))
              (setq-local emms-lyrics-sync-waveform--last-px-w px-w)
	      
              (let ((cursor-x (if (and dur (> dur 0))
                                  (max 1 (min (1- px-w)
                                              (round (* (/ pos-s (float dur))
                                                        px-w))))
                                0)))
                (emms-lyrics-sync-waveform--composite-at-point
                 (plist-get cached :played)
                 (plist-get cached :remaining)
                 cursor-x px-h pos-s dur)
		;; Attach the click map to the image
                (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] #'emms-lyrics-sync-display--waveform-click)
                  (put-text-property (marker-position emms-lyrics-sync-waveform--img-start)
                                     (point)
                                     'keymap map))))
             (t
              ;; Pending: centered flat line
              (emms-lyrics-sync-display--insert-flat-line win-chars pos-s dur)
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
          (emms-lyrics-sync-display--insert-flat-line win-chars pos-s dur))))
        (force-mode-line-update))))

(defun emms-lyrics-sync-display--render-metadata-buffer (track pos-ms)
  "Render cover + header for TRACK into the metadata buffer at POS-MS."
  (when (buffer-live-p emms-lyrics-sync-metadata-buffer)
    (with-current-buffer emms-lyrics-sync-metadata-buffer
      (let* ((inhibit-read-only t)
             (char-w (frame-char-width))
             (body-w (if-let ((win (get-buffer-window (current-buffer) t)))
                         (window-body-width win)
                       65))
             (win-px (* body-w char-w)))
        (erase-buffer)
        (when track
          (let* ((h   (emms-lyrics-sync-display--cover-pixel-height))
                 (img (emms-lyrics-sync-display--cover-or-placeholder track h)))
            (when img
              (let* ((img-w (or (car (image-size img t)) 0))
                     (pad   (max 0 (floor (/ (- win-px img-w) 2.0 char-w)))))
                (insert (make-string pad ?\s))
                (insert-image img)
                (insert "\n\n"))))
          (emms-lyrics-sync-display--render-header track)
          (emms-lyrics-sync-display--apply-cjk-font (point-min) (point-max))
          (goto-char (point-min)))))))

(defun emms-lyrics-sync-display--auto-scroll (idx)
  "Scroll the lyrics window so line IDX is positioned 3 lines from the top."
  (when (and (fboundp 'emms-lyrics-sync-display--window)
             (emms-lyrics-sync-display--window)
             (vectorp emms-lyrics-sync-display--line-markers)
             (< idx (length emms-lyrics-sync-display--line-markers)))
    (let* ((win (emms-lyrics-sync-display--window))
           (marker (aref emms-lyrics-sync-display--line-markers idx)))
      (when (and marker (marker-buffer marker))
        (with-current-buffer emms-lyrics-sync-display--buffer
          (goto-char (marker-position marker))
          (forward-line -3)
          (set-window-start win (point)))))))

(provide 'emms-lyrics-sync-display-redraw)
;;; emms-lyrics-sync-display-redraw.el ends here
