;;; emms-lyrics-display.el --- Buffer rendering and scrolling  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders the lyrics buffer:
;;   - Cover art (top center)
;;   - Track metadata header
;;   - Scrolling LRC lines with current-line highlight
;;   - Word-level karaoke highlight (A2 LRC)
;;   - Waveform bar (delegated to emms-lyrics-waveform.el)
;;
;; Display modes:
;;   'buffer      — dedicated *emms-lyrics* buffer (default)
;;   'side-window — displayed in a side window

;;; Code:

(require 'emms-lyrics-core)

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface emms-lyrics-current-line-face
  '((t :inherit bold :foreground "white"))
  "Face for the currently playing lyrics line.")

(defface emms-lyrics-past-line-face
  '((t :foreground "gray50"))
  "Face for lyrics lines already sung.")

(defface emms-lyrics-future-line-face
  '((t :foreground "gray70"))
  "Face for upcoming lyrics lines.")

(defface emms-lyrics-word-sung-face
  '((t :foreground "white" :weight bold))
  "Face for words already sung within the current A2 line.")

(defface emms-lyrics-word-current-face
  '((t :foreground "yellow" :weight bold))
  "Face for the word currently being sung (A2 karaoke).")

(defface emms-lyrics-metadata-face
  '((t :height 1.2 :weight bold))
  "Face for the track metadata header.")

;;; ── User Options ─────────────────────────────────────────────────────────────

(defcustom emms-lyrics-display-mode 'buffer
  "Where to display lyrics.  One of \\='buffer or \\='side-window."
  :type '(choice (const buffer) (const side-window))
  :group 'emms-lyrics)

(defcustom emms-lyrics-buffer-name "*emms-lyrics*"
  "Name of the lyrics display buffer."
  :type 'string
  :group 'emms-lyrics)

;;; ── Buffer Management ────────────────────────────────────────────────────────

(defun emms-lyrics-display-get-buffer ()
  "Return the lyrics display buffer, creating it if necessary."
  (get-buffer-create emms-lyrics-buffer-name))

(defun emms-lyrics-display-render (result track)
  "Render lyrics RESULT and TRACK metadata into the display buffer."
  ;; TODO: implement full layout:
  ;;   cover art → metadata header → lyrics lines → waveform bar
  (with-current-buffer (emms-lyrics-display-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "[emms-lyrics] Rendering not yet implemented.\nTrack: %S\n"
                      track)))))

(defun emms-lyrics-display-update-position (position-ms)
  "Scroll and highlight the lyrics buffer to POSITION-MS."
  ;; TODO: implement position-based scroll + highlight
  nil)

(provide 'emms-lyrics-display)
;;; emms-lyrics-display.el ends here
