;;; emms-lyrics-sync-display-vars.el --- Customization, faces, state variables and utilities  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-vars.el
;; Created : 2026-06-13 05:35 UTC
;; Purpose : Shared definitions for the emms-lyrics-sync display subsystem.
;;           Declares all defcustom options, defface faces, and defvar state
;;           variables used across the display subsystem.  Also provides four
;;           small stateless utility functions (time formatting, window lookup,
;;           body-width, text centering) required by all sibling modules.
;;
;;           Face design notes:
;;             • Header faces (artist, album, title) use colour only — no
;;               :weight bold or :height scaling.  Scaling changes character
;;               pixel width, which breaks string-width–based centering in
;;               GUI Emacs.  Colour provides sufficient visual hierarchy.
;;             • Current-line face uses #ffcb6b (warm amber) instead of bold.
;;             • A2 karaoke faces: sung words = #c3e88d (sage green),
;;               current word = #ffdd00 (yellow) with underline.
;;               Progression: grey past → green sung → yellow current → white future.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Load order within the display subsystem:
;;   1. emms-lyrics-sync-display-vars.el   ← this file
;;   2. emms-lyrics-sync-display-render.el ← all buffer-insert render fns
;;   3. emms-lyrics-sync-display-redraw.el ← redraw orchestration
;;   4. emms-lyrics-sync-display.el        ← timer, hooks (entry point)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-display-buffer-name "*emms-lyrics*"
  "Name of the emms-lyrics-sync display buffer."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-side 'right
  "Side of the frame where the lyrics side window appears."
  :type  '(choice (const right) (const left))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-width 52
  "Width in columns of the lyrics side window."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cover-height 180
  "Pixel height for cover art in GUI Emacs.  Set to 0 to disable."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cover-filenames
  '("cover.jpg"  "cover.jpeg"  "cover.png"
    "front.jpg"  "front.jpeg"  "front.png"
    "folder.jpg" "folder.jpeg" "folder.png"
    "album.jpg"  "album.jpeg"  "album.png")
  "Filenames tried in order when looking for a sidecar cover image."
  :type  '(repeat string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-context-before 3
  "Number of past lyrics lines visible above the current line."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-context-after 6
  "Number of upcoming lyrics lines visible below the current line."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-lyrics-height 12
  "Total number of physical screen lines reserved for the lyrics area.
The lyrics section always occupies exactly this many lines regardless of
line wrapping or playback position.  The value should satisfy:
  context-before + context-after + 3 = lyrics-height
with the default values 3 + 6 + 3 = 12.

Increase on larger displays or when using a smaller font.  The value
can be changed at any time with setq; it takes effect on the next
full redraw (track change)."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-update-interval 0.1
  "Seconds between timer ticks.  Controls karaoke word-highlight smoothness."
  :type  'number
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────
;;
;; IMPORTANT: Do NOT add :weight bold or :height to any face used in
;; header lines that are centered via string-width.  Both properties
;; scale character pixel width in GUI Emacs, causing the text to render
;; wider than string-width reports, which shifts text right of centre.
;; Use colour alone for visual emphasis.

(defface emms-lyrics-sync-artist-face
  '((t :inherit font-lock-function-name-face))
  "Face for the Artist[ - Composer] header line (line 1)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-album-face
  '((t :inherit font-lock-keyword-face))
  "Face for the Album header line (line 2, shown only when album is available)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-title-face
  '((t :inherit font-lock-string-face))
  "Face for the [Track. ]Title header line (line 3)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-tech-face
  '((t :inherit font-lock-comment-face))
  "Face for the CODEC | bits/kHz | kbps | channels info line (line 4)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-elapsed-face
  '((t :inherit font-lock-constant-face))
  "Face for the elapsed-time portion of the time line (line 5)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-past-line-face
  '((t :inherit font-lock-comment-face))
  "Face for lyrics lines that have already played."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-current-line-face
  '((((background dark))  :foreground "#ffcb6b")
    (((background light)) :foreground "#b36a00"))
  "Face for the currently playing lyrics line.
Uses warm amber (#ffcb6b) instead of bold weight so that character width
stays uniform and string-width–based centering remains accurate."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-future-line-face
  '((t :inherit default))
  "Face for upcoming lyrics lines."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-sung-face
  '((((background dark))  :foreground "#c3e88d")
    (((background light)) :foreground "#2d7a2d"))
  "Face for already-sung words within the current A2 karaoke line.
Sage green (#c3e88d) creates the progression: grey past → green sung
→ yellow current → white future."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-current-face
  '((((background dark))  :foreground "#ffdd00" :underline t)
    (((background light)) :foreground "#cc6600" :underline t))
  "Face for the word currently being sung (A2 karaoke highlight).
Yellow (#ffdd00) with underline marks the active word clearly."
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display--buffer nil
  "The lyrics display buffer.")

(defvar emms-lyrics-sync-display--timer nil
  "The 100ms display-update timer.")

(defvar emms-lyrics-sync-display--last-line-idx -1
  "Line index last used for a lyrics redraw; gates context-window advances.")

(defvar emms-lyrics-sync-display--word-overlay nil
  "Overlay used for A2 current-word highlighting.")

(defvar emms-lyrics-sync-display--sung-overlays nil
  "List of overlays for already-sung words in the current A2 line.")

(defvar emms-lyrics-sync-display--elapsed-marker nil
  "Marker at the START of the elapsed-time text in the header.
Must be created with insertion-type NIL so it stays at the start of the
elapsed text regardless of insertions — used as the start of delete-region
in `emms-lyrics-sync-display--update-elapsed'.")

(defvar emms-lyrics-sync-display--elapsed-end-marker nil
  "Marker at the END of the elapsed-time text in the header.
Must be created with insertion-type T so it advances past newly inserted
elapsed text — used as the end of delete-region in update-elapsed, and
advances to mark the new end after each insert.")

(defvar emms-lyrics-sync-display--lyrics-marker nil
  "Marker at the start of the lyrics section (NIL insertion-type).
`redraw-lyrics-only' deletes from here to point-max and re-inserts.
NIL type keeps this boundary fixed regardless of inserts at the position.")

(defvar emms-lyrics-sync-display--waveform-marker nil
  "Marker at the start of the waveform bar content (NIL insertion-type).
Set after the separator newline, before bar content.  NIL type keeps
the marker anchored so `update-waveform-cursor' always replaces exactly
the bar region without drifting into the lyrics above.")

(defvar emms-lyrics-sync-display--current-file-path nil
  "File path of the track currently rendered in the lyrics buffer.
Used to distinguish new-track (full redraw) from same-track (partial update).
Reset to nil on stop so the next play always triggers a full redraw.")

(defvar emms-lyrics-sync-display--tick-counter 0
  "Counter incremented every display tick.
Throttles waveform cursor updates to ~1/s (every 10 ticks).")

(defvar emms-lyrics-sync-display--word-positions nil
  "Vector of (buf-start buf-end emms-lyrics-sync-word) triples for the
current A2 line.  Populated during lyrics redraw; consumed by the tick.")

(defvar emms-lyrics-sync-display--cover-cache (make-hash-table :test #'equal)
  "Cache: file-path → Emacs image object.")

;;; ── Utilities ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--format-time (seconds)
  "Format SECONDS (number or nil) as \"m:ss\"."
  (if (and (numberp seconds) (>= seconds 0))
      (let* ((s (round seconds))
             (m (/ s 60)))
        (format "%d:%02d" m (% s 60)))
    "--:--"))

(defun emms-lyrics-sync-display--window ()
  "Return the live window showing the lyrics buffer, or nil."
  (when (buffer-live-p emms-lyrics-sync-display--buffer)
    (get-buffer-window emms-lyrics-sync-display--buffer t)))

(defun emms-lyrics-sync-display--body-width ()
  "Return the usable body width of the lyrics window in characters."
  (let ((win (emms-lyrics-sync-display--window)))
    (if win (- (window-body-width win) 2) 60)))

(defun emms-lyrics-sync-display--center (str)
  "Return STR padded with spaces to center it in the lyrics window."
  (let* ((width (emms-lyrics-sync-display--body-width))
         (len   (string-width str))
         (pad   (max 0 (/ (- width len) 2))))
    (concat (make-string pad ?\s) str)))

(provide 'emms-lyrics-sync-display-vars)
;;; emms-lyrics-sync-display-vars.el ends here
