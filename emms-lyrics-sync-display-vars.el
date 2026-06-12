;;; emms-lyrics-sync-display-vars.el --- Customization, faces, state variables and utilities  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-vars.el
;; Created : 2026-06-12 15:00 UTC
;; Purpose : Shared definitions for the emms-lyrics-sync display subsystem.
;;           Declares all defcustom options, defface faces, and defvar state
;;           variables used by emms-lyrics-sync-display-render.el and
;;           emms-lyrics-sync-display.el.  Also provides four small stateless
;;           utility functions (time formatting, window lookup, body-width,
;;           text centering) required by both sibling modules.
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
;;   3. emms-lyrics-sync-display.el        ← redraw, timer, hooks (entry point)
;;
;; External consumers should (require 'emms-lyrics-sync-display) only;
;; that file transitively loads this module and the render module.

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

(defcustom emms-lyrics-sync-display-update-interval 0.1
  "Seconds between timer ticks.  Controls karaoke word-highlight smoothness."
  :type  'number
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface emms-lyrics-sync-artist-face
  '((t :height 1.2 :weight bold :inherit font-lock-function-name-face))
  "Face for the Artist [- Composer] - Album header line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-title-face
  '((t :height 1.1 :weight bold :inherit font-lock-string-face))
  "Face for the Track. Title line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-tech-face
  '((t :inherit font-lock-comment-face))
  "Face for the codec / bitrate / elapsed info line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-elapsed-face
  '((t :inherit font-lock-constant-face))
  "Face for the elapsed-time portion of the tech line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-past-line-face
  '((t :inherit font-lock-comment-face))
  "Face for lyrics lines that have already played."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-current-line-face
  '((t :height 1.15 :weight bold :inherit default))
  "Face for the currently playing lyrics line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-future-line-face
  '((t :inherit default))
  "Face for upcoming lyrics lines."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-sung-face
  '((((background dark))  :foreground "#ffffff" :weight bold)
    (((background light)) :foreground "#000000" :weight bold))
  "Face for already-sung words within the current A2 line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-current-face
  '((((background dark))  :foreground "#ffdd00" :weight bold :underline t)
    (((background light)) :foreground "#cc6600" :weight bold :underline t))
  "Face for the word currently being sung (A2 karaoke highlight)."
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
  "Marker at the start of the elapsed-time span in the header.")

(defvar emms-lyrics-sync-display--elapsed-end-marker nil
  "Marker at the end of the elapsed-time span in the header.")

(defvar emms-lyrics-sync-display--lyrics-marker nil
  "Marker at the start of the lyrics section; used for partial redraws.")

(defvar emms-lyrics-sync-display--waveform-marker nil
  "Marker at the start of the waveform bar content (after separator newline).
Set by `emms-lyrics-sync-display--full-redraw' (via waveform-insert) and by
`emms-lyrics-sync-display--render-waveform-cached' (via redraw-lyrics-only).
`emms-lyrics-sync-display--update-waveform-cursor' uses this marker to
replace only the waveform bar on each throttled tick.

CRITICAL: `emms-lyrics-sync-display--redraw-lyrics-only' must always call
`emms-lyrics-sync-display--render-waveform-cached' so this marker is
refreshed after the lyrics region is deleted and re-rendered.  A stale marker
pointing into deleted text causes `update-waveform-cursor' to erase all lyrics
and append a waveform bar in their place.")

(defvar emms-lyrics-sync-display--current-file-path nil
  "File path of the track currently rendered in the lyrics buffer.
Compared against the incoming track path on each
`emms-lyrics-sync-display-on-track-change' call to decide between a full
redraw (new track) and a partial lyrics+waveform update (same track).
Reset to nil by `emms-lyrics-sync-display-on-stop' so the next play —
even of the same file — triggers a full redraw.")

(defvar emms-lyrics-sync-display--tick-counter 0
  "Counter incremented every display tick.
Throttles `emms-lyrics-sync-display--update-waveform-cursor' to run
approximately once per second (every 10 ticks at the default interval).")

(defvar emms-lyrics-sync-display--word-positions nil
  "Vector of (buf-start buf-end emms-lyrics-sync-word) triples for the
current A2 line.  Populated during lyrics redraw; consumed each tick by
`emms-lyrics-sync-display--update-word-overlays'.")

(defvar emms-lyrics-sync-display--cover-cache (make-hash-table :test #'equal)
  "Cache: cover-file-path → Emacs image object.")

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
