;;; emms-lyrics-sync-display-vars.el --- Customization, faces, state variables and utilities  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-vars.el
;; Created : 2026-06-13 05:35 UTC
;; Updated : 2026-06-13 19:50 UTC
;; Changes : Bug 1 color fix — tech-face and new duration-face are now
;;           distinct from past-line-face (font-lock-comment-face grey).
;;
;;           Old: both used font-lock-comment-face → same grey as past lyrics
;;           New:
;;             tech-face     → #4db6ac  Material Teal 300
;;                             reads as "cool/technical metadata"
;;             duration-face → #80cbc4  Material Teal 200 (lighter)
;;                             complements tech-face and the teal elapsed face
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

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
with the default values 3 + 6 + 3 = 12."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-update-interval 0.1
  "Seconds between timer ticks.  Controls karaoke word-highlight smoothness."
  :type  'number
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────
;;
;; IMPORTANT: Do NOT add :weight bold or :height to faces used in header
;; lines centered via string-width.  Both scale character pixel width in
;; GUI Emacs, causing text to render wider than string-width reports.
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
  '((((background dark))  :foreground "#4db6ac")
    (((background light)) :foreground "#00796b"))
  "Face for the CODEC | bits/kHz | kbps | channels info line (line 4).
Material Teal 300 — clearly distinct from past-line-face (comment grey),
reads as cool/technical metadata without competing with the amber current line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-elapsed-face
  '((t :inherit font-lock-constant-face))
  "Face for the elapsed-time portion of the time line (e.g. \"0:18\")."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-duration-face
  '((((background dark))  :foreground "#80cbc4")
    (((background light)) :foreground "#00897b"))
  "Face for the duration portion of the time line (e.g. \"/ 4:55\").
Material Teal 200 — lighter than tech-face, complements the teal elapsed
face while remaining clearly distinct from the grey past-line-face."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-past-line-face
  '((t :inherit font-lock-comment-face))
  "Face for lyrics lines that have already played."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-current-line-face
  '((((background dark))  :foreground "#ffcb6b")
    (((background light)) :foreground "#b36a00"))
  "Face for the currently playing lyrics line.
Warm amber — no bold weight so string-width centering stays accurate."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-future-line-face
  '((t :inherit default))
  "Face for upcoming lyrics lines."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-sung-face
  '((((background dark))  :foreground "#c3e88d")
    (((background light)) :foreground "#2d7a2d"))
  "Face for already-sung words within the current A2 karaoke line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-current-face
  '((((background dark))  :foreground "#ffdd00" :underline t)
    (((background light)) :foreground "#cc6600" :underline t))
  "Face for the word currently being sung (A2 karaoke highlight)."
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display--buffer nil
  "The lyrics display buffer.")

(defvar emms-lyrics-sync-display--timer nil
  "The 100ms display-update timer.")

(defvar emms-lyrics-sync-display--last-line-idx -1
  "Line index last used for a lyrics redraw.")

(defvar emms-lyrics-sync-display--word-overlay nil
  "Overlay used for A2 current-word highlighting.")

(defvar emms-lyrics-sync-display--sung-overlays nil
  "List of overlays for already-sung words in the current A2 line.")

(defvar emms-lyrics-sync-display--elapsed-marker nil
  "NIL-type marker at the START of the elapsed-time text in the header.")

(defvar emms-lyrics-sync-display--elapsed-end-marker nil
  "T-type marker at the END of the elapsed-time text in the header.")

(defvar emms-lyrics-sync-display--lyrics-marker nil
  "NIL-type marker at the start of the lyrics section.")

(defvar emms-lyrics-sync-display--waveform-marker nil
  "NIL-type marker at the start of the waveform bar content.")

(defvar emms-lyrics-sync-display--current-file-path nil
  "File path of the track currently rendered in the lyrics buffer.")

(defvar emms-lyrics-sync-display--tick-counter 0
  "Counter incremented every display tick.")

(defvar emms-lyrics-sync-display--word-positions nil
  "Vector of (buf-start buf-end emms-lyrics-sync-word) triples.")

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
  "Return the usable body width of the lyrics window in characters.
Subtracts 2 for the left and right margin widths set in display.el."
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
