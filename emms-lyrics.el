;;; emms-lyrics.el --- Synchronized lyrics display for EMMS  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: lxg208
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (emms "0.0") (plz "0.7"))
;; Keywords: multimedia, lyrics, emms
;; URL: https://github.com/lxg208/emms-lyrics

;;; Commentary:

;; Entry point for emms-lyrics.  Provides the global minor mode
;; `emms-lyrics-mode' and user-facing interactive commands.
;;
;; See the design document and README for the full feature plan.
;;
;; NOTE: This package is a work in progress and is not yet functional.

;;; Code:

(require 'emms-lyrics-core)
(require 'emms-lyrics-display)
(require 'emms-lyrics-search)

(defgroup emms-lyrics nil
  "Synchronized lyrics display for EMMS."
  :group 'emms
  :prefix "emms-lyrics-")

;;;###autoload
(define-minor-mode emms-lyrics-mode
  "Global minor mode that displays synchronized lyrics for EMMS playback."
  :global t
  :lighter " Lyrics"
  :group 'emms-lyrics
  (if emms-lyrics-mode
      (emms-lyrics--enable)
    (emms-lyrics--disable)))

(defun emms-lyrics--enable ()
  "Set up EMMS hooks and start the lyrics pipeline."
  ;; TODO: add hooks to emms-player-started-hook,
  ;;       emms-player-stopped-hook, emms-player-finished-hook
  (message "emms-lyrics enabled (not yet implemented)"))

(defun emms-lyrics--disable ()
  "Remove EMMS hooks and tear down the lyrics display."
  ;; TODO: remove hooks, close display buffer
  (message "emms-lyrics disabled"))

;;;###autoload
(defun emms-lyrics-search ()
  "Manually search for lyrics for the current EMMS track.
Opens a search UI pre-filled with track metadata."
  (interactive)
  ;; TODO: delegate to emms-lyrics-search.el
  (message "emms-lyrics-search: not yet implemented"))

;;;###autoload
(defun emms-lyrics-next-source ()
  "Cycle to the next lyrics candidate without reopening the search UI."
  (interactive)
  ;; TODO: implement candidate cycling
  (message "emms-lyrics-next-source: not yet implemented"))

(provide 'emms-lyrics)
;;; emms-lyrics.el ends here
