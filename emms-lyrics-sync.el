;;; emms-lyrics.el --- Synchronized lyrics display for EMMS  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync.el
;; Created : 2026-06-11 23:00 UTC
;; Purpose : Package entry point.  Provides the global minor mode
;;           `emms-lyrics-sync-mode' and all user-facing interactive commands.
;;           Loads and wires together all sub-modules.
;;           Hooks into EMMS player start/stop/finish events.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; Version : 0.1.0
;; Package-Requires: ((emacs "28.1") (emms "0.0") (plz "0.7"))
;; Keywords: multimedia, lyrics, emms
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; emms-lyrics-sync adds foobar2000-style synchronized lyrics to EMMS:
;;   - Synced LRC scrolling with context window
;;   - Word-level karaoke highlighting (A2 LRC extension)
;;   - Cover art display (embedded → sidecar → directory slideshow)
;;   - Waveform loudness bar with playback cursor
;;   - Rich metadata header (artist, composer, album, codec, bit-depth, etc.)
;;   - Pluggable source pipeline (local tags, LRCLIB, NetEase, QQ Music, ...)
;;   - Manual search UI (pre-filled, editable, parallel source query)
;;   - mpv IPC for accurate position; falls back to emms-playing-time
;;
;; Quick start:
;;   (use-package emms-lyrics-sync
;;     :after emms
;;     :config (emms-lyrics-sync-mode 1))
;;
;; NOTE: This package is a work in progress.

;;; Code:

(require 'emms)
(require 'emms-playlist-mode)
(require 'emms-playing-time)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display)
(require 'emms-lyrics-sync-search)
(require 'emms-lyrics-sync-cover)
(require 'emms-lyrics-sync-waveform)

;; Source modules — loaded on demand, but require here so they are available
;; when `emms-lyrics-sync-sources' is evaluated.
(require 'emms-lyrics-sync-local)
(require 'emms-lyrics-sync-lrclib)
(require 'emms-lyrics-sync-netease)
(require 'emms-lyrics-sync-qqmusic)
(require 'emms-lyrics-sync-lyricsovh)

;;; ── Keymap ───────────────────────────────────────────────────────────────────
;; FIX: keymap MUST be defined before define-minor-mode, which references it.

(defvar emms-lyrics-sync-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c L s") #'emms-lyrics-sync-search)
    (define-key m (kbd "C-c L n") #'emms-lyrics-sync-next-source)
    (define-key m (kbd "C-c L t") #'emms-lyrics-sync-toggle-window)
    m)
  "Keymap for `emms-lyrics-sync-mode'.")

;;; ── Minor Mode ───────────────────────────────────────────────────────────────

;;;###autoload
(define-minor-mode emms-lyrics-sync-mode
  "Global minor mode for synchronized lyrics display in EMMS.

When enabled:
  - A side window shows cover art, track metadata, synchronized lyrics,
    and a waveform loudness bar for the currently playing track.
  - Lyrics are fetched automatically from local tags, sidecar .lrc files,
    and remote sources (LRCLIB, NetEase, QQ Music, lyrics.ovh).
  - Word-level karaoke highlighting is applied when A2 LRC is available.
  - Playback position is read from mpv IPC when available.

Keybindings (global when mode is active):
  \\[emms-lyrics-sync-search]        — manual lyrics search
  \\[emms-lyrics-sync-next-source]   — cycle to next search result
  \\[emms-lyrics-sync-toggle-window] — show/hide lyrics window"
  :global  t
  :lighter " ♪"
  :group   'emms-lyrics-sync
  :keymap  emms-lyrics-sync-mode-map
  (if emms-lyrics-sync-mode
      (emms-lyrics-sync--enable)
    (emms-lyrics-sync--disable)))

;;; ── Enable / Disable ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync--enable ()
  "Install EMMS hooks and open the lyrics window."
  (add-hook 'emms-player-started-hook  #'emms-lyrics-sync--on-track-start)
  (add-hook 'emms-player-stopped-hook  #'emms-lyrics-sync--on-track-stop)
  (add-hook 'emms-player-finished-hook #'emms-lyrics-sync--on-track-stop)
  (add-hook 'emms-player-paused-hook   #'emms-lyrics-sync--on-track-pause)
  (add-hook 'emms-player-resumed-hook  #'emms-lyrics-sync--on-track-resume)
  ;; If a track is already playing when the mode is enabled, start immediately.
  (when (emms-lyrics-sync--currently-playing-p)
    (emms-lyrics-sync--on-track-start))
  (message "emms-lyrics: enabled"))

(defun emms-lyrics-sync--disable ()
  "Remove EMMS hooks, stop the display timer, and close the lyrics window."
  (remove-hook 'emms-player-started-hook  #'emms-lyrics-sync--on-track-start)
  (remove-hook 'emms-player-stopped-hook  #'emms-lyrics-sync--on-track-stop)
  (remove-hook 'emms-player-finished-hook #'emms-lyrics-sync--on-track-stop)
  (remove-hook 'emms-player-paused-hook   #'emms-lyrics-sync--on-track-pause)
  (remove-hook 'emms-player-resumed-hook  #'emms-lyrics-sync--on-track-resume)
  (emms-lyrics-sync-display-on-stop)
  (emms-lyrics-sync-core--disconnect-mpv)
  (message "emms-lyrics: disabled"))

;;; ── EMMS Hook Handlers ───────────────────────────────────────────────────────

(defun emms-lyrics-sync--currently-playing-p ()
  "Return non-nil if EMMS is currently playing a track."
  (and (bound-and-true-p emms-player-playing-p)
       (condition-case nil
           (emms-playlist-current-selected-track)
         (error nil))))

(defun emms-lyrics-sync--on-track-start ()
  "Called by `emms-player-started-hook'.
Extracts metadata from the current EMMS track and kicks off the fetch pipeline."
  (condition-case err
      (when-let ((emms-track (emms-playlist-current-selected-track)))
        (let ((track (emms-lyrics-sync-core--extract-track emms-track)))
          (emms-lyrics-sync-core--on-track-change track)))
    (error
     (message "emms-lyrics: error on track start: %S" err))))

(defun emms-lyrics-sync--on-track-stop ()
  "Called by `emms-player-stopped-hook' and `emms-player-finished-hook'."
  (condition-case err
      (progn
        (emms-lyrics-sync-core--on-stop)
        (emms-lyrics-sync-display-on-stop))
    (error
     (message "emms-lyrics: error on track stop: %S" err))))

(defun emms-lyrics-sync--on-track-pause ()
  "Called by `emms-player-paused-hook'.
The display continues to show the last position — no action needed."
  ;; The display timer keeps running while paused; it will show the
  ;; frozen position.  Nothing extra to do.
  nil)

(defun emms-lyrics-sync--on-track-resume ()
  "Called by `emms-player-resumed-hook'.
Reconnects mpv IPC if it was dropped while paused."
  (condition-case nil
      (emms-lyrics-sync-core--mpv-connect)
    (error nil)))

;;; ── User-Facing Commands ─────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-search (&optional track)
  "Open the manual lyrics search UI for TRACK (default: current track).
Pre-fills all metadata fields from the track; the user may edit them
before confirming the search with \\[emms-lyrics-sync-search-execute]."
  (interactive)
  (let ((t (or track emms-lyrics-sync-core--current-track)))
    (unless t
      (user-error "No track is currently playing"))
    (emms-lyrics-sync-search-open t)))

;;;###autoload
(defun emms-lyrics-sync-next-source ()
  "Apply the next search result candidate without reopening the search UI.
Cycles through the result list from the last manual search.
Mirrors the OpenLyrics \"next\" keybinding in foobar2000."
  (interactive)
  (unless emms-lyrics-sync-core--current-track
    (user-error "No track is currently playing"))
  (emms-lyrics-sync-search-cycle-next))

;;;###autoload
(defun emms-lyrics-sync-toggle-window ()
  "Show the lyrics window if hidden; hide it if visible."
  (interactive)
  (emms-lyrics-sync-display-toggle))

;;;###autoload
(defun emms-lyrics-sync-reload ()
  "Force-reload lyrics for the current track, bypassing all caches.
Useful after manually editing a .lrc sidecar file."
  (interactive)
  (unless emms-lyrics-sync-core--current-track
    (user-error "No track is currently playing"))
  ;; Evict from in-memory cache
  (when-let ((fp (emms-lyrics-sync-track-file-path emms-lyrics-sync-core--current-track)))
    (remhash fp emms-lyrics-sync-core--cache))
  ;; Re-trigger the pipeline
  (emms-lyrics-sync-core--on-track-change emms-lyrics-sync-core--current-track)
  (message "emms-lyrics: reloading lyrics…"))

;;;###autoload
(defun emms-lyrics-sync-open-sidecar ()
  "Open the .lrc sidecar file for the current track in a buffer.
Creates the file if it does not exist."
  (interactive)
  (unless emms-lyrics-sync-core--current-track
    (user-error "No track is currently playing"))
  (let* ((fp   (emms-lyrics-sync-track-file-path emms-lyrics-sync-core--current-track))
         (lrc  (and fp (concat (file-name-sans-extension
                                (expand-file-name fp))
                               ".lrc"))))
    (unless lrc
      (user-error "Current track has no file path (stream?)"))
    (find-file lrc)))

(provide 'emms-lyrics-sync)
;;; emms-lyrics.el ends here
