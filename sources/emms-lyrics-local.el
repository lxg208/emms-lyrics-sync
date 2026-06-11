;;; emms-lyrics-local.el --- Local file and embedded tag sources  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Two local sources:
;;
;; `emms-lyrics-source-tag'
;;   Reads embedded lyrics from audio file tags:
;;   - ID3v2 USLT frame (MP3)
;;   - Vorbis comment LYRICS= (FLAC, Ogg)
;;   Uses ffprobe to extract the tag value.
;;
;; `emms-lyrics-source-local-lrc'
;;   Looks for a .lrc sidecar file in the same directory as the track,
;;   with the same base name (file-name-base of file-path).

;;; Code:

(require 'emms-lyrics-core)

(defun emms-lyrics-source-tag (artist title album duration)
  "Return embedded lyrics from the current track's audio tags, or nil."
  ;; TODO: use ffprobe to extract USLT / LYRICS= tag
  ;; ffprobe -v quiet -print_format json -show_format FILE
  nil)

(defun emms-lyrics-source-local-lrc (artist title album duration)
  "Return lyrics from a .lrc sidecar file, or nil if not found.
Looks for FILE-STEM.lrc in the same directory as the current track."
  ;; TODO: derive path from (emms-track-get track 'name)
  ;; (concat (file-name-sans-extension file-path) ".lrc")
  nil)

(provide 'emms-lyrics-local)
;;; emms-lyrics-local.el ends here
