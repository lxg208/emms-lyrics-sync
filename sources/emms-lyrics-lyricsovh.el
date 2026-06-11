;;; emms-lyrics-lyricsovh.el --- lyrics.ovh source (plain text fallback)  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetches plain-text lyrics from lyrics.ovh (https://lyrics.ovh).
;; Free, no API key required.  Used as a last-resort fallback when
;; no synced LRC source returns a result.
;;
;; API endpoint:
;;   GET https://api.lyrics.ovh/v1/ARTIST/TITLE
;;
;; Returns JSON: { "lyrics": "..." }

;;; Code:

(require 'emms-lyrics-core)
(require 'plz)
(require 'json)

(defconst emms-lyrics-lyricsovh-base-url "https://api.lyrics.ovh/v1"
  "Base URL for the lyrics.ovh API.")

(defun emms-lyrics-source-lyricsovh (artist title album duration)
  "Fetch plain-text lyrics from lyrics.ovh for ARTIST / TITLE.
ALBUM and DURATION are accepted for interface consistency but not used.
Returns an `emms-lyrics-result' or nil."
  ;; TODO: implement async plz GET, URL-encode artist/title
  nil)

(provide 'emms-lyrics-lyricsovh)
;;; emms-lyrics-lyricsovh.el ends here
