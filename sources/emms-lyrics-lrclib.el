;;; emms-lyrics-lrclib.el --- LRCLIB source  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetches synced LRC lyrics from LRCLIB (https://lrclib.net).
;; Free, no API key required.
;;
;; API endpoint:
;;   GET https://lrclib.net/api/get
;;       ?artist_name=ARTIST&track_name=TITLE&album_name=ALBUM&duration=SECS
;;
;; Returns JSON with `syncedLyrics' (LRC string) and `plainLyrics' fields.

;;; Code:

(require 'emms-lyrics-core)
(require 'plz)
(require 'json)

(defconst emms-lyrics-lrclib-base-url "https://lrclib.net/api"
  "Base URL for the LRCLIB API.")

(defun emms-lyrics-source-lrclib (artist title album duration)
  "Fetch synced lyrics from LRCLIB for ARTIST / TITLE / ALBUM / DURATION.
Returns an `emms-lyrics-result' or nil."
  ;; TODO: implement async plz GET request
  ;; prefer syncedLyrics, fall back to plainLyrics
  nil)

(provide 'emms-lyrics-lrclib)
;;; emms-lyrics-lrclib.el ends here
