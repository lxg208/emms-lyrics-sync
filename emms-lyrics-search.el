;;; emms-lyrics-search.el --- Manual search UI  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides the manual search UI, modelled on OpenLyrics in foobar2000.
;;
;; Flow:
;;   M-x emms-lyrics-search
;;     → opens *emms-lyrics-search* buffer with all track metadata pre-filled
;;     → user edits fields (artist, title, album, duration)
;;     → C-c C-c triggers parallel search across selected sources
;;     → *emms-lyrics-results* buffer shows ranked candidates
;;     → RET selects a candidate, saves to cache, updates display
;;
;;   C-c C-n  (emms-lyrics-next-source) cycles candidates without reopening UI

;;; Code:

(require 'emms-lyrics-core)

(defvar emms-lyrics-search--candidates nil
  "List of `emms-lyrics-result' candidates from the last search.")

(defvar emms-lyrics-search--current-index 0
  "Index into `emms-lyrics-search--candidates' for cycling.")

(defun emms-lyrics-search--track-to-fields (track)
  "Extract an alist of search fields from EMMS TRACK plist."
  ;; TODO: extract title, artist, album, duration from EMMS track
  nil)

(defun emms-lyrics-search-open ()
  "Open the manual search UI pre-filled with current track metadata."
  ;; TODO: create editable form buffer
  nil)

(defun emms-lyrics-search-execute (artist title album duration sources)
  "Query SOURCES in parallel for ARTIST, TITLE, ALBUM, DURATION.
Returns a list of `emms-lyrics-result' candidates."
  ;; TODO: async parallel source queries, collect and rank results
  nil)

(defun emms-lyrics-search-show-results (candidates)
  "Display CANDIDATES in the *emms-lyrics-results* buffer for selection."
  ;; TODO: render results buffer with source, format, and preview info
  nil)

(defun emms-lyrics-next-source ()
  "Cycle to the next lyrics candidate from the last search."
  ;; TODO: implement candidate cycling
  (interactive)
  nil)

(provide 'emms-lyrics-search)
;;; emms-lyrics-search.el ends here
