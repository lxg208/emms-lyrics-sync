;;; emms-lyrics-core.el --- Pipeline orchestration and cache  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Orchestrates the fetch pipeline:
;;   skip-predicates → disk cache → sources → parse → display
;;
;; Also owns the in-memory and on-disk LRC cache, and the EMMS hook wiring.

;;; Code:

(require 'cl-lib)
(require 'emms)

;;; ── Data Structures ──────────────────────────────────────────────────────────

(cl-defstruct emms-lyrics-result
  "A fetched lyrics payload."
  source        ; symbol: 'local-lrc | 'tag | 'lrclib | 'netease | 'qqmusic | 'lyricsovh
  format        ; 'lrc | 'lrc-a2 | 'plain
  content       ; raw string (LRC text or plain text)
  lines)        ; parsed vector of `emms-lyrics-line' structs, sorted by timestamp

(cl-defstruct emms-lyrics-line
  "A single lyrics line with optional word-level timestamps."
  start-ms      ; integer ms, or nil for unsynced plain-text lines
  text          ; full line string
  words)        ; list of (start-ms end-ms "word") — non-nil only for A2 LRC

;;; ── User Options ─────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sources
  '(emms-lyrics-source-tag
    emms-lyrics-source-local-lrc
    emms-lyrics-source-lrclib
    emms-lyrics-source-netease
    emms-lyrics-source-qqmusic
    emms-lyrics-source-lyricsovh)
  "Ordered list of lyrics source functions.
Each function receives (artist title album duration) and returns an
`emms-lyrics-result' or nil.  The first non-nil result wins."
  :type '(repeat function)
  :group 'emms-lyrics)

(defcustom emms-lyrics-skip-genres
  '("Classical" "Orchestral" "Ambient" "Soundtrack" "Jazz")
  "Genres for which lyrics lookup is silently skipped."
  :type '(repeat string)
  :group 'emms-lyrics)

(defvar emms-lyrics-skip-predicates
  '(emms-lyrics-skip-no-title-p
    emms-lyrics-skip-by-genre-p
    emms-lyrics-skip-by-stream-p)
  "List of predicates (TRACK) → bool.
If any returns non-nil the track is silently skipped.")

;;; ── Skip Predicates ──────────────────────────────────────────────────────────

(defun emms-lyrics-skip-no-title-p (track)
  "Return non-nil when TRACK has no title tag."
  (not (emms-track-get track 'info-title)))

(defun emms-lyrics-skip-by-genre-p (track)
  "Return non-nil when TRACK genre matches `emms-lyrics-skip-genres'."
  (when-let ((genre (emms-track-get track 'info-genre)))
    (member genre emms-lyrics-skip-genres)))

(defun emms-lyrics-skip-by-stream-p (track)
  "Return non-nil for internet radio / stream tracks (no local file)."
  (eq (emms-track-type track) 'url))

;;; ── Pipeline Entry Point ─────────────────────────────────────────────────────

(defun emms-lyrics-fetch-for-track (track)
  "Run the full fetch pipeline for TRACK.
Checks skip predicates, then cache, then remote sources.
Returns an `emms-lyrics-result' or nil."
  ;; TODO: implement pipeline
  ;;   1. run skip predicates
  ;;   2. check disk cache (.lrc sidecar or central cache)
  ;;   3. try each source in `emms-lyrics-sources' asynchronously
  ;;   4. write result to cache
  ;;   5. parse LRC / plain text
  ;;   6. hand off to display engine
  nil)

(provide 'emms-lyrics-core)
;;; emms-lyrics-core.el ends here
