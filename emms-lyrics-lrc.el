;;; emms-lyrics-lrc.el --- LRC parser and timestamp engine  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Parses LRC files into vectors of `emms-lyrics-line' structs.
;;
;; Supported formats:
;;   Standard LRC  : [01:23.45] Some lyric line
;;   Word-level A2 : [01:23.45]<01:23.45>Some <01:23.80>lyric <01:24.10>line
;;   Metadata tags : [ti:Title]  [ar:Artist]  [al:Album]  [offset:+500]
;;   Multi-timestamp: [00:12.00][01:34.00] Repeated chorus line
;;
;; Output is a sorted vector for O(log n) binary-search via
;; `emms-lyrics-lrc-seek'.

;;; Code:

(require 'cl-lib)
(require 'emms-lyrics-core)

;;; ── Timestamp Parsing ────────────────────────────────────────────────────────

(defun emms-lyrics-lrc--ts-to-ms (ts-string)
  "Convert a LRC timestamp string \"MM:SS.xx\" to integer milliseconds."
  ;; TODO: implement
  ;; e.g. "01:23.45" → 83450
  nil)

;;; ── Word-level (A2) Parsing ──────────────────────────────────────────────────

(defun emms-lyrics-lrc--parse-words (text-with-tags)
  "Parse A2 word-level tags from TEXT-WITH-TAGS.
Returns a list of (start-ms end-ms \"word\") triples."
  ;; TODO: implement
  ;; e.g. \"<01:23.45>Some <01:23.80>lyric <01:24.10>line\"
  nil)

;;; ── Line Parsing ─────────────────────────────────────────────────────────────

(defun emms-lyrics-lrc--parse-line (line offset-ms)
  "Parse a single LRC LINE string with OFFSET-MS applied.
Returns a list of `emms-lyrics-line' structs (one per timestamp on the line),
or nil for metadata / empty lines."
  ;; TODO: implement
  nil)

;;; ── Top-level Parser ─────────────────────────────────────────────────────────

(defun emms-lyrics-lrc-parse (content)
  "Parse LRC CONTENT string into a sorted vector of `emms-lyrics-line' structs.
Returns a plist (:lines VECTOR :meta ALIST) where meta contains
ti/ar/al/offset tags."
  ;; TODO: implement
  nil)

;;; ── Seek ─────────────────────────────────────────────────────────────────────

(defun emms-lyrics-lrc-seek (lines-vector position-ms)
  "Return the index in LINES-VECTOR of the active line at POSITION-MS.
Uses binary search.  Returns nil if LINES-VECTOR is empty or
POSITION-MS is before the first timestamp."
  ;; TODO: implement binary search
  nil)

(provide 'emms-lyrics-lrc)
;;; emms-lyrics-lrc.el ends here
