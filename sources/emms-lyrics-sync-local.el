;;; emms-lyrics-sync-local.el --- Embedded tag and .lrc sidecar sources  -*- lexical-binding: t -*-
;; File    : sources/emms-lyrics-sync-local.el
;; Created : 2026-06-11 22:35 UTC
;; Purpose : Two local-only (no network) lyrics sources:
;;           (1) Extract embedded LYRICS/UNSYNCEDLYRICS tag from audio files
;;               via ffprobe; covers Vorbis LYRICS= (FLAC/OGG) and ID3v2
;;               USLT (MP3) uniformly without format-specific parsers.
;;           (2) Read a .lrc sidecar file from the same directory as the
;;               audio file, with a fallback to `emms-lyrics-sync-cache-dir'.
;;           Both sources call their callback synchronously.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Provides:
;;   `emms-lyrics-sync-source-tag'       — ffprobe-based embedded lyrics reader
;;   `emms-lyrics-sync-source-local-lrc' — .lrc sidecar / cache reader
;;
;; ffprobe (part of FFmpeg) must be on PATH for tag extraction.
;; Sidecar reading has no external dependencies.
;;
;; Tag key precedence checked by `emms-lyrics-sync-local--tag-keys':
;;   LYRICS          → Vorbis comment (FLAC, OGG, Opus)
;;   UNSYNCEDLYRICS  → ID3v2 USLT frame (MP3)
;;   Fallback variants for taggers that use non-standard capitalisation.

;;; Code:

(require 'cl-lib)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-local-ffprobe-executable "ffprobe"
  "Name or absolute path of the ffprobe executable.
Used to extract embedded lyrics tags from audio files."
  :type  'string
  :group 'emms-lyrics-sync)

;;; ── Internal: ffprobe Tag Extraction ─────────────────────────────────────────

(defconst emms-lyrics-sync-local--tag-keys
  ;; json-parse-buffer :object-type 'alist interns JSON string keys as symbols,
  ;; preserving case.  List common variants in priority order.
  '(LYRICS lyrics Lyrics
    UNSYNCEDLYRICS unsyncedlyrics UNSYNCED_LYRICS
    LYRIC Lyric lyric)
  "Symbols tried in order when scanning the ffprobe tag alist for lyrics.
Covers Vorbis LYRICS=, ID3 USLT, and common tagger variants.")

(defun emms-lyrics-sync-local--run-ffprobe (file-path)
  "Run ffprobe on FILE-PATH and return the tag alist, or nil on any failure.
Calls ffprobe with `-print_format json -show_entries format_tags'.
Returns nil when: ffprobe is not on PATH, exits non-zero, or produces
invalid JSON."
  (when (executable-find emms-lyrics-sync-local-ffprobe-executable)
    (with-temp-buffer
      (let ((exit-code
             (call-process emms-lyrics-sync-local-ffprobe-executable
                           nil          ; no stdin
                           t            ; output → current buffer
                           nil          ; no display
                           "-v"     "quiet"
                           "-print_format" "json"
                           "-show_entries" "format_tags"
                           (expand-file-name file-path))))
        (when (= 0 exit-code)
          (goto-char (point-min))
          (condition-case parse-err
              (let* ((root   (json-parse-buffer :object-type 'alist
                                                :null-object  nil
                                                :false-object nil))
                     (fmt    (cdr (assq 'format root)))
                     (tags   (cdr (assq 'tags   fmt))))
                tags)
            (json-parse-error
             (message "emms-lyrics-sync-local: ffprobe JSON parse error for %S: %S"
                      file-path parse-err)
             nil)))))))

(defun emms-lyrics-sync-local--find-lyrics-in-tags (tags)
  "Return the first non-empty lyrics string from TAGS alist, or nil.
Tries each symbol in `emms-lyrics-sync-local--tag-keys' in order."
  (when tags
    (cl-loop for sym in emms-lyrics-sync-local--tag-keys
             for val = (cdr (assq sym tags))
             when (and (stringp val)
                       (not (string-empty-p (string-trim val))))
             return val)))

(defun emms-lyrics-sync-local--detect-format (content)
  "Return \\='lrc or \\='plain based on whether CONTENT contains LRC timestamps.
A line matching \\[mm:ss is considered synced LRC."
  (if (string-match-p "\\[[ \t]*[0-9]+:[0-9]" content) 'lrc 'plain))

;;; ── Source 1: Embedded Tag ───────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-source-tag (track callback)
  "Lyrics source: extract embedded lyrics tag from the audio file via ffprobe.
Handles Vorbis LYRICS= (FLAC/OGG) and ID3v2 USLT (MP3) transparently.
Calls CALLBACK synchronously with an `emms-lyrics-sync-result' or nil."
  (let* ((path  (emms-lyrics-sync-track-file-path track))
         (tags  (and path
                     (file-readable-p (expand-file-name path))
                     (emms-lyrics-sync-local--run-ffprobe path)))
         (raw   (emms-lyrics-sync-local--find-lyrics-in-tags tags)))
    (funcall callback
             (when raw
               (emms-lyrics-sync-result--make
                :source  'tag
                :format  (emms-lyrics-sync-local--detect-format raw)
                :content raw)))))

;;; ── Source 2: Local .lrc Sidecar ─────────────────────────────────────────────

(defun emms-lyrics-sync-local--sidecar-path (file-path)
  "Return the .lrc sidecar path for FILE-PATH (same dir, same stem)."
  (concat (file-name-sans-extension (expand-file-name file-path)) ".lrc"))

(defun emms-lyrics-sync-local--cache-lrc-path (track)
  "Return the central-cache .lrc path for TRACK, or nil.
Format: <emms-lyrics-sync-cache-dir>/<artist> - <title>.lrc
Returns nil when artist or title are missing."
  (when-let* ((artist (emms-lyrics-sync-track-artist track))
              (title  (emms-lyrics-sync-track-title  track)))
    (expand-file-name
     (concat (emms-lyrics-sync-local--safe-filename artist)
             " - "
             (emms-lyrics-sync-local--safe-filename title)
             ".lrc")
     emms-lyrics-sync-cache-dir)))

(defun emms-lyrics-sync-local--safe-filename (str)
  "Replace filesystem-unsafe characters in STR for use as a filename component."
  (replace-regexp-in-string "[/\\\\:*?\"<>|\0]" "_" (string-trim str)))

;;;###autoload
(defun emms-lyrics-sync-source-local-lrc (track callback)
  "Lyrics source: read a .lrc sidecar file adjacent to the audio file.
Search order:
  1. <audio-stem>.lrc  in the same directory as the audio file
  2. <artist> - <title>.lrc  in `emms-lyrics-sync-cache-dir'
Calls CALLBACK synchronously with an `emms-lyrics-sync-result' or nil."
  (let* ((path       (emms-lyrics-sync-track-file-path track))
         (sidecar    (and path (emms-lyrics-sync-local--sidecar-path path)))
         (cache-lrc  (emms-lyrics-sync-local--cache-lrc-path track))
         ;; First readable candidate wins.
         (found      (cl-find-if #'file-readable-p
                                 (delq nil (list sidecar cache-lrc)))))
    (funcall callback
             (when found
               (emms-lyrics-sync-result--make
                :source  'local-lrc
                :format  'lrc
                :content (with-temp-buffer
                           (insert-file-contents found)
                           (buffer-string)))))))

(provide 'emms-lyrics-sync-local)
;;; sources/emms-lyrics-sync-local.el ends here
