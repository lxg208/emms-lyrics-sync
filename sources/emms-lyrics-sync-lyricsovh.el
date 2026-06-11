;;; emms-lyrics-sync-lyricsovh.el --- lyrics.ovh plain-text fallback source  -*- lexical-binding: t -*-
;; File    : sources/emms-lyrics-sync-lyricsovh.el
;; Created : 2026-06-11 22:35 UTC
;; Purpose : Fetch plain-text lyrics from lyrics.ovh as a last-resort fallback.
;;           lyrics.ovh is a free, open REST API with no API key requirement.
;;           Returns plain text only (no timestamps), but covers a wide
;;           catalog of Western pop/rock when synced sources fail.
;;           Uses plz.el for async HTTP.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; API endpoint:
;;   GET https://api.lyrics.ovh/v1/<artist>/<title>
;;
;; Response JSON:
;;   { "lyrics": "line1\nline2\n..." }
;;   { "error": "No lyrics found" }    on miss
;;
;; Limitations:
;;   - Plain text only, no timestamps → displayed as plain unscrolled lyrics
;;   - Limited CJK coverage (use NetEase/QQ for Chinese music)
;;   - Occasional Unicode encoding issues for non-Latin scripts
;;
;; This source intentionally does no match scoring — the endpoint takes
;; artist + title directly, so either it returns lyrics or it doesn't.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-lyricsovh-url "https://api.lyrics.ovh/v1"
  "Base URL for the lyrics.ovh API."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-lyricsovh-timeout 10
  "HTTP timeout in seconds for lyrics.ovh requests."
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── URL Construction ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-lyricsovh--build-url (artist title)
  "Build the lyrics.ovh URL for ARTIST and TITLE.
Both path components are percent-encoded."
  (format "%s/%s/%s"
          emms-lyrics-sync-lyricsovh-url
          (url-hexify-string (string-trim artist))
          (url-hexify-string (string-trim title))))

;;; ── Response Parsing ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-lyricsovh--parse (body)
  "Parse a lyrics.ovh response BODY; return `emms-lyrics-sync-result' or nil."
  (condition-case err
      (let* ((obj  (json-parse-string body :object-type 'alist
                                           :null-object nil
                                           :false-object nil))
             ;; Miss: { "error": "No lyrics found" }
             (err-field (cdr (assoc "error" obj)))
             (lyrics    (cdr (assoc "lyrics" obj))))
        (cond
         (err-field nil)
         ((and (stringp lyrics)
               (not (string-empty-p (string-trim lyrics))))
          (emms-lyrics-sync-result--make
           :source  'lyricsovh
           :format  'plain
           :content (string-trim lyrics)))
         (t nil)))
    (json-parse-error
     (message "emms-lyrics-sync-lyricsovh: JSON parse error: %S" err)
     nil)))

;;; ── Source Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-source-lyricsovh (track callback)
  "Lyrics source: fetch plain-text lyrics from lyrics.ovh asynchronously.
This is a last-resort fallback; lyrics are plain text with no timestamps.
CALLBACK is called with an `emms-lyrics-sync-result' or nil."
  (let ((artist (emms-lyrics-sync-track-artist track))
        (title  (emms-lyrics-sync-track-title  track)))
    (unless (and artist title)
      (funcall callback nil)
      (cl-return-from emms-lyrics-sync-source-lyricsovh))
    (plz 'get (emms-lyrics-sync-lyricsovh--build-url artist title)
      :headers '(("Accept" . "application/json"))
      :timeout emms-lyrics-sync-lyricsovh-timeout
      :as      'string
      :then    (lambda (body)
                 (funcall callback (emms-lyrics-sync-lyricsovh--parse body)))
      :else    (lambda (err)
                 ;; 404 = not found, anything else is a real error
                 (let ((status (and (plz-error-p err)
                                    (plz-response-status
                                     (plz-error-response err)))))
                   (unless (eq status 404)
                     (message "emms-lyrics-sync-lyricsovh: HTTP error %S" status)))
                 (funcall callback nil)))))

(provide 'emms-lyrics-sync-lyricsovh)
;;; emms-lyrics-sync-lyricsovh.el ends here
