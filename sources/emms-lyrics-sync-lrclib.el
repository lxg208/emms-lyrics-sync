;;; emms-lyrics-sync-lrclib.el --- LRCLIB remote lyrics source  -*- lexical-binding: t -*-
;; File    : sources/emms-lyrics-sync-lrclib.el
;; Created : 2026-06-11 22:35 UTC
;; Purpose : Fetch synced (LRC) or plain lyrics from LRCLIB (https://lrclib.net).
;;           LRCLIB is a free, open, community-maintained lyrics database.
;;           No API key required.  Supports synced LRC and plain text lyrics.
;;           Uses plz.el for async HTTP.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; API endpoint used:
;;   GET https://lrclib.net/api/get
;;     ?artist_name=<artist>
;;     &track_name=<title>
;;     &album_name=<album>      (optional)
;;     &duration=<seconds>      (optional, improves match accuracy)
;;
;; Response JSON fields consumed:
;;   syncedLyrics  — LRC string with timestamps (preferred)
;;   plainLyrics   — plain text fallback
;;   instrumental  — boolean; when true we skip silently
;;
;; Rate limiting: LRCLIB imposes no hard rate limit for personal use.
;; We send a descriptive User-Agent per their recommendation.
;;
;; Dependency: plz.el  (https://github.com/alphapapa/plz.el)

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-lrclib-url "https://lrclib.net/api/get"
  "Base URL for the LRCLIB get endpoint."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-lrclib-user-agent
  "emms-lyrics/0.1 (https://github.com/lxg208/emms-lyrics-sync)"
  "User-Agent header sent to LRCLIB.
LRCLIB asks that clients identify themselves."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-lrclib-timeout 10
  "HTTP timeout in seconds for LRCLIB requests."
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── URL Construction ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrclib--build-url (track)
  "Build the LRCLIB API URL for TRACK.
All parameters are URL-encoded.  Album and duration are included when
available; LRCLIB uses them to improve match accuracy but does not
require them."
  (let* ((artist   (emms-lyrics-sync-track-artist   track))
         (title    (emms-lyrics-sync-track-title    track))
         (album    (emms-lyrics-sync-track-album    track))
         (duration (emms-lyrics-sync-track-duration track))
         (params   (list (cons "artist_name" (or artist ""))
                         (cons "track_name"  (or title  "")))))
    (when album
      (push (cons "album_name" album) params))
    (when duration
      (push (cons "duration" (number-to-string (round duration))) params))
    (concat emms-lyrics-sync-lrclib-url
            "?"
            (mapconcat (lambda (kv)
                         (concat (url-hexify-string (car kv))
                                 "="
                                 (url-hexify-string (cdr kv))))
                       (nreverse params)
                       "&"))))

;;; ── Response Parsing ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrclib--parse-response (json-string)
  "Parse LRCLIB JSON-STRING and return an `emms-lyrics-sync-result' or nil.
Returns nil when:
  - JSON is malformed or not an object
  - \\='instrumental\\=' key is true
  - Both syncedLyrics and plainLyrics are absent or empty"
  (condition-case parse-err
      (let* ((obj (json-parse-string json-string
                                     :object-type  'alist
                                     :null-object  nil
                                     :false-object nil))
             ;; json-parse-string with :object-type 'alist returns
             ;; string keys, not symbols.
             (instrumental (cdr (assoc "instrumental" obj)))
             (synced       (cdr (assoc "syncedLyrics" obj)))
             (plain        (cdr (assoc "plainLyrics"  obj))))
        (cond
         ;; Instrumental — no lyrics expected.
         (instrumental
          nil)
         ;; Prefer synced LRC.
         ((and (stringp synced)
               (not (string-empty-p (string-trim synced))))
          (emms-lyrics-sync-result--make
           :source  'lrclib
           :format  'lrc
           :content synced))
         ;; Fall back to plain text.
         ((and (stringp plain)
               (not (string-empty-p (string-trim plain))))
          (emms-lyrics-sync-result--make
           :source  'lrclib
           :format  'plain
           :content plain))
         (t nil)))
    (json-parse-error
     (message "emms-lyrics-sync-lrclib: JSON parse error: %S" parse-err)
     nil)))

;;; ── HTTP Error Handling ──────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrclib--handle-error (err callback)
  "Log the plz ERR for LRCLIB and invoke CALLBACK with nil."
  (let ((status (and (plz-error-p err)
                     (plz-response-status (plz-error-response err)))))
    (cond
     ((eq status 404)
      ;; Not found is expected and not worth logging at normal verbosity.
      nil)
     ((eq status 429)
      (message "emms-lyrics-sync-lrclib: rate limited — will retry on next track change"))
     (t
      (message "emms-lyrics-sync-lrclib: HTTP error %S: %S" status err))))
  (funcall callback nil))

;;; ── Source Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-source-lrclib (track callback)
  "Lyrics source: fetch synced LRC from LRCLIB asynchronously.
TRACK is an `emms-lyrics-sync-track'.
CALLBACK is called with an `emms-lyrics-sync-result' or nil.

Skips the request when artist or title is missing."
  (unless (and (emms-lyrics-sync-track-artist track)
               (emms-lyrics-sync-track-title  track))
    (funcall callback nil)
    (cl-return-from emms-lyrics-sync-source-lrclib))
  (let ((url (emms-lyrics-sync-lrclib--build-url track)))
    (plz 'get url
      :headers `(("User-Agent" . ,emms-lyrics-sync-lrclib-user-agent)
                 ("Accept"     . "application/json"))
      :timeout emms-lyrics-sync-lrclib-timeout
      :as      'string
      :then    (lambda (body)
                 (funcall callback
                          (emms-lyrics-sync-lrclib--parse-response body)))
      :else    (lambda (err)
                 (emms-lyrics-sync-lrclib--handle-error err callback)))))

(provide 'emms-lyrics-sync-lrclib)
;;; emms-lyrics-sync-lrclib.el ends here
