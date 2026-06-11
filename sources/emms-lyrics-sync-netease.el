;;; emms-lyrics-sync-netease.el --- NetEase Cloud Music lyrics source  -*- lexical-binding: t -*-
;; File    : sources/emms-lyrics-sync-netease.el
;; Created : 2026-06-11 22:35 UTC
;; Purpose : Fetch synced (LRC) lyrics from NetEase Cloud Music (网易云音乐).
;;           Performs two async HTTP calls:
;;             1. Search API → extract best-match track ID
;;             2. Lyric API  → fetch LRC for that track ID
;;           Uses plz.el for async HTTP.  No API key required.
;;           Particularly strong for Chinese-language music.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; API endpoints (unofficial public API, no auth required):
;;
;;   Search:
;;     GET https://music.163.com/api/search/get
;;       ?s=<artist+title>
;;       &type=1          (1 = songs)
;;       &limit=5
;;       &offset=0
;;
;;   Lyrics:
;;     GET https://music.163.com/api/song/lyric
;;       ?id=<track-id>
;;       &lv=1            (lv=1 requests synced LRC)
;;       &tv=-1           (tv=-1 requests translated lyrics, ignored if absent)
;;
;; Match scoring: candidate results are scored by comparing normalised
;; artist/title strings.  The highest-scoring match above a minimum
;; threshold is selected.  This avoids returning obviously wrong lyrics
;; for popular homonyms or CJK romanisation variants.
;;
;; NetEase returns a `nolyric' or `uncollected' flag in the lyric
;; response for instrumentals or songs with no lyrics in their database.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-netease-search-url
  "https://music.163.com/api/search/get"
  "URL for the NetEase search endpoint."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-netease-lyric-url
  "https://music.163.com/api/song/lyric"
  "URL for the NetEase lyric endpoint."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-netease-timeout 10
  "HTTP timeout in seconds for NetEase requests."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-netease-min-score 0.4
  "Minimum match score [0.0–1.0] to accept a NetEase search result.
Candidates scoring below this threshold are rejected to avoid returning
lyrics for the wrong song.  Raise this to be more conservative."
  :type  'float
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-netease-search-limit 5
  "Number of candidates to fetch from the NetEase search API.
More candidates improve the chance of a correct match at the cost of a
slightly larger response payload."
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── NetEase HTTP Headers ─────────────────────────────────────────────────────

(defconst emms-lyrics-sync-netease--headers
  ;; NetEase's public API requires a browser-like User-Agent and Referer
  ;; to return results.  These are standard headers used by all clients.
  '(("User-Agent"   . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 \
(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    ("Referer"      . "https://music.163.com/")
    ("Accept"       . "application/json, text/plain, */*"))
  "HTTP headers required by the NetEase API.")

;;; ── String Normalisation for Matching ───────────────────────────────────────

(defun emms-lyrics-sync-netease--normalise (str)
  "Lowercase, strip punctuation and featured-artist annotations from STR.
Used to improve fuzzy matching between local tags and NetEase metadata."
  (when (stringp str)
    (let ((s (downcase (string-trim str))))
      ;; Remove common featured-artist suffixes: (feat. X), (ft. X), [feat. X]
      (setq s (replace-regexp-in-string
               "[ \t]*[([（【][ \t]*f(?:ea)?t\\.?.*?[)\\]）】]" "" s))
      ;; Remove punctuation, keeping CJK characters and alphanumeric
      (setq s (replace-regexp-in-string "[[:punct:]]" "" s))
      ;; Collapse whitespace
      (setq s (replace-regexp-in-string "[ \t]+" " " s))
      (string-trim s))))

(defun emms-lyrics-sync-netease--score (candidate-artist candidate-title
                                   want-artist      want-title)
  "Return a match score in [0.0, 1.0] for a NetEase search candidate.
Compares normalised CANDIDATE-ARTIST/TITLE against WANT-ARTIST/TITLE.
Equal weight given to artist and title.  Returns 0.5 for title-only match
when artist information is unavailable."
  (let* ((norm-ca (emms-lyrics-sync-netease--normalise candidate-artist))
         (norm-ct (emms-lyrics-sync-netease--normalise candidate-title))
         (norm-wa (emms-lyrics-sync-netease--normalise want-artist))
         (norm-wt (emms-lyrics-sync-netease--normalise want-title))
         ;; Simple substring containment: a song named "Hello" should match
         ;; a candidate titled "Hello (Radio Edit)".
         (title-match   (or (string= norm-ct norm-wt)
                            (string-search norm-wt norm-ct)
                            (string-search norm-ct norm-wt)))
         (artist-match  (or (null norm-wa)   ; no want-artist → don't penalise
                            (string= norm-ca norm-wa)
                            (string-search norm-wa norm-ca)
                            (string-search norm-ca norm-wa))))
    (cond
     ((and title-match artist-match) 1.0)
     (title-match                    0.6)
     (artist-match                   0.2)
     (t                              0.0))))

;;; ── Step 1: Search ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-netease--search-url (track)
  "Build the NetEase search URL for TRACK."
  (let* ((artist (or (emms-lyrics-sync-track-artist track) ""))
         (title  (or (emms-lyrics-sync-track-title  track) ""))
         (query  (url-hexify-string (concat artist " " title))))
    (format "%s?s=%s&type=1&limit=%d&offset=0"
            emms-lyrics-sync-netease-search-url
            query
            emms-lyrics-sync-netease-search-limit)))

(defun emms-lyrics-sync-netease--best-id (body want-artist want-title)
  "Parse NetEase search response BODY; return best-matching track ID or nil.
Scores each candidate in the result list and returns the ID of the one
exceeding `emms-lyrics-sync-netease-min-score', or nil if none qualifies."
  (condition-case err
      (let* ((obj      (json-parse-string body :object-type 'alist
                                               :null-object nil
                                               :false-object nil))
             (result   (cdr (assoc "result" obj)))
             (songs    (cdr (assoc "songs"  result))))
        (when (and songs (> (length songs) 0))
          (let ((best-id    nil)
                (best-score -1.0))
            (cl-loop for song across songs do
              (let* ((id      (cdr (assoc "id"   song)))
                     (name    (cdr (assoc "name" song)))
                     (artists (cdr (assoc "artists" song)))
                     ;; artists is an array; join first names for matching
                     (artist-str
                      (when (and artists (> (length artists) 0))
                        (mapconcat
                         (lambda (a) (or (cdr (assoc "name" a)) ""))
                         artists " ")))
                     (score (emms-lyrics-sync-netease--score
                             artist-str name want-artist want-title)))
                (when (> score best-score)
                  (setq best-score score
                        best-id    id))))
            (when (>= best-score emms-lyrics-sync-netease-min-score)
              best-id))))
    (json-parse-error
     (message "emms-lyrics-sync-netease: search JSON parse error: %S" err)
     nil)))

;;; ── Step 2: Lyric Fetch ──────────────────────────────────────────────────────

(defun emms-lyrics-sync-netease--lyric-url (track-id)
  "Build the NetEase lyric URL for TRACK-ID."
  (format "%s?id=%s&lv=1&tv=-1"
          emms-lyrics-sync-netease-lyric-url
          (number-to-string track-id)))

(defun emms-lyrics-sync-netease--parse-lyric (body)
  "Parse a NetEase lyric response BODY; return `emms-lyrics-sync-result' or nil."
  (condition-case err
      (let* ((obj      (json-parse-string body :object-type 'alist
                                               :null-object nil
                                               :false-object nil))
             (nolyric  (cdr (assoc "nolyric"     obj)))
             (uncoll   (cdr (assoc "uncollected" obj)))
             (lrc-obj  (cdr (assoc "lrc"         obj)))
             (lrc-str  (and lrc-obj (cdr (assoc "lyric" lrc-obj)))))
        (cond
         ((or nolyric uncoll)
          nil)
         ((and (stringp lrc-str)
               (not (string-empty-p (string-trim lrc-str))))
          (emms-lyrics-sync-result--make
           :source  'netease
           :format  'lrc
           :content lrc-str))
         (t nil)))
    (json-parse-error
     (message "emms-lyrics-sync-netease: lyric JSON parse error: %S" err)
     nil)))

;;; ── Source Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-source-netease (track callback)
  "Lyrics source: fetch synced LRC from NetEase Cloud Music asynchronously.
Performs two sequential HTTP requests: search → lyric.
CALLBACK is called with an `emms-lyrics-sync-result' or nil."
  (let ((artist (emms-lyrics-sync-track-artist track))
        (title  (emms-lyrics-sync-track-title  track)))
    (unless (and artist title)
      (funcall callback nil)
      (cl-return-from emms-lyrics-sync-source-netease))
    ;; Step 1: search
    (plz 'get (emms-lyrics-sync-netease--search-url track)
      :headers emms-lyrics-sync-netease--headers
      :timeout emms-lyrics-sync-netease-timeout
      :as      'string
      :then    (lambda (search-body)
                 (let ((track-id (emms-lyrics-sync-netease--best-id
                                  search-body artist title)))
                   (if (null track-id)
                       (funcall callback nil)
                     ;; Step 2: fetch lyrics for best match
                     (plz 'get (emms-lyrics-sync-netease--lyric-url track-id)
                       :headers emms-lyrics-sync-netease--headers
                       :timeout emms-lyrics-sync-netease-timeout
                       :as      'string
                       :then    (lambda (lyric-body)
                                  (funcall callback
                                           (emms-lyrics-sync-netease--parse-lyric
                                            lyric-body)))
                       :else    (lambda (_err)
                                  (funcall callback nil))))))
      :else    (lambda (_err)
                 (funcall callback nil)))))

(provide 'emms-lyrics-sync-netease)
;;; sources/emms-lyrics-sync-netease.el ends here
