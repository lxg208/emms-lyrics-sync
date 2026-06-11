;;; emms-lyrics-sync-qqmusic.el --- QQ Music lyrics source  -*- lexical-binding: t -*-
;; File    : sources/emms-lyrics-sync-qqmusic.el
;; Created : 2026-06-11 22:35 UTC
;; Purpose : Fetch synced (LRC) lyrics from QQ Music (QQ音乐).
;;           Performs two async HTTP calls:
;;             1. Search API → extract best-match songmid
;;             2. Lyric API  → fetch base64-encoded LRC for that songmid
;;           Uses plz.el for async HTTP.  No API key required.
;;           Particularly strong for Chinese-language / Mandopop / Cantopop.
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
;;   Search (JSONP-style, returns JSON wrapped in a callback):
;;     GET https://c.y.qq.com/soso/fcgi-bin/client_search_cp
;;       ?w=<query>
;;       &format=json
;;       &p=1
;;       &n=5
;;       &cr=1
;;       &g_tk=5381
;;       &t=0         (0 = songs)
;;
;;   Lyrics:
;;     GET https://c.y.qq.com/lyric/fcgi-bin/fcg_query_lyric_new.fcg
;;       ?songmid=<songmid>
;;       &format=json
;;       &nobase64=0
;;
;; The lyric endpoint returns the LRC payload base64-encoded.  We decode
;; it with `base64-decode-string'.
;;
;; Match scoring reuses the same normalisation logic as emms-lyrics-sync-netease
;; since both services deal heavily with CJK and romanised titles.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-qqmusic-search-url
  "https://c.y.qq.com/soso/fcgi-bin/client_search_cp"
  "URL for the QQ Music search endpoint."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-qqmusic-lyric-url
  "https://c.y.qq.com/lyric/fcgi-bin/fcg_query_lyric_new.fcg"
  "URL for the QQ Music lyric endpoint."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-qqmusic-timeout 10
  "HTTP timeout in seconds for QQ Music requests."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-qqmusic-min-score 0.4
  "Minimum match score [0.0–1.0] to accept a QQ Music search result.
Candidates scoring below this threshold are rejected."
  :type  'float
  :group 'emms-lyrics-sync)

;;; ── HTTP Headers ─────────────────────────────────────────────────────────────

(defconst emms-lyrics-sync-qqmusic--headers
  '(("User-Agent"   . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 \
(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    ("Referer"      . "https://y.qq.com/")
    ("Origin"       . "https://y.qq.com")
    ("Accept"       . "application/json, text/plain, */*"))
  "HTTP headers required by the QQ Music API.")

;;; ── String Normalisation (mirrors emms-lyrics-sync-netease) ──────────────────────

(defun emms-lyrics-sync-qqmusic--normalise (str)
  "Lowercase, strip punctuation and featured-artist annotations from STR."
  (when (stringp str)
    (let ((s (downcase (string-trim str))))
      (setq s (replace-regexp-in-string
               "[ \t]*[([（【][ \t]*f(?:ea)?t\\.?.*?[)\\]）】]" "" s))
      (setq s (replace-regexp-in-string "[[:punct:]]" "" s))
      (setq s (replace-regexp-in-string "[ \t]+" " " s))
      (string-trim s))))

(defun emms-lyrics-sync-qqmusic--score (cand-artist cand-title want-artist want-title)
  "Return a match score in [0.0, 1.0] for a QQ Music search candidate."
  (let* ((nca (emms-lyrics-sync-qqmusic--normalise cand-artist))
         (nct (emms-lyrics-sync-qqmusic--normalise cand-title))
         (nwa (emms-lyrics-sync-qqmusic--normalise want-artist))
         (nwt (emms-lyrics-sync-qqmusic--normalise want-title))
         (title-match  (or (string= nct nwt)
                           (string-search nwt nct)
                           (string-search nct nwt)))
         (artist-match (or (null nwa)
                           (string= nca nwa)
                           (string-search nwa nca)
                           (string-search nca nwa))))
    (cond
     ((and title-match artist-match) 1.0)
     (title-match                    0.6)
     (artist-match                   0.2)
     (t                              0.0))))

;;; ── Step 1: Search ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-qqmusic--search-url (track)
  "Build the QQ Music search URL for TRACK."
  (let* ((artist (or (emms-lyrics-sync-track-artist track) ""))
         (title  (or (emms-lyrics-sync-track-title  track) ""))
         (query  (url-hexify-string (concat artist " " title))))
    (format "%s?w=%s&format=json&p=1&n=5&cr=1&g_tk=5381&t=0"
            emms-lyrics-sync-qqmusic-search-url
            query)))

(defun emms-lyrics-sync-qqmusic--best-songmid (body want-artist want-title)
  "Parse QQ Music search BODY; return best-matching songmid string or nil."
  (condition-case err
      (let* ((obj   (json-parse-string body :object-type 'alist
                                            :null-object nil
                                            :false-object nil))
             ;; Path: data → song → list
             (data  (cdr (assoc "data" obj)))
             (song  (cdr (assoc "song" data)))
             (items (cdr (assoc "list" song))))
        (when (and items (> (length items) 0))
          (let ((best-mid   nil)
                (best-score -1.0))
            (cl-loop for item across items do
              (let* ((mid        (cdr (assoc "songmid" item)))
                     (name       (cdr (assoc "songname" item)))
                     ;; singer is an array of objects with "name"
                     (singers    (cdr (assoc "singer" item)))
                     (artist-str
                      (when (and singers (> (length singers) 0))
                        (mapconcat
                         (lambda (s) (or (cdr (assoc "name" s)) ""))
                         singers " ")))
                     (score (emms-lyrics-sync-qqmusic--score
                             artist-str name want-artist want-title)))
                (when (> score best-score)
                  (setq best-score score
                        best-mid   mid))))
            (when (>= best-score emms-lyrics-sync-qqmusic-min-score)
              best-mid))))
    (json-parse-error
     (message "emms-lyrics-sync-qqmusic: search JSON parse error: %S" err)
     nil)))

;;; ── Step 2: Lyric Fetch ──────────────────────────────────────────────────────

(defun emms-lyrics-sync-qqmusic--lyric-url (songmid)
  "Build the QQ Music lyric URL for SONGMID."
  (format "%s?songmid=%s&format=json&nobase64=0"
          emms-lyrics-sync-qqmusic-lyric-url
          (url-hexify-string songmid)))

(defun emms-lyrics-sync-qqmusic--parse-lyric (body)
  "Parse QQ Music lyric BODY; return `emms-lyrics-sync-result' or nil.
The `lyric' field is base64-encoded LRC; we decode it here."
  (condition-case err
      (let* ((obj     (json-parse-string body :object-type 'alist
                                              :null-object nil
                                              :false-object nil))
             (b64     (cdr (assoc "lyric" obj)))
             (decoded (and (stringp b64)
                           (not (string-empty-p (string-trim b64)))
                           (base64-decode-string b64))))
        (when (and decoded
                   (stringp decoded)
                   (not (string-empty-p (string-trim decoded))))
          (emms-lyrics-sync-result--make
           :source  'qqmusic
           :format  'lrc
           :content decoded)))
    (json-parse-error
     (message "emms-lyrics-sync-qqmusic: lyric JSON parse error: %S" err)
     nil)))

;;; ── Source Entry Point ───────────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-source-qqmusic (track callback)
  "Lyrics source: fetch synced LRC from QQ Music asynchronously.
Performs two sequential HTTP requests: search → lyric.
CALLBACK is called with an `emms-lyrics-sync-result' or nil."
  (let ((artist (emms-lyrics-sync-track-artist track))
        (title  (emms-lyrics-sync-track-title  track)))
    (unless (and artist title)
      (funcall callback nil)
      (cl-return-from emms-lyrics-sync-source-qqmusic))
    ;; Step 1: search
    (plz 'get (emms-lyrics-sync-qqmusic--search-url track)
      :headers emms-lyrics-sync-qqmusic--headers
      :timeout emms-lyrics-sync-qqmusic-timeout
      :as      'string
      :then    (lambda (search-body)
                 (let ((songmid (emms-lyrics-sync-qqmusic--best-songmid
                                 search-body artist title)))
                   (if (null songmid)
                       (funcall callback nil)
                     ;; Step 2: fetch lyrics for best match
                     (plz 'get (emms-lyrics-sync-qqmusic--lyric-url songmid)
                       :headers emms-lyrics-sync-qqmusic--headers
                       :timeout emms-lyrics-sync-qqmusic-timeout
                       :as      'string
                       :then    (lambda (lyric-body)
                                  (funcall callback
                                           (emms-lyrics-sync-qqmusic--parse-lyric
                                            lyric-body)))
                       :else    (lambda (_err)
                                  (funcall callback nil))))))
      :else    (lambda (_err)
                 (funcall callback nil)))))

(provide 'emms-lyrics-sync-qqmusic)
;;; emms-lyrics-sync-qqmusic.el ends here
