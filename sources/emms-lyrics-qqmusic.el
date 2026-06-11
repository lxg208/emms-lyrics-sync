;;; emms-lyrics-qqmusic.el --- QQ Music source  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetches synchronized lyrics from QQ Music (QQ音乐).
;; Particularly effective for Chinese-language music.
;;
;; API flow (unofficial):
;;   1. Search: POST https://c.y.qq.com/soso/fcgi-bin/client_search_cp
;;   2. Lyrics: GET  https://c.y.qq.com/lyric/fcgi-bin/fcg_query_lyric_new.fcg
;;              ?songmid=SONG_MID&format=json&nobase64=1
;;
;; NOTE: QQ Music's unofficial API is subject to change and may require
;; specific headers/cookies.  Implementation details TBD.

;;; Code:

(require 'emms-lyrics-core)
(require 'plz)
(require 'json)

(defconst emms-lyrics-qqmusic-search-url
  "https://c.y.qq.com/soso/fcgi-bin/client_search_cp"
  "QQ Music search endpoint.")

(defun emms-lyrics-source-qqmusic (artist title album duration)
  "Fetch lyrics from QQ Music for ARTIST / TITLE / ALBUM / DURATION.
Returns an `emms-lyrics-result' or nil."
  ;; TODO: implement search + lyric fetch flow
  nil)

(provide 'emms-lyrics-qqmusic)
;;; emms-lyrics-qqmusic.el ends here
