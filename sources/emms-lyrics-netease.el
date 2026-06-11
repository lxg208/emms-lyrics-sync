;;; emms-lyrics-netease.el --- NetEase Cloud Music source  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetches synchronized lyrics from NetEase Cloud Music (网易云音乐).
;; Particularly effective for Chinese-language music.
;;
;; API flow:
;;   1. Search: GET https://music.163.com/api/search/get
;;              ?s=ARTIST+TITLE&type=1&limit=5
;;   2. Lyrics: GET https://music.163.com/api/song/lyric
;;              ?id=SONG_ID&lv=1&kv=1&tv=-1
;;
;; NOTE: NetEase's unofficial API may require cookie/user-agent spoofing.
;; Implementation details TBD.

;;; Code:

(require 'emms-lyrics-core)
(require 'plz)
(require 'json)

(defconst emms-lyrics-netease-base-url "https://music.163.com/api"
  "Base URL for the NetEase Cloud Music API.")

(defun emms-lyrics-source-netease (artist title album duration)
  "Fetch lyrics from NetEase Cloud Music for ARTIST / TITLE / ALBUM / DURATION.
Returns an `emms-lyrics-result' or nil."
  ;; TODO: implement search + lyric fetch flow
  nil)

(provide 'emms-lyrics-netease)
;;; emms-lyrics-netease.el ends here
