;;; emms-lyrics-cover.el --- Cover art fetching and display  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Resolves cover art for the current track using the following priority:
;;   1. Embedded tag (ID3 APIC / Vorbis METADATA_BLOCK_PICTURE)
;;   2. cover.jpg / cover.png / front.jpg / front.png in track directory
;;   3. Slideshow: all *.jpg / *.png in track directory
;;   4. Remote fetch (MusicBrainz / Last.fm) — future work
;;
;; Renders the image centered at the top of the lyrics buffer.
;; Gracefully no-ops in terminal (non-graphic) frames.

;;; Code:

(require 'emms-lyrics-core)

(defcustom emms-lyrics-cover-preferred-filenames
  '("cover.jpg" "cover.png" "front.jpg" "front.png"
    "Cover.jpg" "Cover.png" "Front.jpg" "Front.png"
    "folder.jpg" "folder.png" "Folder.jpg" "Folder.png")
  "Filenames to look for when searching for local cover art."
  :type '(repeat string)
  :group 'emms-lyrics)

(defcustom emms-lyrics-cover-max-width 300
  "Maximum display width of cover art in pixels."
  :type 'integer
  :group 'emms-lyrics)

(defun emms-lyrics-cover-find (track)
  "Return the cover art image path for TRACK, or nil if none found.
Checks embedded tags first, then local files in the track directory."
  ;; TODO: implement
  ;;   1. extract embedded art via ffmpeg/exiftool to a temp file
  ;;   2. search for preferred filenames in (file-name-directory file-path)
  nil)

(defun emms-lyrics-cover-insert (image-path buffer)
  "Insert cover art from IMAGE-PATH centered in BUFFER.
No-ops silently in terminal frames."
  ;; TODO: implement image insertion with centering
  nil)

(provide 'emms-lyrics-cover)
;;; emms-lyrics-cover.el ends here
