;;; emms-lyrics-sync-cover.el --- Cover art fetching and caching  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-cover.el
;; Created : 2026-06-11 22:50 UTC
;; Purpose : Provides cover art image objects for the display engine.
;;           Resolution order:
;;             1. Embedded art extracted from the audio file via ffmpeg
;;             2. Sidecar filenames in the track's directory
;;                (cover.jpg / front.jpg / folder.jpg / album.jpg, + .png)
;;             3. Glob fallback: first *.jpg or *.png in the directory
;;             4. Slideshow mode: if multiple images exist in the directory,
;;                cycle through them on a configurable interval
;;           Images are cached in memory as Emacs image objects.
;;           Extraction is async (via make-process); the display buffer is
;;           refreshed when a new image arrives.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Public API:
;;   `emms-lyrics-sync-cover-for-track'   — return cached image or start async fetch
;;   `emms-lyrics-sync-cover-invalidate'  — clear cache for a file path
;;   `emms-lyrics-sync-cover-start-slideshow'  — start cycling images
;;   `emms-lyrics-sync-cover-stop-slideshow'   — stop cycling images
;;
;; The display engine calls `emms-lyrics-sync-cover-for-track' at redraw time.
;; When the image isn't cached yet, it returns nil immediately and schedules
;; a callback that triggers `emms-lyrics-sync-display--full-redraw' when ready.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-cover-height 200
  "Pixel height for cover art in GUI Emacs.  Set to 0 to disable cover art."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-cover-filenames
  '("cover.jpg"   "cover.jpeg"  "cover.png"
    "front.jpg"   "front.jpeg"  "front.png"
    "folder.jpg"  "folder.jpeg" "folder.png"
    "album.jpg"   "album.jpeg"  "album.png"
    "artwork.jpg" "artwork.png")
  "Sidecar filenames tried in order when looking for cover art."
  :type  '(repeat string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-cover-slideshow nil
  "When non-nil, cycle through all images in the track directory."
  :type  'boolean
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-cover-slideshow-interval 10
  "Seconds between slideshow image transitions."
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-cover--cache (make-hash-table :test #'equal)
  "Cache: file-path → image object or 'pending or 'none.")

(defvar emms-lyrics-sync-cover--slideshow-images nil
  "List of image file paths for the current slideshow.")

(defvar emms-lyrics-sync-cover--slideshow-idx 0
  "Index of the currently shown slideshow image.")

(defvar emms-lyrics-sync-cover--slideshow-timer nil
  "Timer driving the slideshow image transitions.")

;;; ── Image File Discovery ─────────────────────────────────────────────────────

(defun emms-lyrics-sync-cover--find-sidecar (dir)
  "Return the first readable sidecar cover image in DIR, or nil.
Tries `emms-lyrics-sync-cover-filenames' in order."
  (cl-loop for name in emms-lyrics-sync-cover-filenames
           for path = (expand-file-name name dir)
           when (file-readable-p path)
           return path))

(defun emms-lyrics-sync-cover--find-any-image (dir)
  "Return the first *.jpg or *.png in DIR, or nil."
  (car (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t)))

(defun emms-lyrics-sync-cover--all-images (dir)
  "Return all image files in DIR sorted by name."
  (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t))

;;; ── Embedded Art Extraction ──────────────────────────────────────────────────

(defun emms-lyrics-sync-cover--extract-async (file-path callback)
  "Asynchronously extract embedded cover art from FILE-PATH via ffmpeg.
Calls CALLBACK with a temp file path on success, or nil on failure.
The caller is responsible for deleting the temp file after use."
  (unless (executable-find "ffmpeg")
    (funcall callback nil)
    (cl-return-from emms-lyrics-sync-cover--extract-async))
  (let ((tmp (make-temp-file "emms-lyrics-sync-cover-" nil ".jpg")))
    (make-process
     :name    "emms-lyrics-sync-cover-extract"
     :buffer  nil
     :command (list "ffmpeg"
                    "-y"                   ; overwrite temp file
                    "-i" (expand-file-name file-path)
                    "-map"      "0:v:0"    ; first video stream (cover art)
                    "-frames:v" "1"
                    "-vcodec"   "mjpeg"
                    tmp)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((ok (and (zerop (process-exit-status proc))
                        (> (or (file-attribute-size
                                (file-attributes tmp)) 0)
                           512))))  ; sanity: >512 bytes = probably a real image
           (funcall callback (if ok tmp
                               (ignore-errors (delete-file tmp))
                               nil))))))))

;;; ── Image Object Creation ────────────────────────────────────────────────────

(defun emms-lyrics-sync-cover--make-image (path)
  "Return an Emacs image object for PATH scaled to `emms-lyrics-sync-cover-height'."
  (when (and path (file-readable-p path) (display-graphic-p)
             (> emms-lyrics-sync-cover-height 0))
    (ignore-errors
      (create-image path nil nil
                    :height emms-lyrics-sync-cover-height
                    :scale  1.0))))

;;; ── Cache & Async Resolution ─────────────────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-cover-for-track (track callback)
  "Return a cover art image for TRACK asynchronously via CALLBACK.
CALLBACK is called with an image object or nil.

Resolution order:
  1. In-memory cache hit → CALLBACK immediately
  2. Pending extraction already running → CALLBACK queued
  3. Try sidecar files synchronously
  4. Async ffmpeg embedded extraction
  5. Glob fallback
  6. nil if everything fails"
  (let* ((fp     (emms-lyrics-sync-track-file-path track))
         (cached (and fp (gethash fp emms-lyrics-sync-cover--cache))))
    (cond
     ;; Cache hit
     ((and cached (not (eq cached 'pending)) (not (eq cached 'none)))
      (funcall callback cached))
     ;; Known miss
     ((eq cached 'none)
      (funcall callback nil))
     ;; Already extracting — the sentinel will call back
     ((eq cached 'pending)
      nil)
     ;; Not yet tried
     (fp
      (let* ((dir       (file-name-directory (expand-file-name fp)))
             (sidecar   (emms-lyrics-sync-cover--find-sidecar dir))
             (sidecar-img (emms-lyrics-sync-cover--make-image sidecar)))
        (if sidecar-img
            ;; Sidecar found synchronously
            (progn
              (puthash fp sidecar-img emms-lyrics-sync-cover--cache)
              (when emms-lyrics-sync-cover-slideshow
                (emms-lyrics-sync-cover--init-slideshow dir))
              (funcall callback sidecar-img))
          ;; Try embedded extraction async
          (puthash fp 'pending emms-lyrics-sync-cover--cache)
          (emms-lyrics-sync-cover--extract-async
           fp
           (lambda (tmp-path)
             (let* ((img (emms-lyrics-sync-cover--make-image tmp-path)))
               (ignore-errors (when tmp-path (delete-file tmp-path)))
               (unless img
                 ;; Glob fallback
                 (setq img (emms-lyrics-sync-cover--make-image
                             (emms-lyrics-sync-cover--find-any-image dir))))
               (puthash fp (or img 'none) emms-lyrics-sync-cover--cache)
               (when emms-lyrics-sync-cover-slideshow
                 (emms-lyrics-sync-cover--init-slideshow dir))
               (funcall callback img)))))))
     (t
      (funcall callback nil)))))

;;;###autoload
(defun emms-lyrics-sync-cover-invalidate (file-path)
  "Remove cached cover art for FILE-PATH, forcing re-resolution on next call."
  (when file-path
    (remhash file-path emms-lyrics-sync-cover--cache)))

;;; ── Slideshow ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-cover--init-slideshow (dir)
  "Initialise slideshow state for all images found in DIR."
  (setq emms-lyrics-sync-cover--slideshow-images (emms-lyrics-sync-cover--all-images dir)
        emms-lyrics-sync-cover--slideshow-idx    0))

;;;###autoload
(defun emms-lyrics-sync-cover-start-slideshow ()
  "Start cycling through all images in the current track's directory."
  (emms-lyrics-sync-cover-stop-slideshow)
  (when (and emms-lyrics-sync-cover--slideshow-images
             (> (length emms-lyrics-sync-cover--slideshow-images) 1))
    (setq emms-lyrics-sync-cover--slideshow-timer
          (run-at-time emms-lyrics-sync-cover-slideshow-interval
                       emms-lyrics-sync-cover-slideshow-interval
                       #'emms-lyrics-sync-cover--slideshow-tick))))

;;;###autoload
(defun emms-lyrics-sync-cover-stop-slideshow ()
  "Stop the cover art slideshow timer."
  (when (timerp emms-lyrics-sync-cover--slideshow-timer)
    (cancel-timer emms-lyrics-sync-cover--slideshow-timer))
  (setq emms-lyrics-sync-cover--slideshow-timer nil))

(defun emms-lyrics-sync-cover--slideshow-tick ()
  "Advance to the next slideshow image and refresh the display."
  (let* ((images emms-lyrics-sync-cover--slideshow-images)
         (n      (length images)))
    (when (> n 1)
      (setq emms-lyrics-sync-cover--slideshow-idx
            (% (1+ emms-lyrics-sync-cover--slideshow-idx) n))
      (let* ((path (nth emms-lyrics-sync-cover--slideshow-idx images))
             (img  (emms-lyrics-sync-cover--make-image path))
             (fp   (emms-lyrics-sync-track-file-path
                    emms-lyrics-sync-core--current-track)))
        (when (and img fp)
          (puthash fp img emms-lyrics-sync-cover--cache)
          ;; Trigger display redraw to show the new image
          (when (fboundp 'emms-lyrics-sync-display--full-redraw)
            (emms-lyrics-sync-display--full-redraw)))))))

(provide 'emms-lyrics-sync-cover)
;;; emms-lyrics-sync-cover.el ends here
