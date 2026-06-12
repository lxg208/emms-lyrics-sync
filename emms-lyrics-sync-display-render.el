;;; emms-lyrics-sync-display-render.el --- Buffer-insert rendering functions  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-render.el
;; Created : 2026-06-12 15:56 UTC
;; Purpose : All buffer-insert rendering functions for the emms-lyrics-sync
;;           display subsystem.  Covers:
;;             • Cover art — synchronous ffmpeg extraction to a STABLE cache
;;               path (never a temp file) + sidecar search + memory cache.
;;               Stable path prevents "Cannot find image file" errors caused
;;               by Emacs lazy-loading images after the temp file was deleted.
;;             • Header rendering — 3-line metadata block (artist/album,
;;               title, tech) with elapsed-time marker pair for fast partial
;;               updates.  Tech line shows CODEC, bits/kHz, kbps, channels
;;               from track struct fields when available.
;;             • Lyrics rendering — context-window slice using direct
;;               lrc-seek indexing.  A2 word-level render records buffer
;;               positions for the overlay tick.
;;             • Overlay helpers — word-current and word-sung overlays.
;;             • Elapsed-time incremental update via marker pair.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Load order within the display subsystem:
;;   1. emms-lyrics-sync-display-vars.el   ← customization, faces, state
;;   2. emms-lyrics-sync-display-render.el ← this file
;;   3. emms-lyrics-sync-display.el        ← redraw orchestration, timer, hooks
;;
;; None of the functions here start timers or call the timer tick.
;; All side effects are buffer insertions plus overlay moves.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)

;;; ── Cover Art ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--find-cover-file (file-path)
  "Return a readable cover image path for FILE-PATH, or nil.
Tries `emms-lyrics-sync-display-cover-filenames' then any .jpg/.png
in the same directory."
  (when file-path
    (let ((dir (file-name-directory (expand-file-name file-path))))
      (or (cl-loop for name in emms-lyrics-sync-display-cover-filenames
                   for p = (expand-file-name name dir)
                   when (file-readable-p p) return p)
          (car (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t))))))

(defun emms-lyrics-sync-display--cover-stable-path (file-path)
  "Return a stable on-disk cache path for the extracted cover of FILE-PATH.
Uses emms-lyrics-sync-cache-dir/covers/<md5-of-path>.jpg.
The file may not exist yet; callers must check `file-exists-p'."
  (expand-file-name
   (concat (md5 (expand-file-name file-path)) ".jpg")
   (expand-file-name "covers" emms-lyrics-sync-cache-dir)))

(defun emms-lyrics-sync-display--extract-embedded-cover (file-path)
  "Extract embedded cover art from FILE-PATH via ffmpeg (synchronous).
Writes to a STABLE cache path under emms-lyrics-sync-cache-dir/covers/
so the file is always present when Emacs lazily loads the image pixels.
Returns the stable path on success, nil on failure or if ffmpeg is absent.
Never deletes the output file — the cache directory is the owner."
  (when (executable-find "ffmpeg")
    (let ((out (emms-lyrics-sync-display--cover-stable-path file-path)))
      (unless (file-exists-p out)
        (make-directory (file-name-directory out) t)
        (let ((ok (and (= 0 (call-process
                             "ffmpeg" nil nil nil
                             "-y" "-i" (expand-file-name file-path)
                             "-map" "0:v:0" "-frames:v" "1"
                             "-vcodec" "mjpeg" out))
                       (> (or (file-attribute-size (file-attributes out)) 0)
                          100))))
          (unless ok
            (ignore-errors (delete-file out))
            (setq out nil))))
      (and out (file-exists-p out) out))))

(defun emms-lyrics-sync-display--cover-image (file-path)
  "Return (possibly cached) Emacs image for the cover of FILE-PATH, or nil.

Cover source resolution order:
  1. In-memory cache (instant).
  2. Embedded art extracted via ffmpeg → stable cache path.
  3. Sidecar file (cover.jpg, front.png, …) in the track directory.

The extracted/sidecar file is NEVER deleted by this function.  Emacs
loads image pixel data lazily on first paint; deleting the file after
`create-image' returns would cause \"Cannot find image file\" spam."
  (when (and (display-graphic-p)
             (> emms-lyrics-sync-display-cover-height 0)
             file-path)
    (or (gethash file-path emms-lyrics-sync-display--cover-cache)
        (let* ((cover (or (emms-lyrics-sync-display--extract-embedded-cover file-path)
                          (emms-lyrics-sync-display--find-cover-file file-path)))
               (img   (when cover
                        (ignore-errors
                          (create-image cover nil nil
                                        :height emms-lyrics-sync-display-cover-height
                                        :scale  1.0)))))
          (when img
            (puthash file-path img emms-lyrics-sync-display--cover-cache))
          img))))

;;; ── Header ───────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--sr-string (hz)
  "Format sample-rate HZ (integer) as \"44.1\" or \"48\" kHz label."
  (when (integerp hz)
    (let ((rem (% hz 1000)))
      (if (zerop rem)
          (number-to-string (/ hz 1000))
        (format "%.1f" (/ hz 1000.0))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the 3-line metadata header for TRACK at point.
ELAPSED-S is the current playback position in seconds.

Inserts:
  Line 1 — Artist[ - Composer] - Album   (emms-lyrics-sync-artist-face)
  Line 2 — [NN. ]Title                   (emms-lyrics-sync-title-face)
  Line 3 — [CODEC | bits/kHz | kbps | ch | ]elapsed / dur

Returns a cons (elapsed-start . elapsed-end) — buffer positions of the
elapsed text, used by the caller to create marker pairs for fast
incremental updates on each tick."
  (let* ((artist   (emms-lyrics-sync-track-artist          track))
         (composer (emms-lyrics-sync-track-composer        track))
         (album    (emms-lyrics-sync-track-album           track))
         (trknum   (emms-lyrics-sync-track-track-number    track))
         (title    (emms-lyrics-sync-track-title           track))
         (codec    (emms-lyrics-sync-track-codec           track))
         (bps      (emms-lyrics-sync-track-bits-per-sample track))
         (sr-hz    (emms-lyrics-sync-track-sample-rate     track))
         (sr       (emms-lyrics-sync-display--sr-string sr-hz))
         (kbps     (emms-lyrics-sync-track-bitrate         track))
         (ch       (emms-lyrics-sync-track-channels        track))
         (dur      (emms-lyrics-sync-track-duration        track))
         ;; ── Line 1 ──────────────────────────────────────────────────────
         (l1 (mapconcat #'identity
                        (delq nil (list (or artist "Unknown Artist")
                                        composer
                                        (or album "")))
                        " - "))
         ;; ── Line 2 ──────────────────────────────────────────────────────
         (l2 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; ── Line 3 — tech fields (only those available) ─────────────────
         ;;   FLAC | 24/48 kHz | 1596 kbps | stereo
         (tech-parts (delq nil
                           (list (when codec
                                   (upcase codec))
                                 (when (and bps sr)
                                   (format "%s/%s kHz" bps sr))
                                 (when (and (null bps) sr)
                                   (format "%s kHz" sr))
                                 (when kbps
                                   (format "%d kbps" kbps))
                                 ch)))
         (static-tech  (mapconcat #'identity tech-parts " | "))
         (elapsed-str  (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str      (emms-lyrics-sync-display--format-time dur))
         (time-part    (concat elapsed-str " / " dur-str))
         ;; Full tech line: "FLAC | 24/48 kHz | 1:27 / 4:55"
         ;;             or just "1:27 / 4:55" when no tech data
         (l3           (if (string-empty-p static-tech)
                           time-part
                         (concat static-tech " | " time-part)))
         (centered-l3  (emms-lyrics-sync-display--center l3))
         ;; Locate elapsed offset within centered string for marker
         (elapsed-offset (string-search elapsed-str centered-l3))
         elapsed-start elapsed-end)
    ;; Insert line 1
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    ;; Insert line 2
    (insert (propertize (emms-lyrics-sync-display--center l2)
                        'face 'emms-lyrics-sync-title-face)  "\n")
    ;; Insert line 3, recording the elapsed span positions
    (let ((line-start (point)))
      (insert (propertize centered-l3 'face 'emms-lyrics-sync-tech-face))
      (when elapsed-offset
        (setq elapsed-start (+ line-start elapsed-offset)
              elapsed-end   (+ elapsed-start (length elapsed-str)))))
    (insert "\n")
    (cons elapsed-start elapsed-end)))

;;; ── Lyrics Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE centered at point, recording word positions.
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples."
  (let* ((words     (emms-lyrics-sync-line-words line))
         (line-text (emms-lyrics-sync-line-text  line))
         (width     (emms-lyrics-sync-display--body-width))
         (len       (string-width line-text))
         (pad       (max 0 (/ (- width len) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        (insert (emms-lyrics-sync-word-text word))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

(defun emms-lyrics-sync-display--render-plain-line (line face)
  "Insert LINE with FACE, centered."
  (insert (propertize (emms-lyrics-sync-display--center
                       (emms-lyrics-sync-line-text line))
                      'face face)
          "\n"))

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.
Uses lrc-seek directly to get cur-idx, then slices the lines vector
for before/current/after context.  Sets word-positions when current
line is A2."
  (setq emms-lyrics-sync-display--word-positions nil)
  (cond
   ;; No lyrics
   ((null doc)
    (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                        'face 'emms-lyrics-sync-past-line-face)
            "\n"))
   ;; Plain (unsynced) text — show all lines
   ((emms-lyrics-sync-lrc-doc-plain-p doc)
    (let ((lines (emms-lyrics-sync-lrc-doc-lines doc)))
      (cl-loop for line across lines do
        (emms-lyrics-sync-display--render-plain-line
         line 'emms-lyrics-sync-future-line-face))))
   ;; Synced LRC — context window around current line
   (t
    (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
           (n       (length lines))
           (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms))
           (n-bef   emms-lyrics-sync-display-context-before)
           (n-aft   emms-lyrics-sync-display-context-after))
      (if (< cur-idx 0)
          ;; Before first timestamp — show first N upcoming lines
          (cl-loop for i from 0 below (min n-aft n) do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-future-line-face))
        ;; Normal: render before / current / after slices
        (let ((start (max 0      (- cur-idx n-bef)))
              (end   (min (1- n) (+ cur-idx n-aft))))
          ;; Before
          (cl-loop for i from start below cur-idx do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-past-line-face))
          ;; Current
          (insert "\n")
          (let ((cur-line (aref lines cur-idx)))
            (if (emms-lyrics-sync-line-words cur-line)
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              (emms-lyrics-sync-display--render-plain-line
               cur-line 'emms-lyrics-sync-current-line-face)))
          (insert "\n")
          ;; After
          (cl-loop for i from (1+ cur-idx) to end do
            (emms-lyrics-sync-display--render-plain-line
             (aref lines i) 'emms-lyrics-sync-future-line-face))))))))

;;; ── Overlay Management ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--ensure-word-overlay ()
  "Return the word-highlight overlay, creating it if needed."
  (unless (overlayp emms-lyrics-sync-display--word-overlay)
    (setq emms-lyrics-sync-display--word-overlay
          (make-overlay 1 1 emms-lyrics-sync-display--buffer t nil))
    (overlay-put emms-lyrics-sync-display--word-overlay
                 'face 'emms-lyrics-sync-word-current-face)
    (overlay-put emms-lyrics-sync-display--word-overlay 'priority 10))
  emms-lyrics-sync-display--word-overlay)

(defun emms-lyrics-sync-display--clear-sung-overlays ()
  "Delete all sung-word overlays."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition current-word overlay and paint sung-word overlays for POS-MS."
  (when (and emms-lyrics-sync-display--word-positions
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (let ((ov           (emms-lyrics-sync-display--ensure-word-overlay))
          (found-cursor nil))
      (emms-lyrics-sync-display--clear-sung-overlays)
      (cl-loop for triple across emms-lyrics-sync-display--word-positions
               for buf-start = (nth 0 triple)
               for buf-end   = (nth 1 triple)
               for word      = (nth 2 triple)
               for w-start   = (emms-lyrics-sync-word-start-ms word)
               for w-end     = (or (emms-lyrics-sync-word-end-ms word)
                                   most-positive-fixnum)
               do (cond
                   ((and (>= pos-ms w-start) (< pos-ms w-end)
                         (not found-cursor))
                    (setq found-cursor t)
                    (move-overlay ov buf-start buf-end
                                  emms-lyrics-sync-display--buffer))
                   ((< w-end pos-ms)
                    (let ((sung (make-overlay buf-start buf-end
                                             emms-lyrics-sync-display--buffer)))
                      (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
                      (push sung emms-lyrics-sync-display--sung-overlays))))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────

(defun emms-lyrics-sync-display--update-elapsed (elapsed-s)
  "Replace only the elapsed-time text in the header for ELAPSED-S."
  (when (and (markerp emms-lyrics-sync-display--elapsed-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-marker)
             (markerp emms-lyrics-sync-display--elapsed-end-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t)
            (new-str (emms-lyrics-sync-display--format-time elapsed-s)))
        (save-excursion
          (delete-region
           (marker-position emms-lyrics-sync-display--elapsed-marker)
           (marker-position emms-lyrics-sync-display--elapsed-end-marker))
          (goto-char (marker-position emms-lyrics-sync-display--elapsed-marker))
          (insert (propertize new-str 'face 'emms-lyrics-sync-elapsed-face)))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
