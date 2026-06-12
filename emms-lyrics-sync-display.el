;;; emms-lyrics-sync-display.el --- Buffer rendering, scrolling, and highlighting  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display.el
;; Created : 2026-06-11 22:50 UTC
;; Purpose : Renders the emms-lyrics-sync display buffer.
;;           Draws cover art (embedded via ffmpeg → sidecar filenames → any
;;           .jpg/.png in the directory), a rich track metadata header matching
;;           the foobar2000 format:
;;             Artist[ - Composer] - Album
;;             Track. Title
;;             FLAC | 24/48 kHz | 1596 kbps | stereo | 1:44 / 3:43
;;           Synchronized lyrics with context window, line-level highlighting,
;;           and word-level A2 karaoke highlighting via overlays.
;;           Delegates waveform to emms-lyrics-sync-waveform.el.
;;           Driven by a 100 ms timer; only overlays move on each tick —
;;           full redraws happen only on track changes or context advances.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Called by emms-lyrics-sync-core.el via two integration hooks:
;;   `emms-lyrics-sync-display-on-track-change'  — new track started
;;   `emms-lyrics-sync-display-on-stop'          — playback stopped / paused
;;
;; Update strategy:
;;   Full redraw  — on track change and on lyrics context-window advance
;;                  (when the current-line index changes)
;;   Overlay tick — every 100 ms: moves word-highlight overlay and updates
;;                  the elapsed-time span in the header without touching text
;;   Word positions — recorded during full redraw as a vector of
;;                    (buf-start buf-end word-struct) triples so the timer
;;                    tick only calls `move-overlay', never re-scans text

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-display-buffer-name "*emms-lyrics*"
  "Name of the emms-lyrics-sync display buffer."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-side 'right
  "Side of the frame where the lyrics side window appears."
  :type  '(choice (const right) (const left))
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-width 52
  "Width in columns of the lyrics side window."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cover-height 180
  "Pixel height for cover art in GUI Emacs.  Set to 0 to disable."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cover-filenames
  '("cover.jpg"  "cover.jpeg"  "cover.png"
    "front.jpg"  "front.jpeg"  "front.png"
    "folder.jpg" "folder.jpeg" "folder.png"
    "album.jpg"  "album.jpeg"  "album.png")
  "Filenames tried in order when looking for a sidecar cover image."
  :type  '(repeat string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-context-before 3
  "Number of past lyrics lines visible above the current line."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-context-after 6
  "Number of upcoming lyrics lines visible below the current line."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-update-interval 0.1
  "Seconds between timer ticks.  Controls karaoke word-highlight smoothness."
  :type  'number
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface emms-lyrics-sync-artist-face
  '((t :height 1.2 :weight bold :inherit font-lock-function-name-face))
  "Face for the Artist [- Composer] - Album header line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-title-face
  '((t :height 1.1 :weight bold :inherit font-lock-string-face))
  "Face for the Track. Title line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-tech-face
  '((t :inherit font-lock-comment-face))
  "Face for the codec / bitrate / elapsed info line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-elapsed-face
  '((t :inherit font-lock-constant-face))
  "Face for the elapsed-time portion of the tech line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-past-line-face
  '((t :inherit font-lock-comment-face))
  "Face for lyrics lines that have already played."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-current-line-face
  '((t :height 1.15 :weight bold :inherit default))
  "Face for the currently playing lyrics line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-future-line-face
  '((t :inherit default))
  "Face for upcoming lyrics lines."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-sung-face
  '((((background dark))  :foreground "#ffffff" :weight bold)
    (((background light)) :foreground "#000000" :weight bold))
  "Face for already-sung words within the current A2 line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-current-face
  '((((background dark))  :foreground "#ffdd00" :weight bold :underline t)
    (((background light)) :foreground "#cc6600" :weight bold :underline t))
  "Face for the word currently being sung (A2 karaoke highlight)."
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display--buffer nil
  "The lyrics display buffer.")

(defvar emms-lyrics-sync-display--timer nil
  "The 100 ms display-update timer.")

(defvar emms-lyrics-sync-display--last-line-idx -1
  "Line index last used for a lyrics redraw; gates context-window advances.")

(defvar emms-lyrics-sync-display--word-overlay nil
  "Overlay used for A2 current-word highlighting.")

(defvar emms-lyrics-sync-display--sung-overlays nil
  "List of overlays for already-sung words in the current A2 line.")

(defvar emms-lyrics-sync-display--elapsed-marker nil
  "Marker at the start of the elapsed-time span in the header.")

(defvar emms-lyrics-sync-display--elapsed-end-marker nil
  "Marker at the end of the elapsed-time span in the header.")

(defvar emms-lyrics-sync-display--lyrics-marker nil
  "Marker at the start of the lyrics section; used for partial redraws.")

(defvar emms-lyrics-sync-display--word-positions nil
  "Vector of (buf-start buf-end emms-lyrics-sync-word) triples for the current A2 line.
Populated during full redraw; consumed by the timer tick.")

(defvar emms-lyrics-sync-display--cover-cache (make-hash-table :test #'equal)
  "Cache: cover-file-path → Emacs image object.")

;;; ── Utilities ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--format-time (seconds)
  "Format SECONDS (number or nil) as \"m:ss\"."
  (if (and (numberp seconds) (>= seconds 0))
      (let* ((s (round seconds))
             (m (/ s 60)))
        (format "%d:%02d" m (% s 60)))
    "--:--"))

(defun emms-lyrics-sync-display--window ()
  "Return the live window showing the lyrics buffer, or nil."
  (when (buffer-live-p emms-lyrics-sync-display--buffer)
    (get-buffer-window emms-lyrics-sync-display--buffer t)))

(defun emms-lyrics-sync-display--body-width ()
  "Return the usable body width of the lyrics window in characters."
  (let ((win (emms-lyrics-sync-display--window)))
    (if win (- (window-body-width win) 2) 60)))

(defun emms-lyrics-sync-display--center (str)
  "Return STR padded with spaces to center it in the lyrics window."
  (let* ((width (emms-lyrics-sync-display--body-width))
         (len   (string-width str))
         (pad   (max 0 (/ (- width len) 2))))
    (concat (make-string pad ?\s) str)))

;;; ── Cover Art ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--find-cover-file (file-path)
  "Return a readable cover image path for FILE-PATH, or nil.
Tries `emms-lyrics-sync-display-cover-filenames' first, then any
.jpg/.png in the same directory."
  (when file-path
    (let ((dir (file-name-directory (expand-file-name file-path))))
      (or (cl-loop for name in emms-lyrics-sync-display-cover-filenames
                   for p = (expand-file-name name dir)
                   when (file-readable-p p) return p)
          ;; Glob fallback
          (car (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t))))))

(defun emms-lyrics-sync-display--extract-embedded-cover (file-path)
  "Extract embedded cover art from FILE-PATH via ffmpeg.
Returns a temp file path on success (caller must delete it), or nil."
  (when (executable-find "ffmpeg")
    (let ((tmp (make-temp-file "emms-lyrics-sync-cover-" nil ".jpg")))
      (let ((ok (= 0 (call-process
                      "ffmpeg" nil nil nil
                      "-y" "-i" (expand-file-name file-path)
                      "-map" "0:v:0" "-frames:v" "1"
                      "-vcodec" "mjpeg" tmp))))
        (if (and ok
                 (> (or (file-attribute-size (file-attributes tmp)) 0) 100))
            tmp
          (ignore-errors (delete-file tmp))
          nil)))))

(defun emms-lyrics-sync-display--cover-image (file-path)
  "Return (possibly cached) Emacs image for the cover of FILE-PATH, or nil."
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
  "Format sample-rate HZ as \"44.1\" or \"48\" for the tech line."
  (when (integerp hz)
    (let ((rem (% hz 1000)))
      (if (zerop rem)
          (number-to-string (/ hz 1000))
        (format "%.1f" (/ hz 1000.0))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the 3-line metadata header for TRACK at point.
Returns a cons (elapsed-start . elapsed-end) — buffer positions of the
elapsed-time text, used for marker-based incremental updates."
  (let* ((artist   (emms-lyrics-sync-track-artist          track))
         (composer (emms-lyrics-sync-track-composer         track))
         (album    (emms-lyrics-sync-track-album            track))
         (trknum   (emms-lyrics-sync-track-track-number     track))
         (title    (emms-lyrics-sync-track-title            track))
         (codec    (emms-lyrics-sync-track-codec            track))
         (bps      (emms-lyrics-sync-track-bits-per-sample  track))
         (sr       (emms-lyrics-sync-display--sr-string
                    (emms-lyrics-sync-track-sample-rate     track)))
         (kbps     (emms-lyrics-sync-track-bitrate          track))
         (ch       (emms-lyrics-sync-track-channels         track))
         (dur      (emms-lyrics-sync-track-duration         track))
         ;; ── Line 1: Artist[ - Composer] - Album ─────────────────────────
         (l1 (mapconcat #'identity
                        (delq nil (list (or artist "Unknown Artist")
                                        composer
                                        album))
                        " - "))
         ;; ── Line 2: [NN. ]Title ──────────────────────────────────────────
         (l2 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; ── Line 3: FLAC | 24/48 kHz | 1596 kbps | stereo | e / d ───────
         (static-parts (mapconcat #'identity
                                  (delq nil
                                        (list codec
                                              (when (and bps sr)
                                                (format "%s/%s kHz" bps sr))
                                              (when kbps (format "%d kbps" kbps))
                                              ch))
                                  " | "))
         (elapsed-str  (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str      (emms-lyrics-sync-display--format-time dur))
         ;; Full tech line with elapsed embedded
         (l3           (concat static-parts
                               (if (string-empty-p static-parts) "" " | ")
                               elapsed-str " / " dur-str))
         ;; Find elapsed offset within the centered tech line
         (centered-l3  (emms-lyrics-sync-display--center l3))
         (elapsed-offset (string-search elapsed-str centered-l3))
         elapsed-start elapsed-end)
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    (insert (propertize (emms-lyrics-sync-display--center l2)
                        'face 'emms-lyrics-sync-title-face)  "\n")
    ;; Insert tech line, recording the elapsed span
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
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples in
forward order, for use by `emms-lyrics-sync-display--update-word-overlays'."
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
Sets `emms-lyrics-sync-display--word-positions' when the current line is A2."
  (setq emms-lyrics-sync-display--word-positions nil)
  (cond
   ((null doc)
    (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                        'face 'emms-lyrics-sync-past-line-face)
            "\n"))
   ((emms-lyrics-sync-lrc-doc-plain-p doc)
    (let ((lines (emms-lyrics-sync-lrc-doc-lines doc)))
      (cl-loop for line across lines do
        (emms-lyrics-sync-display--render-plain-line
         line 'emms-lyrics-sync-future-line-face))))
   (t
    (let* ((ctx     (emms-lyrics-sync-lrc-context doc pos-ms
                                             emms-lyrics-sync-display-context-before
                                             emms-lyrics-sync-display-context-after))
           (before  (alist-get 'before  ctx))
           (current (alist-get 'current ctx))
           (after   (alist-get 'after   ctx)))
      (if (and (null before) (null current) (null after))
          ;; pos is before the first timestamp (e.g. pos=0 at track start).
          ;; Show the first N upcoming lines as future context so the buffer
          ;; is not empty while waiting for the song to reach the first lyric.
          (let ((lines (emms-lyrics-sync-lrc-doc-lines doc)))
            (cl-loop for i from 0
                     below (min emms-lyrics-sync-display-context-after
                                (length lines))
                     do (emms-lyrics-sync-display--render-plain-line
                         (aref lines i)
                         'emms-lyrics-sync-future-line-face)))
        ;; Normal case: render context window
        (dolist (line before)
          (emms-lyrics-sync-display--render-plain-line
           line 'emms-lyrics-sync-past-line-face))
        (when current
          (insert "\n")
          (if (emms-lyrics-sync-line-words current)
              ;; A2: per-word render with position capture
              (setq emms-lyrics-sync-display--word-positions
                    (emms-lyrics-sync-display--render-a2-line current))
            ;; Standard LRC: single face for the whole line
            (emms-lyrics-sync-display--render-plain-line
             current 'emms-lyrics-sync-current-line-face))
          (insert "\n"))
        (dolist (line after)
          (emms-lyrics-sync-display--render-plain-line
           line 'emms-lyrics-sync-future-line-face)))))))

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
  "Delete all sung-word overlays from `emms-lyrics-sync-display--sung-overlays'."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition the current-word overlay and paint sung-word overlays for POS-MS."
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
                   ;; Currently active word
                   ((and (>= pos-ms w-start)
                         (<  pos-ms w-end)
                         (not found-cursor))
                    (setq found-cursor t)
                    (move-overlay ov buf-start buf-end
                                  emms-lyrics-sync-display--buffer))
                   ;; Already sung
                   ((< w-end pos-ms)
                    (let ((sung (make-overlay buf-start buf-end
                                             emms-lyrics-sync-display--buffer)))
                      (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
                      (push sung emms-lyrics-sync-display--sung-overlays))))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────

(defun emms-lyrics-sync-display--update-elapsed (elapsed-s)
  "Replace only the elapsed-time text in the header for ELAPSED-S.
Uses `emms-lyrics-sync-display--elapsed-marker' to avoid a full redraw."
  (when (and (markerp emms-lyrics-sync-display--elapsed-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-marker)
             (markerp emms-lyrics-sync-display--elapsed-end-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t)
            (new-str (emms-lyrics-sync-display--format-time elapsed-s)))
        (save-excursion
          (goto-char (marker-position emms-lyrics-sync-display--elapsed-marker))
          (delete-region
           (marker-position emms-lyrics-sync-display--elapsed-marker)
           (marker-position emms-lyrics-sync-display--elapsed-end-marker))
          (insert (propertize new-str 'face 'emms-lyrics-sync-elapsed-face)))))))

;;; ── Full Buffer Redraw ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--full-redraw ()
  "Erase and repopulate the lyrics buffer for the current track and position."
  (let* ((track   emms-lyrics-sync-core--current-track)
         (result  emms-lyrics-sync-core--current-result)
         (doc     (when result (emms-lyrics-sync-result-doc result)))
         (pos-ms  (or (emms-lyrics-sync-core--playback-position-ms) 0))
         (buf     (emms-lyrics-sync-display--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)
        ;; ── Cover art ──────────────────────────────────────────────────────
        (when track
          (let* ((fp  (emms-lyrics-sync-track-file-path track))
                 (img (emms-lyrics-sync-display--cover-image fp)))
            (when img
              (let* ((img-w  (or (car (image-size img t)) 0))
                     (char-w (frame-char-width))
                     (pad    (max 0 (/ (- (* (emms-lyrics-sync-display--body-width)
                                             char-w)
                                         img-w)
                                      2))))
                (insert (make-string (/ pad char-w) ?\s))
                (insert-image img)
                (insert "\n\n")))))
        ;; ── Metadata header ────────────────────────────────────────────────
        (when track
          (let ((elapsed-range
                 (emms-lyrics-sync-display--render-header track (/ pos-ms 1000.0))))
            (when (car elapsed-range)
              (setq emms-lyrics-sync-display--elapsed-marker
                    (copy-marker (car elapsed-range) t)
                    emms-lyrics-sync-display--elapsed-end-marker
                    (copy-marker (cdr elapsed-range))))))
        (insert "\n")
        ;; ── Lyrics ─────────────────────────────────────────────────────────
        (setq emms-lyrics-sync-display--lyrics-marker (copy-marker (point) t))
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)
        ;; ── Waveform ───────────────────────────────────────────────────────
        (when (and track (fboundp 'emms-lyrics-sync-waveform-insert))
          (insert "\n")
          (emms-lyrics-sync-waveform-insert
           (emms-lyrics-sync-track-file-path track)
           (/ pos-ms 1000.0)
           (emms-lyrics-sync-track-duration track)))
        (goto-char (point-min))
        (setq emms-lyrics-sync-display--last-line-idx
              (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1))))))

(defun emms-lyrics-sync-display--redraw-lyrics-only (doc pos-ms)
  "Replace only the lyrics section (and waveform) for POS-MS.
Preserves the cover art and header."
  (when (and (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (setq emms-lyrics-sync-display--word-positions nil)
        (delete-region (marker-position emms-lyrics-sync-display--lyrics-marker)
                       (point-max))
        (goto-char (marker-position emms-lyrics-sync-display--lyrics-marker))
        (emms-lyrics-sync-display--render-lyrics doc pos-ms)
        ;; NOTE: waveform is NOT re-appended here.
        ;; It is managed exclusively by full-redraw and the async extraction
        ;; callback.  Appending it here caused one waveform per lyric line
        ;; advance (~every 5-6 s).
        ))))

;;; ── Timer ────────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--start-timer ()
  "Start the 100 ms display-update timer."
  (emms-lyrics-sync-display--stop-timer)
  (setq emms-lyrics-sync-display--timer
        (run-at-time nil emms-lyrics-sync-display-update-interval
                     #'emms-lyrics-sync-display--tick)))

(defun emms-lyrics-sync-display--stop-timer ()
  "Cancel the display-update timer."
  (when (timerp emms-lyrics-sync-display--timer)
    (cancel-timer emms-lyrics-sync-display--timer))
  (setq emms-lyrics-sync-display--timer nil))

(defun emms-lyrics-sync-display--tick ()
  "Periodic update callback: advance context window, update overlays."
  ;; Skip work when the buffer is not visible.
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (emms-lyrics-sync-display--window))
    (condition-case err
        (let* ((result  emms-lyrics-sync-core--current-result)
               (doc     (when result (emms-lyrics-sync-result-doc result)))
               (pos-ms  (or (emms-lyrics-sync-core--playback-position-ms) 0))
               (new-idx (if doc (emms-lyrics-sync-lrc-seek doc pos-ms) -1)))
          ;; Elapsed time — every tick
          (emms-lyrics-sync-display--update-elapsed (/ pos-ms 1000.0))
          ;; Lyrics context — only when line index advances
          (unless (= new-idx emms-lyrics-sync-display--last-line-idx)
            (setq emms-lyrics-sync-display--last-line-idx new-idx)
            (emms-lyrics-sync-display--redraw-lyrics-only doc pos-ms))
          ;; Word overlays — every tick for smooth A2 animation
          (emms-lyrics-sync-display--update-word-overlays pos-ms))
      (error
       (message "emms-lyrics-sync-display: tick error: %S" err)))))

;;; ── Buffer & Window Management ───────────────────────────────────────────────

(defun emms-lyrics-sync-display--get-buffer ()
  "Return the lyrics buffer, creating it if needed."
  (unless (buffer-live-p emms-lyrics-sync-display--buffer)
    (setq emms-lyrics-sync-display--buffer
          (get-buffer-create emms-lyrics-sync-display-buffer-name))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (special-mode)
      (setq-local cursor-type        nil
                  truncate-lines     t
                  line-spacing       0.15
                  left-margin-width  1
                  right-margin-width 1)))
  emms-lyrics-sync-display--buffer)

;;;###autoload
(defun emms-lyrics-sync-display-show ()
  "Show the emms-lyrics-sync buffer in a side window."
  (interactive)
  (display-buffer
   (emms-lyrics-sync-display--get-buffer)
   `(display-buffer-in-side-window
     (side          . ,emms-lyrics-sync-display-side)
     (window-width  . ,emms-lyrics-sync-display-width)
     (preserve-size . (t . nil))
     (slot          . 0))))

;;;###autoload
(defun emms-lyrics-sync-display-hide ()
  "Hide the emms-lyrics-sync side window."
  (interactive)
  (when-let* ((win (emms-lyrics-sync-display--window)))
    (delete-window win)))

;;;###autoload
(defun emms-lyrics-sync-display-toggle ()
  "Toggle the emms-lyrics-sync side window."
  (interactive)
  (if (emms-lyrics-sync-display--window)
      (emms-lyrics-sync-display-hide)
    (emms-lyrics-sync-display-show)))

;;; ── Core Hook Integration ────────────────────────────────────────────────────

(defun emms-lyrics-sync-display-on-track-change ()
  "Called by `emms-lyrics-sync-core' when a new track starts.
Triggers a full buffer redraw and (re)starts the update timer."
  (setq emms-lyrics-sync-display--last-line-idx -1)
  (emms-lyrics-sync-display--full-redraw)
  (emms-lyrics-sync-display--start-timer)
  (emms-lyrics-sync-display-show))

(defun emms-lyrics-sync-display-on-stop ()
  "Called by `emms-lyrics-sync-core' when playback stops or is paused."
  (emms-lyrics-sync-display--stop-timer))

(provide 'emms-lyrics-sync-display)
;;; emms-lyrics-sync-display.el ends here
