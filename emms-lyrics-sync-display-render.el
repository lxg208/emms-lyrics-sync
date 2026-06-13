;;; emms-lyrics-sync-display-render.el --- Buffer-insert rendering functions  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-render.el
;; Created : 2026-06-13 05:35 UTC
;; Updated : 2026-06-13 18:30 UTC
;; Changes :
;;   • render-header: l5-suffix NOW ACTUALLY uses emms-lyrics-sync-duration-face
;;     (previous artifact had it in the changelog but NOT in the code — this is
;;     the real fix).
;;   • render-lyrics: paren count corrected to 8 at end of function
;;     (was 9 in previous artifact — one extra `)` caused a parse error that
;;     the user had to manually fix every time).
;;   • render-lyrics: pre-roll path mirrors first-line-as-current layout:
;;     n-bef blank rows | blank | line-0 (future-face) | blank | future | bot-pad
;;     Prevents visual jump when line-0 timestamp fires.
;;   • render-lyrics: past/future lines selected by physical row budget
;;     (count-wrapped-lines) not logical line count — fixes height bounce.
;;   • render-a2-line: visual text (words concatenated) used for centering,
;;     not raw text containing <mm:ss.cc> markers.
;;   • render-a2-line: current-line-face (amber) as base face so word
;;     overlays paint correctly: amber → green (sung) → yellow (current).
;;   • ensure-word-overlay: priority raised to 100; buffer-affinity check
;;     added — recreates overlay if buffer was killed/recreated.
;;   • reset-word-overlay: new function, collapses cursor overlay to (1,1)
;;     before any lyrics redraw to prevent stale highlights.
;;   • update-word-overlays: collapses cursor overlay when no word matches
;;     (was leaving stale overlay from previous position).
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)
(require 'emms-lyrics-sync-display-vars)

;;; ── Cover Art ────────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--find-cover-file (file-path)
  "Return a readable sidecar cover image path for FILE-PATH, or nil."
  (when file-path
    (let ((dir (file-name-directory (expand-file-name file-path))))
      (or (cl-loop for name in emms-lyrics-sync-display-cover-filenames
                   for p = (expand-file-name name dir)
                   when (file-readable-p p) return p)
          (car (directory-files dir t "\\.\\(jpg\\|jpeg\\|png\\)\\'" t))))))

(defun emms-lyrics-sync-display--cover-stable-path (file-path)
  "Return a stable on-disk cache path for the extracted cover of FILE-PATH."
  (expand-file-name
   (concat (md5 (expand-file-name file-path)) ".jpg")
   (expand-file-name "covers" emms-lyrics-sync-cache-dir)))

(defun emms-lyrics-sync-display--extract-embedded-cover (file-path)
  "Extract embedded cover art from FILE-PATH via ffmpeg (synchronous).
Writes to a stable path under emms-lyrics-sync-cache-dir/covers/.
Returns the stable path on success, nil on failure."
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
  "Return a (possibly cached) Emacs image for the cover of FILE-PATH, or nil."
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

;;; ── ffprobe Tech-Info Augmentation ───────────────────────────────────────────

(defun emms-lyrics-sync-display--ffprobe-augment (track callback)
  "Augment TRACK tech fields asynchronously via ffprobe; call CALLBACK when done."
  (let ((fp (emms-lyrics-sync-track-file-path track)))
    (if (not (and fp (executable-find "ffprobe")))
        (funcall callback)
      (if (emms-lyrics-sync-track-codec track)
          (funcall callback)
        (let ((buf (generate-new-buffer " *emms-ffprobe-tech*")))
          (make-process
           :name    "emms-lyrics-sync-ffprobe"
           :buffer  buf
           :command (list "ffprobe"
                          "-v"            "quiet"
                          "-print_format" "json"
                          "-show_streams"
                          "-show_format"
                          "-select_streams" "a:0"
                          (expand-file-name fp))
           :noquery t
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let ((json (with-current-buffer buf (buffer-string))))
                 (kill-buffer buf)
                 (condition-case nil
                     (let* ((obj    (json-parse-string
                                     json
                                     :object-type  'alist
                                     :null-object  nil
                                     :false-object nil))
                            (strs   (cdr (assq 'streams obj)))
                            (audio  (and strs (> (length strs) 0)
                                         (aref strs 0)))
                            (fmt    (cdr (assq 'format  obj))))
                       (when audio
                         (let ((cn (cdr (assq 'codec_name audio))))
                           (when (stringp cn)
                             (setf (emms-lyrics-sync-track-codec track)
                                   (upcase cn))))
                         (let ((sr (cdr (assq 'sample_rate audio))))
                           (when (stringp sr)
                             (setf (emms-lyrics-sync-track-sample-rate track)
                                   (string-to-number sr))))
                         (let* ((b (cdr (assq 'bits_per_raw_sample audio)))
                                (n (if (stringp b) (string-to-number b)
                                     (or b 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bits-per-sample
                                    track) n)))
                         (let* ((raw (or (cdr (assq 'bit_rate audio))
                                         (and fmt (cdr (assq 'bit_rate fmt)))))
                                (n   (if (stringp raw) (string-to-number raw)
                                       (or raw 0))))
                           (when (> n 0)
                             (setf (emms-lyrics-sync-track-bitrate track)
                                   (round (/ n 1000.0)))))
                         (let ((layout (cdr (assq 'channel_layout audio)))
                               (count  (cdr (assq 'channels audio))))
                           (cond
                            ((and (stringp layout) (not (string-empty-p layout)))
                             (setf (emms-lyrics-sync-track-channels track) layout))
                            ((integerp count)
                             (setf (emms-lyrics-sync-track-channels track)
                                   (format "%d ch" count)))))))
                   (error nil))
                 (funcall callback))))))))))

;;; ── Header Rendering ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--sr-string (hz)
  "Format sample-rate HZ as \"44.1\" or \"48\" kHz for the tech line."
  (when (and (integerp hz) (> hz 0))
    (let* ((khz (/ hz 1000))
           (rem (% hz 1000)))
      (if (zerop rem)
          (number-to-string khz)
        (format "%d.%d" khz (/ rem 100))))))

(defun emms-lyrics-sync-display--render-header (track elapsed-s)
  "Insert the metadata header for TRACK at point.
Returns (elapsed-buf-start . elapsed-buf-end) for marker-based incremental
updates, or nil if the elapsed line was not inserted.

Header layout:
  Line 1: Artist[ - Composer]             ← artist-face
  Line 2: Album                           ← album-face  (omitted if absent)
  Line 3: [Track. ]Title                  ← title-face
  Line 4: CODEC | bits/kHz | kbps | ch   ← tech-face   (omitted if all nil)
  Line 5: elapsed / duration              ← elapsed-face / duration-face

IMPORTANT: l5-suffix (\"/ 4:55\") uses emms-lyrics-sync-duration-face,
NOT tech-face.  Both faces were previously the same grey as past-line-face
which made them unreadable.  tech-face is now steel-blue and duration-face
is a lighter blue-grey — both clearly distinct from the grey past lyrics."
  (let* ((artist   (emms-lyrics-sync-track-artist          track))
         (composer (emms-lyrics-sync-track-composer         track))
         (album    (emms-lyrics-sync-track-album            track))
         (trknum   (emms-lyrics-sync-track-track-number     track))
         (title    (emms-lyrics-sync-track-title            track))
         (codec    (emms-lyrics-sync-track-codec            track))
         (bps      (emms-lyrics-sync-track-bits-per-sample  track))
         (sr       (emms-lyrics-sync-track-sample-rate      track))
         (kbps     (emms-lyrics-sync-track-bitrate          track))
         (ch       (emms-lyrics-sync-track-channels         track))
         (dur      (emms-lyrics-sync-track-duration         track))
         ;; Line 1
         (l1 (concat (or artist "Unknown Artist")
                     (if (and composer (not (string-empty-p composer)))
                         (concat " - " composer) "")))
         ;; Line 2
         (l2 (when (and album (not (string-empty-p album))) album))
         ;; Line 3
         (l3 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         ;; Line 4
         (sr-s   (emms-lyrics-sync-display--sr-string sr))
         (bps/sr (cond ((and bps sr-s) (format "%d/%s" bps sr-s))
                       (sr-s           sr-s)
                       (t              nil)))
         (kbps-s (when (and kbps (> kbps 0)) (format "%d kbps" kbps)))
         (l4     (let ((parts (delq nil (list codec bps/sr kbps-s ch))))
                   (when parts (mapconcat #'identity parts " | "))))
         ;; Line 5
         (elapsed-str (emms-lyrics-sync-display--format-time elapsed-s))
         (dur-str     (emms-lyrics-sync-display--format-time dur))
         (l5-suffix   (concat " / " dur-str))
         elapsed-start elapsed-end)
    ;; ── Insert lines ─────────────────────────────────────────────────────────
    (insert (propertize (emms-lyrics-sync-display--center l1)
                        'face 'emms-lyrics-sync-artist-face) "\n")
    (when l2
      (insert (propertize (emms-lyrics-sync-display--center l2)
                          'face 'emms-lyrics-sync-album-face) "\n"))
    (insert (propertize (emms-lyrics-sync-display--center l3)
                        'face 'emms-lyrics-sync-title-face) "\n")
    (when l4
      (insert (propertize (emms-lyrics-sync-display--center l4)
                          'face 'emms-lyrics-sync-tech-face) "\n"))
    ;; Line 5: two halves with different faces, record elapsed span via (point)
    (let* ((l5-full (concat elapsed-str l5-suffix))
           (pad     (max 0 (/ (- (emms-lyrics-sync-display--body-width)
                                  (string-width l5-full))
                               2))))
      (insert (make-string pad ?\s))
      (setq elapsed-start (point))
      (insert (propertize elapsed-str 'face 'emms-lyrics-sync-elapsed-face))
      (setq elapsed-end (point))
      ;; ── KEY FIX: duration uses duration-face, NOT tech-face ──────────────
      ;; Previously both l5-suffix and l4 used tech-face (same grey as
      ;; past-line-face).  Now duration-face is a lighter blue-grey that
      ;; reads as "metadata" without clashing with the teal elapsed face.
      (insert (propertize l5-suffix 'face 'emms-lyrics-sync-duration-face)))
    (insert "\n")
    (cons elapsed-start elapsed-end)))

;;; ── Lyrics: Wrapping Helper ──────────────────────────────────────────────────

(defun emms-lyrics-sync-display--count-wrapped-lines (text)
  "Return the number of physical screen lines TEXT occupies after word-wrap.
Uses the current window body width.  Never returns less than 1."
  (if (or (null text) (string-empty-p text))
      1
    (let* ((width (emms-lyrics-sync-display--body-width))
           (words (split-string text))
           (count 1)
           (used  0))
      (dolist (w words)
        (let ((wlen (1+ (string-width w))))
          (if (> (+ used wlen) width)
              (setq count (1+ count)
                    used  (string-width w))
            (setq used (+ used wlen)))))
      count)))

(defun emms-lyrics-sync-display--wrap-and-insert (text face)
  "Insert TEXT with FACE, word-wrapped and centered within body width."
  (if (or (null text) (string-empty-p (string-trim text)))
      (insert "\n")
    (let* ((width  (emms-lyrics-sync-display--body-width))
           (words  (split-string text))
           lines current-line current-width)
      (dolist (w words)
        (let ((wlen (string-width w)))
          (if (null current-line)
              (setq current-line (list w)
                    current-width wlen)
            (if (<= (+ current-width 1 wlen) width)
                (setq current-line  (append current-line (list w))
                      current-width (+ current-width 1 wlen))
              (push current-line lines)
              (setq current-line  (list w)
                    current-width wlen)))))
      (when current-line (push current-line lines))
      (setq lines (nreverse lines))
      (dolist (seg lines)
        (let* ((seg-str (mapconcat #'identity seg " "))
               (pad     (max 0 (/ (- width (string-width seg-str)) 2))))
          (insert (make-string pad ?\s))
          (insert (propertize seg-str 'face face))
          (insert "\n"))))))

;;; ── Lyrics: A2 Word-Level Render ─────────────────────────────────────────────

(defun emms-lyrics-sync-display--a2-visual-text (line)
  "Return the visible text of A2 LINE by concatenating word texts.
A2 line-text contains raw <mm:ss.cc> markers; those are not visible and
must not be used for string-width centering or count-wrapped-lines."
  (let ((words (emms-lyrics-sync-line-words line)))
    (if words
        (mapconcat #'emms-lyrics-sync-word-text words "")
      (or (emms-lyrics-sync-line-text line) ""))))

(defun emms-lyrics-sync-display--render-a2-line (line)
  "Insert A2 word-level LINE at point, centered, recording word positions.
Uses current-line-face (amber) as the base face so the visual progression
is: amber (base) → green (sung overlay) → yellow+underline (current overlay).
Returns a vector of (buf-start buf-end emms-lyrics-sync-word) triples."
  (let* ((words    (emms-lyrics-sync-line-words line))
         (vis-text (emms-lyrics-sync-display--a2-visual-text line))
         (width    (emms-lyrics-sync-display--body-width))
         (len      (string-width vis-text))
         (pad      (max 0 (/ (- width len) 2)))
         positions)
    (insert (make-string pad ?\s))
    (dolist (word words)
      (let ((start (point)))
        ;; Base face is current-line-face (amber) — overlays paint over it
        (insert (propertize (emms-lyrics-sync-word-text word)
                            'face 'emms-lyrics-sync-current-line-face))
        (push (list start (point) word) positions)))
    (insert "\n")
    (vconcat (nreverse positions))))

;;; ── Lyrics: Context Window with Fixed Physical Height ────────────────────────

(defun emms-lyrics-sync-display--render-lyrics (doc pos-ms)
  "Insert the lyrics section for DOC at POS-MS.
Always occupies exactly `emms-lyrics-sync-display-lyrics-height' physical
screen lines.

Algorithm (normal path):
  1. cur-height = count-wrapped-lines(cur-line-text)
  2. fut-budget = n-after - (cur-height - 1)
  3. Past: walk BACKWARD from cur-idx-1; accumulate physical rows via
     count-wrapped-lines; stop when row budget exhausted.
     A line is only included if its physical height FITS the remaining budget
     entirely (no partial lines).
  4. Future: same forward from cur-idx+1 against fut-budget.
  5. top-pad = n-before - past-phys  blank lines above past lines.
  6. bot-pad = fut-budget - future-phys  blank lines below future lines.

Pre-roll path (before first timestamp):
  Mirrors the normal layout as if line-0 were the current line but shown
  in future-face.  This prevents a visual jump when line-0 fires.
  Layout: [n-bef blanks] [blank] [line-0 future-face] [blank] [lines 1..k] [bot-pad]

Total = n-bef + 1 + cur-height + 1 + fut-budget = n-bef + n-aft + 3 = lyrics-height."
  (setq emms-lyrics-sync-display--word-positions nil)
  (let* ((n-bef emms-lyrics-sync-display-context-before)
         (n-aft emms-lyrics-sync-display-context-after))
    (cond
     ;; ── No lyrics ──────────────────────────────────────────────────────────
     ((null doc)
      (let ((total emms-lyrics-sync-display-lyrics-height))
        (dotimes (_ (/ total 2)) (insert "\n"))
        (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                            'face 'emms-lyrics-sync-past-line-face) "\n")
        (let ((remaining (- total (/ total 2) 1)))
          (dotimes (_ (max 0 remaining)) (insert "\n")))))

     ;; ── Plain (unsynced) text ───────────────────────────────────────────────
     ((emms-lyrics-sync-lrc-doc-plain-p doc)
      (let ((lines (emms-lyrics-sync-lrc-doc-lines doc)))
        (cl-loop for line across lines do
          (emms-lyrics-sync-display--wrap-and-insert
           (emms-lyrics-sync-line-text line)
           'emms-lyrics-sync-future-line-face))))

     ;; ── Synced LRC ─────────────────────────────────────────────────────────
     (t
      (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
             (n       (length lines))
             (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms)))

        (if (< cur-idx 0)
            ;; ── Pre-roll: mirror first-line-as-current layout ───────────────
            ;; Line 0 shown in future-face IN THE CURRENT SLOT so the layout
            ;; is identical to what it will be when line-0 fires.  No jump.
            (let* ((first-line
                    (and (> n 0) (aref lines 0)))
                   (first-text
                    (and first-line (emms-lyrics-sync-line-text first-line)))
                   (first-height
                    (if first-line
                        (emms-lyrics-sync-display--count-wrapped-lines first-text)
                      1))
                   (fut-budget (max 0 (- n-aft (- first-height 1))))
                   ;; Future lines after slot 0
                   (future-lines
                    (let (acc (budget fut-budget))
                      (cl-loop for i from 1 below n
                               for ltext = (emms-lyrics-sync-line-text (aref lines i))
                               for lphys = (emms-lyrics-sync-display--count-wrapped-lines ltext)
                               while (and (> budget 0) (<= lphys budget))
                               do (push (aref lines i) acc)
                                  (cl-decf budget lphys))
                      (nreverse acc)))
                   (future-phys
                    (cl-loop for l in future-lines
                             sum (emms-lyrics-sync-display--count-wrapped-lines
                                  (emms-lyrics-sync-line-text l))))
                   (bot-pad (max 0 (- fut-budget future-phys))))
              ;; n-bef blank rows above current slot
              (dotimes (_ n-bef) (insert "\n"))
              ;; Blank separator before current slot
              (insert "\n")
              ;; Line 0 in current slot (future-face — not yet playing)
              (if first-line
                  (emms-lyrics-sync-display--wrap-and-insert
                   first-text 'emms-lyrics-sync-future-line-face)
                (insert "\n"))
              ;; Blank separator after current slot
              (insert "\n")
              ;; Future lines
              (dolist (line future-lines)
                (emms-lyrics-sync-display--wrap-and-insert
                 (emms-lyrics-sync-line-text line)
                 'emms-lyrics-sync-future-line-face))
              ;; Bottom padding
              (dotimes (_ bot-pad) (insert "\n")))

          ;; ── Normal: fixed-height context window ────────────────────────────
          (let* ((cur-line   (aref lines cur-idx))
                 (cur-text   (emms-lyrics-sync-line-text cur-line))
                 (cur-height (if (emms-lyrics-sync-line-words cur-line)
                                 1
                               (emms-lyrics-sync-display--count-wrapped-lines
                                cur-text)))
                 (fut-budget (max 0 (- n-aft (- cur-height 1))))
                 ;; Past lines: walk backward, accumulate physical rows
                 (past-lines
                  (let (acc (budget n-bef))
                    (cl-loop for i downfrom (1- cur-idx) to 0
                             for ltext = (emms-lyrics-sync-line-text (aref lines i))
                             for lphys = (emms-lyrics-sync-display--count-wrapped-lines ltext)
                             while (and (> budget 0) (<= lphys budget))
                             do (push (aref lines i) acc)
                                (cl-decf budget lphys))
                    acc))
                 (past-phys
                  (cl-loop for l in past-lines
                           sum (emms-lyrics-sync-display--count-wrapped-lines
                                (emms-lyrics-sync-line-text l))))
                 (top-pad (max 0 (- n-bef past-phys)))
                 ;; Future lines: walk forward
                 (future-lines
                  (let (acc (budget fut-budget))
                    (cl-loop for i from (1+ cur-idx) below n
                             for ltext = (emms-lyrics-sync-line-text (aref lines i))
                             for lphys = (emms-lyrics-sync-display--count-wrapped-lines ltext)
                             while (and (> budget 0) (<= lphys budget))
                             do (push (aref lines i) acc)
                                (cl-decf budget lphys))
                    (nreverse acc)))
                 (future-phys
                  (cl-loop for l in future-lines
                           sum (emms-lyrics-sync-display--count-wrapped-lines
                                (emms-lyrics-sync-line-text l))))
                 (bot-pad (max 0 (- fut-budget future-phys))))
            ;; Top padding
            (dotimes (_ top-pad) (insert "\n"))
            ;; Past lines
            (dolist (line past-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-line-text line)
               'emms-lyrics-sync-past-line-face))
            ;; Blank before current
            (insert "\n")
            ;; Current line
            (if (emms-lyrics-sync-line-words cur-line)
                (setq emms-lyrics-sync-display--word-positions
                      (emms-lyrics-sync-display--render-a2-line cur-line))
              (emms-lyrics-sync-display--wrap-and-insert
               cur-text 'emms-lyrics-sync-current-line-face))
            ;; Blank after current
            (insert "\n")
            ;; Future lines
            (dolist (line future-lines)
              (emms-lyrics-sync-display--wrap-and-insert
               (emms-lyrics-sync-line-text line)
               'emms-lyrics-sync-future-line-face))
            ;; Bottom padding
            ;; ── PAREN FIX: this closes 6 levels after dotimes:
            ;;   1. normal-path let*   2. if   3. inner let* (lines/n/cur-idx)
            ;;   4. (t ...) branch     5. cond  6. outer let* (n-bef/n-aft)
            (dotimes (_ bot-pad) (insert "\n")))))))))

;;; ── Overlay Management ───────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--ensure-word-overlay ()
  "Return the word-highlight overlay, creating or recreating it if needed.
Priority 100 overrides all font-lock text-property faces."
  (let ((buf emms-lyrics-sync-display--buffer))
    (when (or (not (overlayp emms-lyrics-sync-display--word-overlay))
              (not (eq (overlay-buffer emms-lyrics-sync-display--word-overlay)
                       buf)))
      (when (overlayp emms-lyrics-sync-display--word-overlay)
        (ignore-errors (delete-overlay emms-lyrics-sync-display--word-overlay)))
      (setq emms-lyrics-sync-display--word-overlay
            (make-overlay 1 1 buf t nil))
      (overlay-put emms-lyrics-sync-display--word-overlay
                   'face 'emms-lyrics-sync-word-current-face)
      (overlay-put emms-lyrics-sync-display--word-overlay 'priority 100))
    emms-lyrics-sync-display--word-overlay))

(defun emms-lyrics-sync-display--reset-word-overlay ()
  "Collapse the word-highlight overlay to (1,1) before a lyrics redraw.
Prevents stale yellow highlights from painting over rewritten buffer content
between the delete-region call and the next update-word-overlays tick."
  (when (overlayp emms-lyrics-sync-display--word-overlay)
    (ignore-errors
      (move-overlay emms-lyrics-sync-display--word-overlay
                    1 1 emms-lyrics-sync-display--buffer))))

(defun emms-lyrics-sync-display--clear-sung-overlays ()
  "Delete all sung-word overlays."
  (mapc #'delete-overlay emms-lyrics-sync-display--sung-overlays)
  (setq emms-lyrics-sync-display--sung-overlays nil))

(defun emms-lyrics-sync-display--update-word-overlays (pos-ms)
  "Reposition current-word and sung-word overlays for POS-MS."
  (when (and emms-lyrics-sync-display--word-positions
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((ov           (emms-lyrics-sync-display--ensure-word-overlay))
            (found-cursor nil))
        (emms-lyrics-sync-display--clear-sung-overlays)
        (cl-loop
         for triple across emms-lyrics-sync-display--word-positions
         for buf-start = (nth 0 triple)
         for buf-end   = (nth 1 triple)
         for word      = (nth 2 triple)
         for w-start   = (emms-lyrics-sync-word-start-ms word)
         for w-end     = (or (emms-lyrics-sync-word-end-ms word)
                             most-positive-fixnum)
         do
         (cond
          ((and (>= pos-ms w-start) (< pos-ms w-end) (not found-cursor))
           (setq found-cursor t)
           (move-overlay ov buf-start buf-end
                         emms-lyrics-sync-display--buffer))
          ((< w-end pos-ms)
           (let ((sung (make-overlay buf-start buf-end
                                    emms-lyrics-sync-display--buffer)))
             (overlay-put sung 'face 'emms-lyrics-sync-word-sung-face)
             (overlay-put sung 'priority 90)
             (push sung emms-lyrics-sync-display--sung-overlays)))))
        ;; Collapse cursor overlay when no word matches (before first word,
        ;; or after last word) — prevents stale highlight.
        (unless found-cursor
          (move-overlay ov 1 1 emms-lyrics-sync-display--buffer))))))

;;; ── Elapsed Time Incremental Update ─────────────────────────────────────────

(defun emms-lyrics-sync-display--update-elapsed (elapsed-s)
  "Replace only the elapsed-time text in the header for ELAPSED-S seconds.
Marker invariant:
  elapsed-marker     NIL-type → stays at start of elapsed text.
  elapsed-end-marker T-type   → advances past newly inserted text."
  (when (and (markerp emms-lyrics-sync-display--elapsed-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-marker)
             (markerp emms-lyrics-sync-display--elapsed-end-marker)
             (marker-buffer emms-lyrics-sync-display--elapsed-end-marker)
             (buffer-live-p emms-lyrics-sync-display--buffer))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (let ((inhibit-read-only t)
            (new-str (emms-lyrics-sync-display--format-time elapsed-s)))
        (save-excursion
          (let ((start (marker-position emms-lyrics-sync-display--elapsed-marker))
                (end   (marker-position emms-lyrics-sync-display--elapsed-end-marker)))
            (when (and (integerp start) (integerp end) (<= start end))
              (goto-char start)
              (delete-region start end)
              (insert (propertize new-str
                                  'face 'emms-lyrics-sync-elapsed-face)))))))))

(provide 'emms-lyrics-sync-display-render)
;;; emms-lyrics-sync-display-render.el ends here
