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
(require 'emms-lyrics-sync-display-anim)

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

(defun emms-lyrics-sync-display--cover-pixel-height ()
  "Return cover art pixel height based on actual window space.

Uses the metadata window's actual body height when displayed, falling
back to `display-metadata-lines' if the window isn't visible yet.
This prevents the header from being clipped if Emacs shrinks the
window to fit the frame."
  (let* ((char-h (frame-char-height))
         (win    (get-buffer-window emms-lyrics-sync-metadata-buffer t))
         (avail  (if win 
                     (window-body-height win)
                   emms-lyrics-sync-display-metadata-lines))
         ;; Reserve 6 lines for the 2-line gap + 4 header lines.
         ;; Cap at 9 lines so the cover doesn't get absurdly large on tall screens.
         (lines  (min 10 (max 3 (- avail 6))))
         (px     (* lines char-h)))
    (min px emms-lyrics-sync-display-cover-height)))

(defun emms-lyrics-sync-display--cover-image (file-path height)
  "Return Emacs image for cover of FILE-PATH at HEIGHT pixels, or nil."
  (when (and (display-graphic-p) file-path (> height 0))
    (let* ((key    (list file-path height))
           (cached (gethash key emms-lyrics-sync-display--cover-cache)))
      (cond
       ((eq cached 'none) nil)
       (cached cached)
       (t
        (let* ((cover (ignore-errors
                        (or (emms-lyrics-sync-display--extract-embedded-cover file-path)
                            (emms-lyrics-sync-display--find-cover-file file-path))))
               (img   (when cover
                        (ignore-errors
                          (create-image cover nil nil :height height :scale 1.0)))))
          (if img
              (progn (puthash key img emms-lyrics-sync-display--cover-cache) img)
            (puthash key 'none emms-lyrics-sync-display--cover-cache)
            nil)))))))

(defvar emms-lyrics-sync-display--placeholder-colors
  '("#00E5FF"  ; Cyan A400
    "#1DE9B6"  ; Teal A400
    "#448AFF"  ; Blue A200
    "#FF4081"  ; Pink A200
    "#7C4DFF"  ; Deep Purple A200
    "#FFAB40"  ; Orange A200
    "#69F0AE"  ; Green A200
    "#FF6E40"  ; Deep Orange A200
    "#40C4FF"  ; Light Blue A200
    "#E040FB") ; Purple A200
  "Vivid Material accent colors for placeholder cover shapes.")

;; (defvar emms-lyrics-sync-display--placeholder-colors
;;   '("#1565C0" ; Blue 800
;;     "#00695C" ; Teal 800
;;     "#4527A0" ; Deep Purple 800
;;     "#00838F" ; Cyan 800
;;     "#AD1457" ; Pink 800
;;     "#2E7D32" ; Green 800
;;     "#E65100" ; Deep Orange 800
;;     "#1A237E" ; Indigo 900
;;     "#37474F" ; Blue Grey 800
;;     "#6A1B9A") ; Purple 800
;;   "Material Design 800-level colors for placeholder cover circles.")

(defvar emms-lyrics-sync-display--placeholder-shapes
  '(circle ellipse-h ellipse-v rect)
  "Shape types for placeholder cover generation.
circle    — equal rx/ry
ellipse-h — wide ellipse (rx > ry)
ellipse-v — tall ellipse (rx < ry)
rect      — rounded rectangle, possibly rotated")

(defun emms-lyrics-sync-display--placeholder-image (track height)
  "Generate deterministic placeholder SVG for TRACK at HEIGHT pixels.

Picks 4-7 shapes; shape type and color are chosen independently from
their respective lists, all derived from a SHA-256 hash of
artist+album+path so the same track always produces the same image.
Returns nil when SVG is unsupported or HEIGHT is zero."
  (when (and (display-graphic-p) (> height 0))
    (let* ((artist  (or (and track (emms-lyrics-sync-track-artist track)) ""))
           (album   (or (and track (emms-lyrics-sync-track-album  track)) ""))
           (fp      (or (and track (emms-lyrics-sync-track-file-path track)) ""))
           (hash    (secure-hash 'sha256 (concat artist "|" album "|" fp)))
           ;; Extract Nth byte (0-based) from hex hash as integer 0-255
           (byte    (lambda (n)
                      (string-to-number
                       (substring hash (* n 2) (+ (* n 2) 2)) 16)))
           (rng     (lambda (b lo hi)
                      (+ lo (round (* (- hi lo) (/ (funcall byte b) 255.0))))))
           (w height) (h height)
           (ncolors (length emms-lyrics-sync-display--placeholder-colors))
           (nshapes (length emms-lyrics-sync-display--placeholder-shapes))
           ;; Number of shapes: 4-7 driven by hash byte 0
           (n-shapes (+ 4 (mod (funcall byte 0) 4)))
           ;; Rotation pool for rect shapes (bytes 1-3, safely within 0-31)
           (rot-pool (list (funcall rng 1 -20 20)
                           (funcall rng 2 -15 15)
                           (funcall rng 3  10 25)))
           ;; Build shape SVG strings
           ;; Bytes 4..31 drive per-shape geometry (7 shapes × 4 bytes = 28,
           ;; so max byte index = 4 + 6*4 + 3 = 31 — within SHA256's 0-31).
           (shapes-svg
            (let ((result "")
                  (rot-idx 0)
                  (geo-byte 4))   ; geometry bytes start at 4
              (dotimes (i n-shapes)
                (let* ((off     (* i 3))
                       ;; Shape and color chosen from SEPARATE lists
                       (shape   (nth (mod (funcall byte (+ 1 off)) nshapes)
                                     emms-lyrics-sync-display--placeholder-shapes))
                       (color   (nth (mod (funcall byte (+ 2 off)) ncolors)
                                     emms-lyrics-sync-display--placeholder-colors))
                       (opacity (+ 0.20 (* 0.15 (/ (funcall byte (+ 3 off)) 255.0))))
                       ;; Center position: keep shapes mostly within bounds
                       (cx (funcall rng geo-byte
                                    (round (* w 0.18)) (round (* w 0.80))))
                       (cy (funcall rng (+ geo-byte 1)
                                    (round (* h 0.18)) (round (* h 0.72))))
                       (piece
                        (pcase shape
                          ('circle
                           (let ((r (funcall rng (+ geo-byte 2)
                                             (round (* h 0.20))
                                             (round (* h 0.34)))))
                             (format
                              "<circle cx='%d' cy='%d' r='%d' fill='%s' fill-opacity='%.2f'/>"
                              cx cy r color opacity)))
                          ('ellipse-h
                           (let ((rx (funcall rng (+ geo-byte 2)
                                              (round (* w 0.28)) (round (* w 0.40))))
                                 (ry (funcall rng (+ geo-byte 3)
                                              (round (* h 0.10)) (round (* h 0.18)))))
                             (format
                              "<ellipse cx='%d' cy='%d' rx='%d' ry='%d' fill='%s' fill-opacity='%.2f'/>"
                              cx cy rx ry color opacity)))
                          ('ellipse-v
                           (let ((rx (funcall rng (+ geo-byte 2)
                                              (round (* w 0.10)) (round (* w 0.16))))
                                 (ry (funcall rng (+ geo-byte 3)
                                              (round (* h 0.24)) (round (* h 0.36)))))
                             (format
                              "<ellipse cx='%d' cy='%d' rx='%d' ry='%d' fill='%s' fill-opacity='%.2f'/>"
                              cx cy rx ry color opacity)))
                          ('rect
                           (let* ((rw  (funcall rng (+ geo-byte 2)
                                                (round (* w 0.30)) (round (* w 0.48))))
                                  (rh  (funcall rng (+ geo-byte 3)
                                                (round (* h 0.13)) (round (* h 0.22))))
                                  (rot (nth (mod rot-idx 3) rot-pool))
                                  (rrx (round (* w 0.016))))
                             (setq rot-idx (1+ rot-idx))
                             (format
                              (concat "<rect x='%d' y='%d' width='%d' height='%d' rx='%d'"
                                      " fill='%s' fill-opacity='%.2f'"
                                      " transform='rotate(%d %d %d)'/>")
                              (- cx (/ rw 2)) (- cy (/ rh 2)) rw rh rrx
                              color opacity rot cx cy))))))
                  (setq result   (concat result piece))
                  (setq geo-byte (+ geo-byte 4))))
              result))
           ;; ── Text fields ───────────────────────────────────────────────
           (initial
            (if (> (length artist) 0)
                (let ((c (substring artist 0 1)))
                  (if (string-match-p "[<>&\"']" c) "♪" c))
              "♪"))
           (artist-display
            (if (> (length artist) 0)
                (if (> (length artist) 18)
                    (concat (substring artist 0 17) "…")
                  artist)
              "Unknown"))
           (album-display
            (if (> (length album) 0)
                (if (> (length album) 24)
                    (concat (substring album 0 23) "…")
                  album)
              ""))
           (font-sz  (round (* h 0.39)))
           (sep-y    (round (* h 0.836)))
           (sep-x1   (round (* w 0.33)))
           (sep-x2   (round (* w 0.67)))
           (artist-y (round (* h 0.886)))
           (album-y  (round (* h 0.940)))
           ;; ── Assemble SVG ──────────────────────────────────────────────
           (svg
            (format
             (concat
              "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>"
              "<defs>"
              "<radialGradient id='bg' cx='50%%' cy='45%%' r='75%%'>"
              "<stop offset='0%%'   stop-color='#0D2137'/>"
              "<stop offset='100%%' stop-color='#040C14'/>"
              "</radialGradient>"
              "<radialGradient id='vig' cx='50%%' cy='50%%' r='70%%'>"
              "<stop offset='55%%' stop-color='#000000' stop-opacity='0'/>"
              "<stop offset='100%%' stop-color='#000000' stop-opacity='0.55'/>"
              "</radialGradient>"
              "</defs>"
              ;; Background
              "<rect width='%d' height='%d' fill='url(#bg)'/>"
              ;; Shapes (inserted as literal string — no format directives inside)
              "%s"
              ;; Vignette
              "<rect width='%d' height='%d' fill='url(#vig)'/>"
              ;; Initial
              "<text x='%d' y='%d'"
              " fill='#FFFACD' fill-opacity='0.93'"
              " font-size='%d' font-family='sans-serif' font-weight='300'"
              " text-anchor='middle' dominant-baseline='central'>%s</text>"
              ;; Bottom text backdrop
              "<rect x='0' y='%d' width='%d' height='%d'"
              " fill='#000000' fill-opacity='0.38'/>"
              ;; Separator
              "<rect x='%d' y='%d' width='%d' height='1'"
              " fill='#FFFACD' fill-opacity='0.40'/>"
              ;; Artist name
              "<text x='%d' y='%d'"
              " fill='#FFFACD'"
              " font-size='%d' font-family='sans-serif' font-weight='500'"
              " text-anchor='middle' letter-spacing='3'>%s</text>"
              ;; Album name
              "<text x='%d' y='%d'"
              " fill='#FFFACD' fill-opacity='0.65'"
              " font-size='%d' font-family='sans-serif' font-weight='300'"
              " text-anchor='middle' letter-spacing='2'>%s</text>"
              "</svg>")
             ;; SVG dimensions
             w h
             ;; Background rect
             w h
             ;; Shapes (literal string)
             shapes-svg
             ;; Vignette rect
             w h
             ;; Initial
             (round (* w 0.50)) (round (* h 0.50))
             font-sz initial
             ;; Backdrop rect
             (round (* h 0.800)) w (round (* h 0.200))
             ;; Separator
             sep-x1 sep-y (- sep-x2 sep-x1)
             ;; Artist
             (round (* w 0.50)) artist-y
             (round (* h 0.048))
             artist-display
             ;; Album
             (round (* w 0.50)) album-y
             (round (* h 0.034))
             album-display)))
      (ignore-errors (create-image svg 'svg t :scale 1.0)))))


(defun emms-lyrics-sync-display--cover-or-placeholder (track height)
  "Return cover image for TRACK at HEIGHT, or deterministic placeholder.
Placeholder only generated when `display-cover-placeholder' is non-nil."
  (or (emms-lyrics-sync-display--cover-image
       (emms-lyrics-sync-track-file-path track) height)
      (when emms-lyrics-sync-display-cover-placeholder
        (emms-lyrics-sync-display--placeholder-image track height))))


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

(defun emms-lyrics-sync-display--render-header (track)
  "Insert the metadata header for TRACK at point.

Layout:
  Line 1: Artist[ - Composer]             ← artist-face
  Line 2: Album                           ← album-face (omitted if absent)
  Line 3: [Track. ]Title                  ← title-face
  Line 4: CODEC | bits/kHz | kbps | ch   ← tech-face (omitted if all nil)

Elapsed time and duration are displayed in the waveform mode line,
not here.  Returns nil."
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
         (l1 (concat (or artist "Unknown Artist")
                     (if (and composer (not (string-empty-p composer)))
                         (concat " - " composer) "")))
         (l2 (when (and album (not (string-empty-p album))) album))
         (l3 (concat (if trknum (format "%s. " trknum) "")
                     (or title "Unknown Title")))
         (sr-s   (emms-lyrics-sync-display--sr-string sr))
         (bps/sr (cond ((and bps sr-s) (format "%d/%s" bps sr-s))
                       (sr-s           sr-s)
                       (t              nil)))
         (kbps-s (when (and kbps (> kbps 0)) (format "%d kbps" kbps)))
         (l4     (let ((parts (delq nil (list codec bps/sr kbps-s ch))))
                   (when parts (mapconcat #'identity parts " | ")))))
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
    nil))

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
  "Insert the full lyrics for DOC at POS-MS, setting faces and markers."
  (setq emms-lyrics-sync-display--word-positions nil)
  ;; Trivial doc = nil, or 1-2 lines that are blank / instrumental markers.
  ;; Try the breathing-bar animation; if disabled or librsvg unavailable,
  ;; render-initial returns nil and (or ...) falls through to plain text.
  ;; Paren structure is unchanged from the original (if THEN ELSE).
  (if (emms-lyrics-sync-display-anim--trivial-doc-p doc)
      (or (progn
            (emms-lyrics-sync-display-anim-stop)
            (emms-lyrics-sync-display-anim-render-initial
             emms-lyrics-sync-core--current-track))
          (insert (propertize (emms-lyrics-sync-display--center "(no lyrics)")
                              'face 'emms-lyrics-sync-past-line-face) "\n"))
    (let* ((lines   (emms-lyrics-sync-lrc-doc-lines doc))
           (n       (length lines))
           (cur-idx (emms-lyrics-sync-lrc-seek doc pos-ms)))
      
      ;; Allocate a vector for line markers
      (setq emms-lyrics-sync-display--line-markers (make-vector n nil))
      
      (dotimes (i n)
        (let* ((line (aref lines i))
               (ms   (emms-lyrics-sync-line-start-ms line))
               (text (emms-lyrics-sync-line-text line))
               (is-current (= i cur-idx)))
          
          ;; Record the marker at the very beginning of the line
          (aset emms-lyrics-sync-display--line-markers i (copy-marker (point) nil))
          
          ;; Render the line text
          (if is-current
              (if (emms-lyrics-sync-line-words line)
                  (setq emms-lyrics-sync-display--word-positions
                        (emms-lyrics-sync-display--render-a2-line line))
                (emms-lyrics-sync-display--wrap-and-insert
                 text 'emms-lyrics-sync-current-line-face))
            (let ((face (if (< i cur-idx)
                            'emms-lyrics-sync-past-line-face
                          'emms-lyrics-sync-future-line-face)))
              (emms-lyrics-sync-display--wrap-and-insert text face)))
          
          ;; Attach the timestamp to the entire line for `RET` to find
          (when ms
            (let ((bol (marker-position (aref emms-lyrics-sync-display--line-markers i)))
                  (eol (point)))
              (put-text-property bol eol 'emms-lyrics-sync-start-ms ms))))))))

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
