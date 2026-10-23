;;; emms-lyrics-sync-display-vars.el --- Customization, faces, state variables and utilities  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-display-vars.el
;; Created : 2026-06-13 05:35 UTC
;; Updated : 2026-06-13 19:50 UTC
;; Changes : Bug 1 color fix — tech-face and new duration-face are now
;;           distinct from past-line-face (font-lock-comment-face grey).
;;
;;           Old: both used font-lock-comment-face → same grey as past lyrics
;;           New:
;;             tech-face     → #4db6ac  Material Teal 300
;;                             reads as "cool/technical metadata"
;;             duration-face → #80cbc4  Material Teal 200 (lighter)
;;                             complements tech-face and the teal elapsed face
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-display-font-family nil
  "Font family forced in the lyrics buffer, or nil to use the default.
When non-nil, all text in the *emms-lyrics* buffer is displayed in this
single family regardless of character script.  This prevents baseline
jitter between Latin and CJK characters when their fallback fonts have
different line heights.

Choose a font that is monospaced for both Latin and CJK at matched
vertical metrics, e.g. \"Sarasa Mono SC\" or \"Source Han Mono SC\".
A proportional CJK font (e.g. most 黑体 faces) will make Latin text
look poorly spaced.

Changes take effect on the next lyrics buffer creation — restart
Emacs (or kill the *emms-lyrics* buffer) after changing this value."
  :type '(choice (const :tag "Use default face" nil) string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cjk-font nil
  "Font family for CJK characters in the lyrics buffer, or nil.

When non-nil, CJK runs (detected via East Asian Width, i.e.
`char-width' = 2) receive a `:family'/`:height' face text property
pointing at this family.  Face properties take priority over the
buffer's base `:family' in Emacs font resolution, so Latin renders
with `display-font-family' and CJK with this family, each at an
independently controlled size.

Recommended pairing:
  (setq emms-lyrics-sync-display-font-family \"SauceCodePro Nerd Font\")
  (setq emms-lyrics-sync-display-cjk-font   \"FZYouH_504L\")
  (setq emms-lyrics-sync-display-cjk-scale  1.10)

Leave nil to use `display-font-family' for all scripts (recommended
when that family already covers Latin and CJK at matched vertical
metrics, e.g. Sarasa Mono SC or Source Han Mono)."
  :type '(choice (const :tag "Use display-font-family for all scripts" nil)
                 string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cjk-scale 1.10
  "Height multiplier for CJK text relative to the default face height.

Only used when `display-cjk-font' is non-nil.  Effective CJK height is
  (face-attribute 'default :height) × display-cjk-scale
which is an absolute pixel/point height in the same units as the
`default' face's :height attribute.

Tune visually: CJK glyphs often appear smaller than Latin at the same
nominal point size (smaller x-height relative to em-square), so a
scale slightly above 1.0 often renders at visual parity.  1.05-1.20
covers most font pairs."
  :type 'number
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-font-scale 1.0
  "Multiplier applied to the default face height in the lyrics buffer.

1.0 = same as the frame's default face.  0.85 = 15% smaller, useful on
small screens to fit more content (or a taller waveform) without
shrinking the cover further.  1.1 = 10% larger for readability on
high-DPI displays.

CJK text scales by `display-font-scale × display-cjk-scale', so
`display-cjk-scale' stays a *relative* multiplier against whatever
Latin size is in effect.  Reducing `display-font-scale' shrinks both
scripts proportionally; reducing only `display-cjk-scale' shrinks
only CJK.

The cover and waveform heights are computed from `frame-char-height',
which is independent of this option — font-scale only affects the
height of the lyric/header text, not the images.  To make room for a
taller waveform on a small screen, lower this and then raise
`emms-lyrics-sync-waveform-height'."
  :type 'number
  :group 'emms-lyrics-sync)

(defvar-local emms-lyrics-sync-display--base-height nil
  "Effective base face height (:height attribute) in the lyrics buffer.

Set by `emms-lyrics-sync-display--apply-font' to
  (face-attribute 'default :height) × display-font-scale
before `buffer-face-mode' remaps the default face.  Read by
`emms-lyrics-sync-display--apply-cjk-font' to compute CJK height as
  base-height × display-cjk-scale

Storing the value in a buffer-local avoids depending on whether
`face-attribute' consults `face-remapping-alist' after
buffer-face-mode installs a `:height' override — that behavior is
not specified and has varied across Emacs versions.")

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

(defcustom emms-lyrics-sync-display-lyrics-height 12
  "Total number of physical screen lines reserved for the lyrics area.
The lyrics section always occupies exactly this many lines regardless of
line wrapping or playback position.  The value should satisfy:
  context-before + context-after + 3 = lyrics-height
with the default values 3 + 6 + 3 = 12."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-update-interval 0.1
  "Seconds between timer ticks.  Controls karaoke word-highlight smoothness."
  :type  'number
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-side-padding 2
  "Character columns of padding on each side of the waveform image.

The image is generated at `(window-body-width - 2 × this)' character
columns wide and centered with `this' spaces of padding on the left;
the right side auto-fills with the remaining buffer space.  Set to 0
to have the image fill the body edge-to-edge.

Two characters (~26 px at typical font sizes) provides visible
breathing room from the window edges without materially reducing
waveform resolution."
  :type 'integer
  :group 'emms-lyrics-sync)

;;; ── Faces ────────────────────────────────────────────────────────────────────
;;
;; IMPORTANT: Do NOT add :weight bold or :height to faces used in header
;; lines centered via string-width.  Both scale character pixel width in
;; GUI Emacs, causing text to render wider than string-width reports.
;; Use colour alone for visual emphasis.

(defface emms-lyrics-sync-artist-face
  '((t :inherit font-lock-function-name-face))
  "Face for the Artist[ - Composer] header line (line 1)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-album-face
  '((t :inherit font-lock-keyword-face))
  "Face for the Album header line (line 2, shown only when album is available)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-title-face
  '((t :inherit font-lock-string-face))
  "Face for the [Track. ]Title header line (line 3)."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-tech-face
  '((((background dark))  :foreground "#4db6ac")
    (((background light)) :foreground "#00796b"))
  "Face for the CODEC | bits/kHz | kbps | channels info line (line 4).
Material Teal 300 — clearly distinct from past-line-face (comment grey),
reads as cool/technical metadata without competing with the amber current line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-elapsed-face
  '((t :inherit font-lock-constant-face))
  "Face for the elapsed-time portion of the time line (e.g. \"0:18\")."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-duration-face
  '((((background dark))  :foreground "#80cbc4")
    (((background light)) :foreground "#00897b"))
  "Face for the duration portion of the time line (e.g. \"/ 4:55\").
Material Teal 200 — lighter than tech-face, complements the teal elapsed
face while remaining clearly distinct from the grey past-line-face."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-past-line-face
  '((t :inherit font-lock-comment-face))
  "Face for lyrics lines that have already played."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-current-line-face
  '((((background dark))  :foreground "#ffcb6b")
    (((background light)) :foreground "#b36a00"))
  "Face for the currently playing lyrics line.
Warm amber — no bold weight so string-width centering stays accurate."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-future-line-face
  '((t))
  "Face for upcoming lyrics lines.

Empty spec: the face falls through to the buffer's effective
`default' for all attributes.  This is intentionally *not*
`(:inherit default)' — an explicit `:inherit default' resolves
against the frame's default face, bypassing the buffer-local
`face-remapping-alist' height override installed by `--apply-font'.
An empty spec cascades implicitly through the remapped default, so
future lines pick up `display-font-scale' at the same height as
current and past lines."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-sung-face
  '((((background dark))  :foreground "#c3e88d")
    (((background light)) :foreground "#2d7a2d"))
  "Face for already-sung words within the current A2 karaoke line."
  :group 'emms-lyrics-sync)

(defface emms-lyrics-sync-word-current-face
  '((((background dark))  :foreground "#ffdd00" :underline t)
    (((background light)) :foreground "#cc6600" :underline t))
  "Face for the word currently being sung (A2 karaoke highlight)."
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-display--buffer nil
  "The lyrics display buffer.")

(defvar emms-lyrics-sync-display--timer nil
  "The 100ms display-update timer.")

(defvar emms-lyrics-sync-display--last-line-idx -1
  "Line index last used for a lyrics redraw.")

(defvar emms-lyrics-sync-display--word-overlay nil
  "Overlay used for A2 current-word highlighting.")

(defvar emms-lyrics-sync-display--sung-overlays nil
  "List of overlays for already-sung words in the current A2 line.")

(defvar emms-lyrics-sync-display--elapsed-marker nil
  "NIL-type marker at the START of the elapsed-time text in the header.")

(defvar emms-lyrics-sync-display--elapsed-end-marker nil
  "T-type marker at the END of the elapsed-time text in the header.")

(defvar emms-lyrics-sync-display--lyrics-marker nil
  "NIL-type marker at the start of the lyrics section.")

(defvar emms-lyrics-sync-display--current-file-path nil
  "File path of the track currently rendered in the lyrics buffer.")

(defvar emms-lyrics-sync-display--tick-counter 0
  "Counter incremented every display tick.")

(defvar emms-lyrics-sync-display--word-positions nil
  "Vector of (buf-start buf-end emms-lyrics-sync-word) triples.")

(defvar emms-lyrics-sync-display--cover-cache (make-hash-table :test #'equal)
  "Cache: file-path → Emacs image object.")

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
  "Return the usable body width of the lyrics window in characters.
`window-body-width' already excludes fringes and margins, so no
additional subtraction is needed."
  (let ((win (emms-lyrics-sync-display--window)))
    (if win (window-body-width win) 60)))

(defun emms-lyrics-sync-display--center (str)
  "Return STR padded with spaces to center it in the lyrics window."
  (let* ((width (emms-lyrics-sync-display--body-width))
         (len   (string-width str))
         (pad   (max 0 (ceiling (- width len) 2))))   ; ← was floor
    (concat (make-string pad ?\s) str)))
(defun emms-lyrics-sync-display--center (str)
  (let* ((width (emms-lyrics-sync-display--body-width))
         (len   (string-width str))
         (pad   (max 0 (/ (- width len) 2))))
    (concat (make-string pad ?\s) str)))

;; In emms-lyrics-sync-display-vars.el
(defcustom emms-lyrics-sync-display-cover-lines 10
  "Target cover art height in character lines.
Effective pixel height is `cover-lines × frame-char-height', clamped by
`emms-lyrics-sync-display-cover-height' (hard upper bound) and by the
space available in the window after reserving room for the header,
lyrics, and waveform.  This makes the cover occupy the same visual
proportion on every machine regardless of DPI or scaling mode."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-cover-height 900
  "Hard upper bound on cover art pixel height.  Rarely reached.
The effective height is min(cover-height, cover-lines × char-height,
available-lines × char-height).  Raise this only if you want a cover
taller than `cover-lines' would allow."
  :type  'integer
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-display-fontset nil
  "Fontset name applied in the lyrics buffer, or nil.

A fontset maps character ranges to specific fonts, letting you use
different fonts for Latin and CJK text within the same buffer.  Each
font in the fontset can be specified at its own size, so their
vertical metrics can be matched.  Use this when a single font (via
`emms-lyrics-sync-display-font-family') renders Latin or CJK text in a
way you dislike.

Example — SauceCodePro for Latin, FZYouHei for CJK, tuned heights:

  (create-fontset-from-fontset-spec
   \"-*-SauceCodePro Nerd Font-*-*-*-*-21-*-*-*-*-*-fontset-lyrics\")
  (set-fontset-font \"fontset-lyrics\" 'han
                    (font-spec :family \"FZYouH_504L\" :height 220))
  (setq emms-lyrics-sync-display-fontset \"fontset-lyrics\")

Takes precedence over `emms-lyrics-sync-display-font-family'.
Call `emms-lyrics-sync-display-refresh-font' after changing this."
  :type '(choice (const :tag "Use default" nil) string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-waveform-buffer-name "*emms-lyrics-waveform*"
  "Name of the dedicated waveform buffer.

The waveform lives in its own side window below the lyrics window,
in a fixed-height, fixed-position viewport.  Because the window
height is fixed, the waveform image cannot move vertically as lyric
lines of different heights scroll past above it.  This decouples
CJK font scaling (`display-cjk-scale') from line-height jitter: the
scale becomes a pure aesthetic choice, unbounded by integer-pixel
rounding thresholds."
  :type 'string
  :group 'emms-lyrics-sync)

(defvar emms-lyrics-sync-waveform-buffer nil
  "The waveform display buffer.")

(defcustom emms-lyrics-sync-waveform-center-line 1
  "Thickness of the center line in pixels.
A thin line helps visualize silence without overpowering quiet sections.
1 is a standard hairline; 2 is more visible on high-DPI screens."
  :type 'integer
  :group 'emms-lyrics-sync)

(defvar-local emms-lyrics-sync-waveform--cached-mode-line "  0:00  "
  "Cached mode line string, updated on each waveform redraw.
Persists after playback stops so the mode line stays informative
even after `emms-lyrics-sync-core--on-stop' clears `current-track'.")

;;; ── CJK font application ─────────────────────────────────────────────────────

(defun emms-lyrics-sync-display--cjk-char-p (char)
  "Return non-nil if CHAR should use the CJK font.

Uses `char-width' (East Asian Width), which returns 2 for kanji,
kana, hangul, bopomofo, fullwidth forms, and CJK punctuation.
More reliable than `get-char-code-property' with 'script, which
does not return script names in any Emacs build: script data comes
from Scripts.txt, not UnicodeData.txt, and is not in
`char-code-property-table'."
  (and (characterp char) (= 2 (char-width char))))

(defun emms-lyrics-sync-display--apply-cjk-font (start end)
  "Apply `display-cjk-font' as a face text property to CJK runs in [START, END).
Face text properties honor both :family and :height and merge cleanly
with existing face attributes via `add-face-text-property'.  Height
is `emms-lyrics-sync-display--base-height' × `display-cjk-scale',
where the base reflects `display-font-scale' set by `--apply-font'.
No-op when `display-cjk-font' is nil."
  (when (and emms-lyrics-sync-display-cjk-font
             (integerp start)
             (integerp end)
             (< start end))
    (let* ((base-h (or emms-lyrics-sync-display--base-height
                       (face-attribute 'default :height)))
           (scale  (or emms-lyrics-sync-display-cjk-scale 1.10))
           (cjk-h  (round (* base-h scale)))
           (spec   (list :family emms-lyrics-sync-display-cjk-font
                         :height cjk-h))
           (pos    start)
           (run-st nil))
      (save-excursion
        (goto-char start)
        (while (< pos end)
          (if (emms-lyrics-sync-display--cjk-char-p (char-after pos))
              (unless run-st (setq run-st pos))
            (when run-st
              (add-face-text-property run-st pos spec 'append)
              (setq run-st nil)))
          (setq pos (1+ pos)))
        (when run-st
          (add-face-text-property run-st end spec 'append))))))

;;; ── Font x-height calibration ────────────────────────────────────────────────

(defun emms-lyrics-sync-display--decode-font-path (path)
  "Return PATH as a multibyte string using `file-name-coding-system'.

`font-info' returns the file path in the raw bytes the font backend
produced — a unibyte string for paths containing non-ASCII characters.
Unibyte strings with high bytes display as octal escapes in `%s',
`format', and `message', making log output unreadable and breaking
any consumer that treats the value as text (e.g. embedding it in a
process argument via `%S').

Decodes with `file-name-coding-system' (falling back to UTF-8 on
Linux/macOS) so the return value round-trips cleanly through
`file-readable-p', `find-file', `message', and `call-process' — the
same path Emacs itself uses for reading files."
  (cond
   ((not (stringp path)) path)
   ((multibyte-string-p path) path)
   (t (decode-coding-string path (or file-name-coding-system 'utf-8)))))

(defun emms-lyrics-sync-display--font-file-for-family (family &optional weight slant)
  "Return the installed font file path for FAMILY, or nil.

WEIGHT and SLANT narrow the match; both default to whatever `find-font'
selects for FAMILY alone.  For families that ship many variants,
specifying at least SLANT avoids surprising picks — SauceCodePro Nerd
Font Mono, for example, resolves LightItalic before Light when only
WEIGHT is given, because fontconfig's default slant preference for that
family is italic.

SLANT symbols: `normal' (= roman), `italic', `oblique'.
WEIGHT symbols: `thin', `ultra-light', `light', `semi-light', `book',
                `regular', `medium', `semi-bold', `bold', `ultra-bold',
                `heavy', `ultra-heavy'.

Returns a multibyte string (decoded via `file-name-coding-system') so
the value is safe for `message', `format %s', and process arguments."
  (when family
    (let* ((props (append (list :family family)
                          (when weight (list :weight weight))
                          (when slant  (list :slant  slant))))
           (spec (apply #'font-spec props))
           (ent  (find-font spec)))
      (when ent
        (let ((info (ignore-errors (font-info ent))))
          (cond
           ((and (vectorp info)
                 (>= (length info) 12)
                 (stringp (aref info 11))
                 (file-readable-p (aref info 11)))
            (emms-lyrics-sync-display--decode-font-path (aref info 11)))
           ((vectorp info)
            (cl-loop for x across info
                     when (and (stringp x) (file-regular-p x))
                     return (emms-lyrics-sync-display--decode-font-path x)))
           (t nil)))))))

(defun emms-lyrics-sync-display--measure-x-height (font-file char &optional px-size)
  "Return x-height of CHAR in FONT-FILE at PX-SIZE pixels.

Shells out to python3 with freetype-py.  PX-SIZE defaults to 400:
at 21px a single hinting pixel is ~5% of the measurement, so the
ratio swings by several percent between adjacent sizes.  At 400px
the ratio is stable.  Also uses FT_LOAD_NO_HINTING in Python.

The font path is passed via sys.argv rather than embedded in the
Python source.  `font-info' returns the path as a unibyte string
containing raw UTF-8 bytes; embedding it with `%S' produces octal
escapes that Python interprets as Latin-1 codepoints, mangling the
path.  Passing through argv (after decoding to multibyte) lets
`call-process' emit the original bytes to `execvp' correctly.

Returns a float or nil if python3, freetype-py, or the font file is
unavailable."
  (unless px-size (setq px-size 400))
  (when (and (executable-find "python3")
             (stringp font-file)
             (file-readable-p font-file))
    (let* ((code "\
import sys
try:
    import freetype
except ImportError:
    sys.exit(2)
try:
    face = freetype.Face(sys.argv[1])
    face.set_pixel_sizes(0, int(sys.argv[3]))
    face.load_char(sys.argv[2], freetype.FT_LOAD_NO_HINTING)
    print(face.glyph.metrics.height / 64.0)
except Exception as e:
    print(e, file=sys.stderr)
    sys.exit(3)
")
           ;; Decode unibyte path to proper multibyte before passing
           ;; through call-process, which will re-encode to file-name-
           ;; coding-system (UTF-8 on Linux) when constructing argv.
           (font-file (if (multibyte-string-p font-file)
                          font-file
                        (decode-coding-string font-file 'utf-8))))
      (with-temp-buffer
        (when (zerop (ignore-errors
                       (call-process "python3" nil t nil "-c" code
                                     font-file
                                     (char-to-string char)
                                     (number-to-string px-size))))
          (string-to-number (string-trim (buffer-string))))))))

(defun emms-lyrics-sync-display--compute-cjk-scale (latin-file cjk-file)
  "Return x-height ratio LATIN-FILE / CJK-FILE, or nil on measurement failure.

Values < 1.0 mean the CJK font's x-height is taller at the same
nominal size (typical for FZYouHei vs SauceCodePro); values > 1.0
mean it is shorter.  Multiply by a perceptual factor > 1.0 to add a
visual bump beyond pure x-height parity."
  (let ((lx (emms-lyrics-sync-display--measure-x-height latin-file ?x 400))
        (cx (emms-lyrics-sync-display--measure-x-height cjk-file   ?x 400)))
    (when (and lx cx (> lx 0) (> cx 0))
      (/ lx (float cx)))))

;;;###autoload
(defun emms-lyrics-sync-display-calibrate-cjk-scale (&optional latin-file cjk-file)
  "Measure Latin and CJK font x-heights and set `display-cjk-scale'.

Files default to those resolved from `display-font-family' and
`display-cjk-font'.  Requires freetype-py:
  pip install --user freetype-py

Prints the measured ratio.  The ratio is the *mathematical* x-height
match, not necessarily the visually-preferred value — CJK text often
reads smaller than Latin even at matched x-height (lower stroke
density per unit area), so many users multiply by 1.05–1.15:

  (setq emms-lyrics-sync-display-cjk-scale
        (* 1.10 (emms-lyrics-sync-display--compute-cjk-scale L C)))

Run M-x emms-lyrics-sync-display-refresh-font after setting manually."
  (interactive)
  (let* ((latin-file (or latin-file
                         (emms-lyrics-sync-display--font-file-for-family
                          emms-lyrics-sync-display-font-family)))
         (cjk-file   (or cjk-file
                         (emms-lyrics-sync-display--font-file-for-family
                          emms-lyrics-sync-display-cjk-font))))
    (unless latin-file
      (user-error "Cannot resolve Latin font file for %S"
                  emms-lyrics-sync-display-font-family))
    (unless cjk-file
      (user-error "Cannot resolve CJK font file for %S"
                  emms-lyrics-sync-display-cjk-font))
    (let ((scale (emms-lyrics-sync-display--compute-cjk-scale
                  latin-file cjk-file)))
      (unless scale
        (user-error "Measurement failed (freetype-py installed?)"))
      (setq emms-lyrics-sync-display-cjk-scale scale)
      (emms-lyrics-sync-display-refresh-font)
      (message "cjk-scale = %.4f  (x-height match; latin=%s cjk=%s)"
               scale
               (file-name-nondirectory latin-file)
               (file-name-nondirectory cjk-file)))))

;;;###autoload
(defun emms-lyrics-sync-display-tune-cjk-scale (scale)
  "Set `display-cjk-scale' to SCALE and re-render lyrics immediately.

For interactive tuning: play a track, run this with different values
until the CJK/Latin size ratio looks right on the current machine.
Faster than restarting Emacs or waiting for the next lyric advance."
  (interactive "nNew cjk-scale (0.5-2.0): ")
  (unless (and (numberp scale) (> scale 0.3) (< scale 3.0))
    (user-error "cjk-scale should be between 0.3 and 3.0"))
  (setq emms-lyrics-sync-display-cjk-scale scale)
  (when (buffer-live-p emms-lyrics-sync-display--buffer)
    (emms-lyrics-sync-display--full-redraw))
  (message "cjk-scale = %s  (font: %s)"
           scale
           emms-lyrics-sync-display-cjk-font))

;;;###autoload
(defun emms-lyrics-sync-display-scan-cjk-scale (start end step)
  "Scan `display-cjk-scale' from START to END by STEP.

For each value, redraw the lyrics buffer and measure pixel deltas
between consecutive lyric lines.  A uniform-delta value means all
lyric lines render at the same height; a spread (max − min) of ≥ 2
indicates visible jitter.

Results are written to *emms-lyrics-scale-scan* with a verdict per
value:
  uniform    — all lyric lines same height (ideal, no jitter)
  ok (1px)   — 1px max difference; imperceptible
  borderline — 2px max difference; usually visible
  JITTER     — 3+ px difference; clearly visible
  NO-DATA    — no lyric lines measured; check buffer visibility

The original `display-cjk-scale' is restored after the scan; the
buffer is redrawn once more at the end to reflect the restored
state.  The scan pauses the display timer to avoid interference
from the 100 ms tick during measurement.

Requires a track playing with lyrics visible in a live window.

Usage:
  M-x emms-lyrics-sync-display-scan-cjk-scale RET
  Start: 1.05
  End:   1.20
  Step:  0.01

Or programmatically:
  (emms-lyrics-sync-display-scan-cjk-scale 1.05 1.20 0.01)"
  (interactive
   (list (read-number "Start cjk-scale: "
                      (or emms-lyrics-sync-display-cjk-scale 1.0))
         (read-number "End cjk-scale:   " 1.20)
         (read-number "Step:            " 0.01)))
  (unless (buffer-live-p emms-lyrics-sync-display--buffer)
    (user-error "Lyrics buffer not live; play a track first"))
  (unless (get-buffer-window emms-lyrics-sync-display--buffer t)
    (user-error "Lyrics buffer not displayed; \
run M-x emms-lyrics-sync-display-show first"))
  (unless (and (numberp start) (numberp end) (numberp step)
               (< start end) (> step 0))
    (user-error "Invalid scan range: start=%S end=%S step=%S"
                start end step))
  (let* ((orig-scale emms-lyrics-sync-display-cjk-scale)
         (iterations (1+ (floor (/ (- end start) step))))
         (results '()))
    (unwind-protect
        (progn
          (emms-lyrics-sync-display--stop-timer)
          (message "Scanning %d values from %.3f to %.3f (step %.3f)..."
                   iterations start end step)
          (dotimes (i iterations)
            (let* ((scale (+ start (* i step)))
                   (deltas (progn
                             (setq emms-lyrics-sync-display-cjk-scale scale)
                             (emms-lyrics-sync-display--full-redraw)
                             (redisplay t)
                             (sit-for 0.05)
                             (emms-lyrics-sync--measure-lyric-deltas))))
              (push (cons scale deltas) results)
              (message "Scanning %.3f (%d/%d)..."
                       scale (1+ i) iterations)))
          (setq results (nreverse results))
          (emms-lyrics-sync-display--show-scan-results
           start end step results))
      ;; Restore on any exit path
      (setq emms-lyrics-sync-display-cjk-scale orig-scale)
      (when (buffer-live-p emms-lyrics-sync-display--buffer)
        (emms-lyrics-sync-display--full-redraw))
      (emms-lyrics-sync-display--start-timer))))

(defun emms-lyrics-sync--measure-lyric-deltas ()
  "Return list of pixel deltas between consecutive lyric lines.

Restricts measurement to lines between `lyrics-marker' and
`waveform-marker' so cover-image transitions never appear in the
result.  Returns nil if the buffer is not displayed in a live window,
markers are not set, or fewer than 2 lyric lines are visible.

Line positions come from `posn-at-point' on the display window; the
caller should ensure the window has been redisplayed before calling."
  (when (and (buffer-live-p emms-lyrics-sync-display--buffer)
             (markerp emms-lyrics-sync-display--lyrics-marker)
             (marker-buffer emms-lyrics-sync-display--lyrics-marker)
             (markerp emms-lyrics-sync-display--waveform-marker)
             (marker-buffer emms-lyrics-sync-display--waveform-marker))
    (with-current-buffer emms-lyrics-sync-display--buffer
      (when-let* ((win (get-buffer-window (current-buffer) t))
                  (lyrics-start
                   (marker-position emms-lyrics-sync-display--lyrics-marker))
                  (waveform-start
                   (marker-position emms-lyrics-sync-display--waveform-marker))
                  ((< lyrics-start waveform-start)))
        (save-excursion
          (goto-char lyrics-start)
          (let ((prev-y nil)
                (deltas '()))
            (while (< (point) waveform-start)
              (let ((y (cdr (posn-x-y (posn-at-point (point) win)))))
                (when (and y prev-y)
                  (push (- y prev-y) deltas))
                (when y (setq prev-y y)))
              (forward-line 1))
            (nreverse deltas)))))))

(defun emms-lyrics-sync-display--show-scan-results (start end step results)
  "Populate *emms-lyrics-scale-scan* with a formatted scan table.
Helper for `emms-lyrics-sync-display-scan-cjk-scale'."
  (let* ((title (or (and emms-lyrics-sync-core--current-track
                         (emms-lyrics-sync-track-title
                          emms-lyrics-sync-core--current-track))
                    "(no track)"))
         (highest-uniform nil)
         (highest-ok nil)
         (buf (get-buffer-create "*emms-lyrics-scale-scan*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "cjk-scale scan: %.3f → %.3f (step %.3f)\n"
                        start end step))
        (insert (format "Track: %s\n"
                        title))
        (insert (format "char-height: %d\n\n"
                        (frame-char-height)))
        (insert "scale  n   min  max  spread  verdict\n")
        (insert "-----  --  ---  ---  ------  -------\n")
        (dolist (r results)
          (let* ((scale (car r))
                 (deltas (cdr r))
                 (n (length deltas))
                 (mn (and deltas (apply #'min deltas)))
                 (mx (and deltas (apply #'max deltas)))
                 (spread (and mn (- mx mn)))
                 (verdict (cond
                           ((null deltas)    "NO-DATA")
                           ((zerop spread)   "uniform")
                           ((= spread 1)     "ok (1px)")
                           ((= spread 2)     "borderline")
                           (t                "JITTER"))))
            (when (and spread (zerop spread))
              (setq highest-uniform scale))
            (when (and spread (<= spread 1))
              (setq highest-ok scale))
            (insert (format "%.3f  %2d  %3s  %3s  %6s  %s\n"
                            scale n
                            (or mn "-") (or mx "-")
                            (or spread "-") verdict))))
        (insert "\nLegend:\n")
        (insert "  uniform    — all lyric lines same pixel height (ideal)\n")
        (insert "  ok (1px)   — 1px max difference; imperceptible\n")
        (insert "  borderline — 2px max difference; usually visible\n")
        (insert "  JITTER     — 3+ px difference; clearly visible\n")
        (insert "  NO-DATA    — no lyric lines measured; check visibility\n")
        (insert "\nSummary:\n")
        (insert (format "  Highest uniform:     %s\n"
                        (if highest-uniform
                            (format "%.3f" highest-uniform)
                          "none")))
        (insert (format "  Highest ok (≤1px):   %s\n"
                        (if highest-ok
                            (format "%.3f" highest-ok)
                          "none")))
        (insert "\nNote: values are machine-specific.  frame-char-height\n")
        (insert "and integer pixel rounding determine the threshold.\n")
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(provide 'emms-lyrics-sync-display-vars)
;;; emms-lyrics-sync-display-vars.el ends here
