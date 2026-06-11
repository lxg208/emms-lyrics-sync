;;; emms-lyrics-sync-lrc.el --- LRC parser and timestamp engine  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-lrc.el
;; Created : 2026-06-11 21:32 UTC
;; Purpose : Parse LRC files (standard and A2 word-level extension) into
;;           sorted vectors of `emms-lyrics-sync-line' structs.  Provides
;;           O(log n) binary-search seek used by the display engine and
;;           the playback timer.  This module has no external dependencies
;;           and can be loaded and tested independently.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Supported LRC variants:
;;
;;   Standard     [mm:ss.cc] line text
;;   A2/Enhanced  [mm:ss.cc]<mm:ss.cc>word <mm:ss.cc>word...
;;   Multi-ts     [mm:ss.cc][mm:ss.cc] repeated chorus line
;;   Metadata     [ti:Title]  [ar:Artist]  [al:Album]  [offset:±ms]
;;
;; Fraction digits are normalised to ms regardless of how many digits are
;; present (1 = tenths → ×100, 2 = centiseconds → ×10, 3 = ms → ×1).
;;
;; Public API summary:
;;   `emms-lyrics-sync-lrc-parse'        — STRING → `emms-lyrics-sync-lrc-doc'
;;   `emms-lyrics-sync-lrc-seek'         — DOC POS-MS → line index (binary search)
;;   `emms-lyrics-sync-lrc-current-line' — DOC POS-MS → `emms-lyrics-sync-line' or nil
;;   `emms-lyrics-sync-lrc-context'      — DOC POS-MS N-BEFORE N-AFTER → alist
;;   `emms-lyrics-sync-lrc-synced-p'     — DOC → bool
;;   `emms-lyrics-sync-lrc-line-count'   — DOC → integer

;;; Code:

(require 'cl-lib)
(require 'subr-x)   ; string-trim, string-empty-p

;;; ── Data Structures ──────────────────────────────────────────────────────────

(cl-defstruct (emms-lyrics-sync-word
               (:constructor emms-lyrics-sync-word--make)
               (:copier nil))
  "One word entry from an A2 word-level LRC line."
  (start-ms nil :type (or null integer)
            :documentation "Word start in ms.")
  (end-ms   nil :type (or null integer)
            :documentation "Word end in ms; nil for the last word until post-processing.")
  (text     ""  :type string
            :documentation "Word text, may include trailing whitespace."))

(cl-defstruct (emms-lyrics-sync-line
               (:constructor emms-lyrics-sync-line--make)
               (:copier nil))
  "One display line from an LRC file."
  (start-ms nil :type (or null integer)
            :documentation "Line start in ms; nil for plain (unsynced) text lines.")
  (text     ""  :type string
            :documentation "Full line text.")
  (words    nil :type list
            :documentation "List of `emms-lyrics-sync-word'; non-nil only for A2 LRC lines."))

(cl-defstruct (emms-lyrics-sync-lrc-doc
               (:constructor emms-lyrics-sync-lrc-doc--make)
               (:copier nil))
  "Complete parsed LRC document returned by `emms-lyrics-sync-lrc-parse'."
  (lines     #()  :type vector
             :documentation "Vector of `emms-lyrics-sync-line', sorted ascending by start-ms.")
  (title     nil  :type (or null string))
  (artist    nil  :type (or null string))
  (album     nil  :type (or null string))
  (offset-ms 0    :type integer
             :documentation "Global offset applied to all timestamps (from [offset:...]).")
  (plain-p   nil  :type boolean
             :documentation "t when the file contained no timestamp tags at all."))

;;; ── Internal Regexps ─────────────────────────────────────────────────────────

(defconst emms-lyrics-sync-lrc--ts-content-re
  ;; Matches the content INSIDE [...] when it is a timestamp, e.g. "01:23.45"
  (rx bos
      (group (+ digit))          ; minutes
      ":"
      (group (+ digit))          ; seconds
      "."
      (group (repeat 1 3 digit)) ; fraction (1–3 digits)
      eos)
  "Regexp matching the content of a standard LRC timestamp tag (no brackets).")

(defconst emms-lyrics-sync-lrc--meta-content-re
  ;; Matches the content INSIDE [...] when it is a metadata tag, e.g. "ti:Song"
  (rx bos
      (group alpha (* (any alnum ?_ ?-))) ; key: must start with a letter
      ":"
      (group (* anything))               ; value
      eos)
  "Regexp matching the content of an LRC metadata tag (no brackets).")

(defconst emms-lyrics-sync-lrc--word-ts-re
  ;; Matches an A2 inline word timestamp, e.g. <01:23.45>
  (rx "<"
      (group (+ digit))          ; minutes
      ":"
      (group (+ digit))          ; seconds
      "."
      (group (repeat 1 3 digit)) ; fraction
      ">")
  "Regexp matching an A2 word-level timestamp tag.")

;;; ── Timestamp Conversion ─────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrc--ts-to-ms (min-s sec-s frac-s)
  "Convert timestamp string components MIN-S, SEC-S, FRAC-S to integer ms.
FRAC-S may be 1–3 characters; it is normalised to milliseconds."
  (let* ((frac-len (length frac-s))
         (frac-num (string-to-number frac-s))
         (frac-ms  (cond ((= frac-len 1) (* frac-num 100))
                         ((= frac-len 2) (* frac-num 10))
                         (t              frac-num))))
    (+ (* (string-to-number min-s) 60000)
       (* (string-to-number sec-s) 1000)
       frac-ms)))

;;; ── A2 Word Parser ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrc--parse-words (text)
  "Parse A2 word-level TEXT into a list of `emms-lyrics-sync-word', or nil.
TEXT is the lyric portion that follows the line timestamp, e.g.:
  \"<01:23.45>Hello <01:23.80>world <01:24.10>today\"
Returns nil when TEXT contains no A2 word timestamps.

Word end-ms values are filled in for all words except the last; the
caller (the document post-processor) fills in the last word's end-ms
using the next line's start-ms."
  (when (string-match-p emms-lyrics-sync-lrc--word-ts-re text)
    (let ((pos 0)
          (len (length text))
          words)
      ;; Collect all <ts>word-text segments in one forward pass.
      ;; We save match-string results before calling string-match again.
      (while (string-match emms-lyrics-sync-lrc--word-ts-re text pos)
        (let* ((ts-end    (match-end 0))
               (m1        (match-string 1 text))
               (m2        (match-string 2 text))
               (m3        (match-string 3 text))
               (ms        (emms-lyrics-sync-lrc--ts-to-ms m1 m2 m3))
               ;; Find start of the NEXT <ts> to know where this word ends.
               ;; This second string-match clobbers match data, which is fine
               ;; because we have already extracted m1/m2/m3 above.
               (next-ts   (string-match emms-lyrics-sync-lrc--word-ts-re text ts-end))
               (word-end  (or next-ts len))
               (word-text (substring text ts-end word-end)))
          (push (emms-lyrics-sync-word--make :start-ms ms
                                         :end-ms   nil
                                         :text     word-text)
                words)
          (setq pos ts-end)))
      ;; Fill end-ms: word[n].end-ms = word[n+1].start-ms
      (let ((result (nreverse words)))
        (cl-loop for cell on result
                 when (cdr cell)
                 do (setf (emms-lyrics-sync-word-end-ms (car cell))
                          (emms-lyrics-sync-word-start-ms (cadr cell))))
        result))))

;;; ── Single-Line Classifier ───────────────────────────────────────────────────

(defun emms-lyrics-sync-lrc--parse-line (line)
  "Classify and parse one LRC LINE string.
Returns one of:
  (:meta KEY VALUE)    — a metadata tag, e.g. from [ti:Song]
  (:lines LIST)        — a list of `emms-lyrics-sync-line' structs (≥1 for multi-ts)
  nil                  — empty, blank, or unrecognised line

A plain text line (no timestamp) returns (:lines (LIST)) with start-ms = nil."
  (let ((pos 0)
        (len (length line))
        timestamps
        meta)
    ;; ── Collect leading [tag] segments ───────────────────────────────────────
    (while (and (< pos len)
                (null meta)
                (eq (aref line pos) ?\[))
      (let ((close (string-search "]" line pos)))
        ;; Stop on malformed tag (no closing bracket).
        (unless close
          (setq pos len)
          (cl-return))
        (let ((content (substring line (1+ pos) close)))
          (cond
           ;; ── Timestamp tag: digits:digits.frac ────────────────────────────
           ((string-match emms-lyrics-sync-lrc--ts-content-re content)
            (push (emms-lyrics-sync-lrc--ts-to-ms
                   (match-string 1 content)
                   (match-string 2 content)
                   (match-string 3 content))
                  timestamps)
            (setq pos (1+ close)))
           ;; ── Metadata tag: alpha-key:value (only at column 0) ─────────────
           ((and (zerop pos)
                 (string-match emms-lyrics-sync-lrc--meta-content-re content))
            (setq meta (list :meta
                             (match-string 1 content)
                             (string-trim (match-string 2 content))))
            (setq pos len))     ; metadata tag consumes the rest of the line
           ;; ── Unknown tag — stop collecting tags ───────────────────────────
           (t (setq pos len))))))
    (cond
     ;; ── Metadata result ──────────────────────────────────────────────────────
     (meta meta)
     ;; ── Synced lyric line(s): one per timestamp ───────────────────────────
     (timestamps
      (let* ((text  (string-trim (substring line pos)))
             (words (emms-lyrics-sync-lrc--parse-words text)))
        (list :lines
              (mapcar (lambda (ts)
                        (emms-lyrics-sync-line--make
                         :start-ms ts
                         :text     text
                         ;; Each multi-ts copy gets its own word list so
                         ;; the display engine can mutate faces independently.
                         :words    (copy-sequence words)))
                      (sort timestamps #'<)))))
     ;; ── Plain text (no timestamp found) ──────────────────────────────────
     (t
      (let ((trimmed (string-trim line)))
        (unless (string-empty-p trimmed)
          (list :lines
                (list (emms-lyrics-sync-line--make :start-ms nil
                                               :text     trimmed
                                               :words    nil)))))))))

;;; ── Document Parser (Main Entry Point) ───────────────────────────────────────

(defun emms-lyrics-sync-lrc-parse (string)
  "Parse LRC content from STRING into an `emms-lyrics-sync-lrc-doc'.

Handles standard LRC, A2 word-level karaoke, multi-timestamp lines, and
metadata tags.  The returned doc's :lines vector is sorted ascending by
start-ms (timed lines first, plain text lines at the end)."
  (let (all-lines
        (title nil) (artist nil) (album nil)
        (offset-ms 0) (has-ts nil))
    ;; ── First pass: classify every line ──────────────────────────────────────
    (dolist (raw (split-string string "\n"))
      (let ((line (string-trim raw)))
        (unless (string-empty-p line)
          (when-let ((result (emms-lyrics-sync-lrc--parse-line line)))
            (pcase result
              (`(:meta ,key ,val)
               (pcase (downcase key)
                 ("ti"     (setq title  val))
                 ("ar"     (setq artist val))
                 ("al"     (setq album  val))
                 ("offset"
                  ;; [offset:+500] delays by 500 ms; negative value advances.
                  (setq offset-ms
                        (or (ignore-errors (string-to-number val)) 0)))))
              (`(:lines ,lyric-lines)
               (dolist (l lyric-lines)
                 (when (emms-lyrics-sync-line-start-ms l) (setq has-ts t))
                 (push l all-lines))))))))
    ;; ── Apply global offset to all timed lines ────────────────────────────────
    (when (and has-ts (/= offset-ms 0))
      (dolist (l all-lines)
        (when (emms-lyrics-sync-line-start-ms l)
          (setf (emms-lyrics-sync-line-start-ms l)
                (max 0 (+ (emms-lyrics-sync-line-start-ms l) offset-ms))))))
    ;; ── Sort: timed lines ascending, plain text lines at the end ──────────────
    (setq all-lines
          (sort all-lines
                (lambda (a b)
                  (let ((ma (emms-lyrics-sync-line-start-ms a))
                        (mb (emms-lyrics-sync-line-start-ms b)))
                    (cond ((and ma mb) (< ma mb))
                          (ma          t)
                          (t           nil))))))
    (let ((vec (vconcat all-lines)))
      ;; ── Post-process A2 last-word end-ms ─────────────────────────────────
      ;; For line[i], its last word's end-ms is set to line[i+1]'s start-ms.
      ;; This cannot be done inside --parse-line because we need the next line.
      (dotimes (i (length vec))
        (let* ((ln        (aref vec i))
               (last-word (car (last (emms-lyrics-sync-line-words ln)))))
          (when (and last-word (null (emms-lyrics-sync-word-end-ms last-word)))
            (let ((next (and (< (1+ i) (length vec)) (aref vec (1+ i)))))
              (when (and next (emms-lyrics-sync-line-start-ms next))
                (setf (emms-lyrics-sync-word-end-ms last-word)
                      (emms-lyrics-sync-line-start-ms next)))))))
      (emms-lyrics-sync-lrc-doc--make
       :lines     vec
       :title     title
       :artist    artist
       :album     album
       :offset-ms offset-ms
       :plain-p   (not has-ts)))))

;;; ── Seek (Binary Search) ─────────────────────────────────────────────────────

(defun emms-lyrics-sync-lrc-seek (doc pos-ms)
  "Return the index of the line active at POS-MS in DOC.
Performs binary search on DOC's lines vector (must be sorted by start-ms).
Returns -1 when POS-MS precedes the first synced line, or when DOC is empty.

The \"active\" line is the last line whose start-ms ≤ POS-MS."
  (let* ((lines (emms-lyrics-sync-lrc-doc-lines doc))
         (n     (length lines)))
    (if (zerop n)
        -1
      (let ((lo 0) (hi (1- n)) (result -1))
        (while (<= lo hi)
          (let* ((mid    (ash (+ lo hi) -1))
                 (mid-ms (emms-lyrics-sync-line-start-ms (aref lines mid))))
            (cond
             ;; Plain text line (no timestamp) — skip backward
             ((null mid-ms)
              (setq hi (1- mid)))
             ;; mid is a candidate: start-ms ≤ pos-ms
             ((<= mid-ms pos-ms)
              (setq result mid
                    lo     (1+ mid)))
             ;; mid is too late
             (t
              (setq hi (1- mid))))))
        result))))

(defun emms-lyrics-sync-lrc-current-line (doc pos-ms)
  "Return the `emms-lyrics-sync-line' active at POS-MS in DOC, or nil."
  (let ((idx (emms-lyrics-sync-lrc-seek doc pos-ms)))
    (when (>= idx 0)
      (aref (emms-lyrics-sync-lrc-doc-lines doc) idx))))

;;; ── Context Window (for Display Engine) ─────────────────────────────────────

(defun emms-lyrics-sync-lrc-context (doc pos-ms n-before n-after)
  "Return a context window of lines centred on the active line at POS-MS.

Returns a list of (LINE . ACTIVEP) cons cells:
  - N-BEFORE preceding lines  (ACTIVEP = nil)
  - the active line itself    (ACTIVEP = t)
  - N-AFTER  following lines  (ACTIVEP = nil)

If POS-MS precedes all lines, the window starts at index 0.
If DOC is empty, returns nil."
  (let* ((lines  (emms-lyrics-sync-lrc-doc-lines doc))
         (n      (length lines)))
    (when (> n 0)
      (let* ((cur   (max 0 (emms-lyrics-sync-lrc-seek doc pos-ms)))
             (start (max 0      (- cur n-before)))
             (end   (min (1- n) (+ cur n-after)))
             result)
        (cl-loop for i from start to end
                 do (push (cons (aref lines i) (= i cur)) result))
        (nreverse result)))))

;;; ── Active Word Within a Line ────────────────────────────────────────────────

(defun emms-lyrics-sync-lrc-active-word (line pos-ms)
  "Return the `emms-lyrics-sync-word' active at POS-MS within LINE, or nil.
Returns nil for lines without A2 word data or when POS-MS < first word."
  (when-let ((words (emms-lyrics-sync-line-words line)))
    (cl-loop for word in words
             when (and (<= (emms-lyrics-sync-word-start-ms word) pos-ms)
                       (or (null (emms-lyrics-sync-word-end-ms word))
                           (< pos-ms (emms-lyrics-sync-word-end-ms word))))
             return word)))

;;; ── Convenience Predicates & Accessors ──────────────────────────────────────

(defun emms-lyrics-sync-lrc-synced-p (doc)
  "Return t if DOC contains at least one timestamp-synced line."
  (not (emms-lyrics-sync-lrc-doc-plain-p doc)))

(defun emms-lyrics-sync-lrc-line-count (doc)
  "Return the total number of lines in DOC (synced + plain)."
  (length (emms-lyrics-sync-lrc-doc-lines doc)))

(defun emms-lyrics-sync-lrc-a2-p (doc)
  "Return t if DOC contains any A2 word-level data."
  (cl-some (lambda (l) (emms-lyrics-sync-line-words l))
           (emms-lyrics-sync-lrc-doc-lines doc)))

(provide 'emms-lyrics-sync-lrc)
;;; emms-lyrics-sync-lrc.el ends here
