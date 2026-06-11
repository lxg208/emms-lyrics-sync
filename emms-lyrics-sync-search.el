;;; emms-lyrics-sync-search.el --- Manual lyrics search UI  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-search.el
;; Created : 2026-06-11 22:50 UTC
;; Purpose : Provides the manual lyrics search interface, modelled on
;;           OpenLyrics in foobar2000.
;;
;;           `emms-lyrics-sync-search' opens a dedicated search buffer pre-filled
;;           with all track metadata fields (artist, title, album, duration).
;;           The user edits the fields, selects sources, and confirms.
;;           All selected sources are queried in parallel; results are
;;           collected and presented in a candidate buffer where the user
;;           picks one.  The selected result is immediately applied to the
;;           display and written to the sidecar cache.
;;
;;           `emms-lyrics-sync-next-source' cycles through the last result set
;;           without reopening the search UI — mirroring the OpenLyrics
;;           "next" keybinding.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; Search buffer keybindings (emms-lyrics-sync-search-mode):
;;   C-c C-c  — execute search
;;   C-c C-k  — cancel
;;   TAB      — move to next field
;;   S-TAB    — move to previous field
;;
;; Results buffer keybindings (emms-lyrics-sync-results-mode):
;;   RET      — accept highlighted result
;;   n / p    — move between results
;;   C-c C-k  — cancel
;;   v        — preview result in a temporary buffer

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emms-lyrics-sync-core)
(require 'emms-lyrics-sync-lrc)

;;; ── Customization ────────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-search-buffer-name "*emms-lyrics-sync-search*"
  "Name of the manual search input buffer."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-search-results-buffer-name "*emms-lyrics-sync-results*"
  "Name of the search results buffer."
  :type  'string
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-search-timeout 15
  "Seconds to wait for all parallel source responses before showing results."
  :type  'integer
  :group 'emms-lyrics-sync)

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-search--last-results nil
  "List of `emms-lyrics-sync-result' objects from the last manual search.
Enables `emms-lyrics-sync-next-source' to cycle without re-querying.")

(defvar emms-lyrics-sync-search--last-result-idx 0
  "Index into `emms-lyrics-sync-search--last-results' currently applied.")

(defvar emms-lyrics-sync-search--active-track nil
  "The `emms-lyrics-sync-track' the last manual search was performed for.")

;;; ── Field Infrastructure ─────────────────────────────────────────────────────

;; Each field is represented by a pair of markers: one at the beginning of the
;; value span and one at the end.  Text properties mark the value regions as
;; 'emms-lyrics-sync-search-field so TAB navigation can find them.

(defvar-local emms-lyrics-sync-search--fields nil
  "Alist of (field-name . (value-start-marker . value-end-marker)) in search buf.")

(defun emms-lyrics-sync-search--insert-field (name value)
  "Insert a labelled, editable field NAME with initial VALUE at point.
Records the value's buffer span in `emms-lyrics-sync-search--fields'."
  (let* ((label     (format "%-12s" (concat name ":")))
         (val-start nil)
         (val-end   nil))
    (insert (propertize label 'face 'font-lock-keyword-face
                               'read-only t
                               'field     'label))
    (setq val-start (point))
    (insert (propertize (or value "")
                        'face               'font-lock-string-face
                        'emms-lyrics-sync-field  name
                        'rear-nonsticky     '(emms-lyrics-sync-field)))
    (setq val-end (point))
    (insert "\n")
    (push (cons name (cons (copy-marker val-start t)
                           (copy-marker val-end)))
          emms-lyrics-sync-search--fields)))

(defun emms-lyrics-sync-search--field-value (name)
  "Return the current string value of field NAME from the search buffer."
  (let ((entry (assoc name emms-lyrics-sync-search--fields)))
    (when entry
      (let* ((m1 (cadr entry))
             (m2 (cddr entry)))
        (when (and (markerp m1) (markerp m2)
                   (marker-buffer m1))
          (string-trim
           (buffer-substring-no-properties
            (marker-position m1)
            (marker-position m2))))))))

;;; ── Source Checkbox Infrastructure ──────────────────────────────────────────

(defvar-local emms-lyrics-sync-search--source-states nil
  "Alist of (source-fn-symbol . enabled-bool) for the search buffer.")

(defun emms-lyrics-sync-search--insert-source-toggle (sym label)
  "Insert a toggleable checkbox line for source SYM with LABEL."
  (let ((enabled t))
    (push (cons sym enabled) emms-lyrics-sync-search--source-states)
    (insert (propertize "  [x] " 'face 'font-lock-builtin-face
                                 'emms-lyrics-sync-checkbox sym)
            label "\n")))

(defun emms-lyrics-sync-search--toggle-source-at-point ()
  "Toggle the source checkbox on the current line."
  (interactive)
  (let* ((sym (get-text-property (line-beginning-position) 'emms-lyrics-sync-checkbox))
         (entry (assq sym emms-lyrics-sync-search--source-states)))
    (when entry
      (setcdr entry (not (cdr entry)))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (line-beginning-position))
          (when (re-search-forward "\\[.\\]" (line-end-position) t)
            (replace-match (if (cdr entry) "[x]" "[ ]"))))))))

(defun emms-lyrics-sync-search--enabled-sources ()
  "Return list of source function symbols currently checked in the search buffer."
  (delq nil
        (mapcar (lambda (kv) (when (cdr kv) (car kv)))
                emms-lyrics-sync-search--source-states)))

;;; ── Search Buffer ────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-search-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'emms-lyrics-sync-search-execute)
    (define-key m (kbd "C-c C-k") #'emms-lyrics-sync-search-cancel)
    (define-key m (kbd "TAB")     #'emms-lyrics-sync-search-next-field)
    (define-key m (kbd "<backtab>") #'emms-lyrics-sync-search-prev-field)
    (define-key m (kbd "SPC")     #'emms-lyrics-sync-search--toggle-source-at-point)
    m)
  "Keymap for `emms-lyrics-sync-search-mode'.")

(define-derived-mode emms-lyrics-sync-search-mode special-mode "Lyrics-Search"
  "Major mode for the emms-lyrics-sync manual search input buffer."
  (setq-local revert-buffer-function #'ignore))

;; Allow editing in field regions despite special-mode's read-only inhibit
(put 'emms-lyrics-sync-search-mode 'mode-class 'special)

;;;###autoload
(defun emms-lyrics-sync-search (&optional track)
  "Open the manual lyrics search UI for TRACK (default: current EMMS track).
Pre-fills all metadata fields; user edits and confirms with \\[emms-lyrics-sync-search-execute]."
  (interactive)
  (let* ((track   (or track emms-lyrics-sync-core--current-track))
         (buf     (get-buffer-create emms-lyrics-sync-search-buffer-name)))
    (unless track
      (user-error "No track is currently playing"))
    (setq emms-lyrics-sync-search--active-track track)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-search-mode)
        (setq emms-lyrics-sync-search--fields        nil
              emms-lyrics-sync-search--source-states nil)
        ;; ── Header ────────────────────────────────────────────────────────
        (insert (propertize "  Lyrics Search\n"
                            'face '(:height 1.2 :weight bold))
                (propertize (make-string 40 ?─) 'face 'shadow)
                "\n\n")
        ;; ── Metadata fields ───────────────────────────────────────────────
        (insert (propertize "  Search Fields\n" 'face 'font-lock-comment-face))
        (emms-lyrics-sync-search--insert-field "Artist"   (emms-lyrics-sync-track-artist   track))
        (emms-lyrics-sync-search--insert-field "Title"    (emms-lyrics-sync-track-title    track))
        (emms-lyrics-sync-search--insert-field "Album"    (emms-lyrics-sync-track-album    track))
        (emms-lyrics-sync-search--insert-field "Duration"
          (when (emms-lyrics-sync-track-duration track)
            (number-to-string (round (emms-lyrics-sync-track-duration track)))))
        (insert "\n")
        ;; ── Source selection ─────────────────────────────────────────────
        (insert (propertize "  Sources  (SPC to toggle)\n"
                            'face 'font-lock-comment-face))
        (dolist (src emms-lyrics-sync-sources)
          (emms-lyrics-sync-search--insert-source-toggle
           src (emms-lyrics-sync-search--source-label src)))
        (insert "\n")
        ;; ── Help line ────────────────────────────────────────────────────
        (insert (propertize
                 "  C-c C-c  search    C-c C-k  cancel    TAB  next field\n"
                 'face 'shadow))
        ;; Position cursor on Artist value
        (goto-char (point-min))
        (emms-lyrics-sync-search-next-field)))
    (pop-to-buffer buf
                   '(display-buffer-below-selected
                     (window-height . 20)))))

(defun emms-lyrics-sync-search--source-label (sym)
  "Return a human-readable label for source function SYM."
  (pcase sym
    ('emms-lyrics-sync-source-tag         "Embedded tag (ID3 USLT / Vorbis LYRICS=)")
    ('emms-lyrics-sync-source-local-lrc   "Local .lrc sidecar")
    ('emms-lyrics-sync-source-lrclib      "LRCLIB (free, synced, no key)")
    ('emms-lyrics-sync-source-netease     "NetEase Cloud Music")
    ('emms-lyrics-sync-source-qqmusic     "QQ Music")
    ('emms-lyrics-sync-source-lyricsovh   "lyrics.ovh (plain text fallback)")
    (_ (symbol-name sym))))

;;; ── Field Navigation ─────────────────────────────────────────────────────────

(defun emms-lyrics-sync-search-next-field ()
  "Move point to the beginning of the next editable field value."
  (interactive)
  (let ((pos (next-single-property-change (point) 'emms-lyrics-sync-field)))
    (when pos (goto-char pos))))

(defun emms-lyrics-sync-search-prev-field ()
  "Move point to the beginning of the previous editable field value."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'emms-lyrics-sync-field)))
    (when pos
      ;; Jump to start of the field value, not its end
      (goto-char
       (or (previous-single-property-change pos 'emms-lyrics-sync-field)
           pos)))))

;;; ── Execute Search ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-search-cancel ()
  "Close the search buffer without searching."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emms-lyrics-sync-search-execute ()
  "Run the manual search using the current field values and selected sources."
  (interactive)
  (unless (eq major-mode 'emms-lyrics-sync-search-mode)
    (user-error "Not in emms-lyrics-sync-search buffer"))
  (let* ((artist   (emms-lyrics-sync-search--field-value "Artist"))
         (title    (emms-lyrics-sync-search--field-value "Title"))
         (album    (emms-lyrics-sync-search--field-value "Album"))
         (dur-str  (emms-lyrics-sync-search--field-value "Duration"))
         (duration (when (and dur-str (not (string-empty-p dur-str)))
                     (string-to-number dur-str)))
         ;; Build a synthetic track from the edited fields
         (search-track (emms-lyrics-sync-track--make
                        :title    (unless (string-empty-p title)    title)
                        :artist   (unless (string-empty-p artist)   artist)
                        :album    (unless (string-empty-p album)    album)
                        :duration duration
                        :file-path
                        (emms-lyrics-sync-track-file-path
                         emms-lyrics-sync-search--active-track)))
         (sources  (emms-lyrics-sync-search--enabled-sources))
         (search-buf (current-buffer)))
    (unless sources
      (user-error "No sources selected"))
    (kill-buffer search-buf)
    ;; Query all selected sources in parallel
    (message "emms-lyrics: searching %d source(s)…" (length sources))
    (emms-lyrics-sync-search--query-parallel
     search-track sources
     (lambda (results)
       (if results
           (emms-lyrics-sync-search--show-results results search-track)
         (message "emms-lyrics: no lyrics found in any source"))))))

(defun emms-lyrics-sync-search--query-parallel (track sources callback)
  "Query all SOURCES for TRACK in parallel; call CALLBACK with result list.
CALLBACK receives a list of `emms-lyrics-sync-result' objects (never nil items),
sorted by source priority.  Waits up to `emms-lyrics-sync-search-timeout' seconds."
  (let* ((n-sources  (length sources))
         (results    (make-vector n-sources nil))
         (done-count 0)
         (deadline   (run-at-time emms-lyrics-sync-search-timeout nil
                                  (lambda ()
                                    (funcall callback
                                             (delq nil (append results nil)))))))
    (cl-loop for src in sources
             for idx from 0 do
      (condition-case err
          (funcall src track
                   (let ((i idx))
                     (lambda (result)
                       (aset results i result)
                       (cl-incf done-count)
                       (when (= done-count n-sources)
                         (cancel-timer deadline)
                         (funcall callback
                                  (delq nil (append results nil)))))))
        (error
         (message "emms-lyrics-sync-search: source %S error: %S" src err)
         (cl-incf done-count)
         (when (= done-count n-sources)
           (cancel-timer deadline)
           (funcall callback (delq nil (append results nil)))))))))

;;; ── Results Buffer ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-results-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")     #'emms-lyrics-sync-results-accept)
    (define-key m (kbd "n")       #'emms-lyrics-sync-results-next)
    (define-key m (kbd "p")       #'emms-lyrics-sync-results-prev)
    (define-key m (kbd "v")       #'emms-lyrics-sync-results-preview)
    (define-key m (kbd "C-c C-k") #'emms-lyrics-sync-results-cancel)
    (define-key m (kbd "q")       #'emms-lyrics-sync-results-cancel)
    m)
  "Keymap for `emms-lyrics-sync-results-mode'.")

(define-derived-mode emms-lyrics-sync-results-mode special-mode "Lyrics-Results"
  "Major mode for the emms-lyrics-sync search results buffer."
  (setq-local truncate-lines t))

(defvar-local emms-lyrics-sync-results--items nil
  "Vector of `emms-lyrics-sync-result' objects shown in the results buffer.")

(defun emms-lyrics-sync-search--show-results (results track)
  "Display RESULTS in the results buffer for TRACK."
  (let ((buf (get-buffer-create emms-lyrics-sync-search-results-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emms-lyrics-sync-results-mode)
        (setq emms-lyrics-sync-results--items (vconcat results))
        ;; ── Header ──────────────────────────────────────────────────────
        (insert (propertize "  Lyrics Results\n"
                            'face '(:height 1.2 :weight bold))
                (propertize (format "  %s — %s\n"
                                    (or (emms-lyrics-sync-track-artist track) "?")
                                    (or (emms-lyrics-sync-track-title  track) "?"))
                            'face 'font-lock-comment-face)
                (propertize (make-string 60 ?─) 'face 'shadow)
                "\n\n")
        ;; ── Result rows ─────────────────────────────────────────────────
        (cl-loop for result in results
                 for idx from 0 do
          (let* ((source (emms-lyrics-sync-result-source result))
                 (fmt    (emms-lyrics-sync-result-format result))
                 (label  (emms-lyrics-sync-search--source-label source))
                 (fmt-s  (pcase fmt
                           ('lrc    (propertize "[synced LRC]"
                                                'face 'font-lock-string-face))
                           ('lrc-a2 (propertize "[synced A2 LRC]"
                                                'face 'font-lock-builtin-face))
                           (_       (propertize "[plain text]"
                                                'face 'font-lock-comment-face)))))
            (insert
             (propertize
              (format "  %d.  %-40s  %s\n" (1+ idx) label fmt-s)
              'face         (if (= idx 0) 'highlight 'default)
              'emms-result  idx))))
        (insert (propertize
                 "\n  RET accept   n/p navigate   v preview   q cancel\n"
                 'face 'shadow))
        (goto-char (point-min))
        (emms-lyrics-sync-results-next)))   ; land on first result
    (pop-to-buffer buf
                   '(display-buffer-below-selected
                     (window-height . 15)))))

(defun emms-lyrics-sync-results--current-idx ()
  "Return the result index at point, or nil."
  (get-text-property (point) 'emms-result))

(defun emms-lyrics-sync-results-next ()
  "Move to the next result row."
  (interactive)
  (let ((pos (next-single-property-change (point) 'emms-result)))
    (when pos
      (goto-char pos)
      (emms-lyrics-sync-results--highlight-current))))

(defun emms-lyrics-sync-results-prev ()
  "Move to the previous result row."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'emms-result)))
    (when pos
      (goto-char pos)
      (emms-lyrics-sync-results--highlight-current))))

(defun emms-lyrics-sync-results--highlight-current ()
  "Highlight the result row at point."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((idx    (get-text-property (point) 'emms-result))
               (end    (or (next-single-property-change (point) 'emms-result)
                           (point-max)))
               (cur-idx (emms-lyrics-sync-results--current-idx)))
          (when idx
            (put-text-property point end 'face
                               (if (eq idx cur-idx) 'highlight 'default)))
          (goto-char (or end (point-max))))))))

(defun emms-lyrics-sync-results-preview ()
  "Show the raw lyrics content of the result at point in a temp buffer."
  (interactive)
  (let ((idx (emms-lyrics-sync-results--current-idx)))
    (when idx
      (let* ((result  (aref emms-lyrics-sync-results--items idx))
             (content (emms-lyrics-sync-result-content result))
             (buf     (get-buffer-create "*emms-lyrics-sync-preview*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min))))
        (display-buffer buf '(display-buffer-below-selected
                               (window-height . 20)))))))

(defun emms-lyrics-sync-results-cancel ()
  "Close the results buffer without applying any result."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emms-lyrics-sync-results-accept ()
  "Apply the currently highlighted result and close the results buffer."
  (interactive)
  (let ((idx (emms-lyrics-sync-results--current-idx)))
    (unless idx
      (user-error "No result selected"))
    (let* ((result  (aref emms-lyrics-sync-results--items idx))
           (track   emms-lyrics-sync-search--active-track))
      ;; Parse LRC if not yet done
      (emms-lyrics-sync-core--parse-result result)
      ;; Save to sidecar / cache
      (when track
        (emms-lyrics-sync-core--write-cache track result))
      ;; Update live state and refresh display
      (setq emms-lyrics-sync-core--current-result  result
            emms-lyrics-sync-search--last-results  (append emms-lyrics-sync-results--items nil)
            emms-lyrics-sync-search--last-result-idx idx)
      (kill-buffer (current-buffer))
      (emms-lyrics-sync-display-on-track-change)
      (message "emms-lyrics: applied result from %S"
               (emms-lyrics-sync-result-source result)))))

;;; ── Next Source (OpenLyrics-style cycle) ─────────────────────────────────────

;;;###autoload
(defun emms-lyrics-sync-next-source ()
  "Cycle to the next result from the last manual search without re-querying.
If no previous results exist, falls back to `emms-lyrics-sync-search'."
  (interactive)
  (if (null emms-lyrics-sync-search--last-results)
      (call-interactively #'emms-lyrics-sync-search)
    (let* ((n       (length emms-lyrics-sync-search--last-results))
           (new-idx (% (1+ emms-lyrics-sync-search--last-result-idx) n))
           (result  (nth new-idx emms-lyrics-sync-search--last-results))
           (track   emms-lyrics-sync-search--active-track))
      (setq emms-lyrics-sync-search--last-result-idx new-idx)
      (emms-lyrics-sync-core--parse-result result)
      (when track
        (emms-lyrics-sync-core--write-cache track result))
      (setq emms-lyrics-sync-core--current-result result)
      (emms-lyrics-sync-display-on-track-change)
      (message "emms-lyrics: source %d/%d — %S"
               (1+ new-idx) n (emms-lyrics-sync-result-source result)))))

(provide 'emms-lyrics-sync-search)
;;; emms-lyrics-sync-search.el ends here
