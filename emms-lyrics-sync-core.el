;;; emms-lyrics-sync-core.el --- Pipeline orchestration and data model  -*- lexical-binding: t -*-
;; File    : emms-lyrics-sync-core.el
;; Created : 2026-06-11 23:00 UTC
;; Purpose : Defines the track metadata struct and lyrics result struct.
;;           Orchestrates the fetch pipeline: skip-predicates → cache →
;;           sources (sequential async) → parse → hand-off to display.
;;           Owns the on-disk (.lrc sidecar) and in-memory LRC cache.
;;           Implements mpv IPC for accurate playback position with
;;           emms-playing-time as fallback.
;;           Wires EMMS hooks for track start/stop/finish events.
;;
;; Copyright (C) 2026  emms-lyrics-sync contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author  : lxg208
;; URL     : https://github.com/lxg208/emms-lyrics-sync

;;; Commentary:
;;
;; This module is the spine of emms-lyrics.  It does NOT render anything —
;; rendering is entirely delegated to emms-lyrics-sync-display.el.
;;
;; Pipeline (executed on every EMMS track-change):
;;
;;   1. Extract metadata from the EMMS track plist → emms-lyrics-sync-track
;;   2. Run skip predicates; bail out silently if any returns t
;;   3. Check in-memory cache (keyed by file-path)
;;   4. Check on-disk .lrc sidecar
;;   5. Try remote sources in order (each is an async function)
;;   6. On success: write sidecar, update cache, call display
;;   7. On total failure: call display with nil (it shows nothing)
;;
;; mpv IPC strategy:
;;   A persistent Unix domain socket connection is maintained.  Every 100 ms
;;   the display timer calls `emms-lyrics-sync-core--playback-position-ms', which
;;   sends a non-blocking `get_property time-pos' request and returns the last
;;   cached value.  Responses arrive asynchronously via the process filter and
;;   update the cache.  On any error or socket absence the function falls back
;;   to `emms-playing-time'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'emms)
(require 'emms-playing-time)
(require 'emms-lyrics-sync-lrc)

;;; ── User Options ─────────────────────────────────────────────────────────────

(defgroup emms-lyrics-sync nil
  "Synchronized lyrics display for EMMS."
  :group  'emms
  :prefix "emms-lyrics-sync-"
  :link   '(url-link "https://github.com/lxg208/emms-lyrics-sync"))

(defcustom emms-lyrics-sync-sources
  '(emms-lyrics-sync-source-tag
    emms-lyrics-sync-source-local-lrc
    emms-lyrics-sync-source-lrclib
    emms-lyrics-sync-source-netease
    emms-lyrics-sync-source-qqmusic
    emms-lyrics-sync-source-lyricsovh)
  "Ordered list of lyrics source functions to try.
Each function accepts (track callback) where CALLBACK is called with an
`emms-lyrics-sync-result' or nil."
  :type  '(repeat function)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-skip-predicates
  '(emms-lyrics-sync-skip-no-title-p
    emms-lyrics-sync-skip-stream-p)
  "List of predicate functions (TRACK → bool).
If any predicate returns non-nil the track is silently skipped."
  :type  '(repeat function)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-skip-genres
  '("Classical" "Orchestral" "Ambient" "Soundtrack" "Jazz" "Instrumental")
  "Genres for which lyrics are not fetched.
Used by `emms-lyrics-sync-skip-genre-p'."
  :type  '(repeat string)
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-cache-dir
  (expand-file-name "emms-lyrics-sync-cache" user-emacs-directory)
  "Central cache directory for .lrc files when the music dir is read-only.
Entries are stored as <artist> - <title>.lrc."
  :type  'directory
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-prefer-sidecar t
  "When non-nil, write fetched lyrics as a .lrc sidecar next to the track.
Falls back to `emms-lyrics-sync-cache-dir' when the music directory is read-only."
  :type  'boolean
  :group 'emms-lyrics-sync)

;;; ── mpv IPC Options ──────────────────────────────────────────────────────────

(defcustom emms-lyrics-sync-mpv-socket
  (expand-file-name "mpvsocket" temporary-file-directory)
  "Path to the mpv IPC Unix domain socket.
Must match the --input-ipc-server= value passed to mpv.
Example mpv invocation:
  mpv --input-ipc-server=/tmp/mpvsocket <file>"
  :type  'file
  :group 'emms-lyrics-sync)

(defcustom emms-lyrics-sync-mpv-ipc-enabled t
  "When non-nil, use mpv IPC for accurate playback position.
Falls back to `emms-playing-time' when nil or when IPC is unavailable."
  :type  'boolean
  :group 'emms-lyrics-sync)

;;; ── Data Structures ──────────────────────────────────────────────────────────

(cl-defstruct (emms-lyrics-sync-track
               (:constructor emms-lyrics-sync-track--make)
               (:copier nil))
  "Metadata extracted from the current EMMS track plist."
  ;; Identity
  (file-path       nil :type (or null string)
                   :documentation "Full absolute path including filename, or nil for streams.")
  ;; Tags
  (title           nil :type (or null string))
  (artist          nil :type (or null string))
  (composer        nil :type (or null string))
  (album           nil :type (or null string))
  (track-number    nil :type (or null string))
  (genre           nil :type (or null string))
  ;; Technical
  (duration        nil :type (or null number)
                   :documentation "Duration in seconds.")
  (codec           nil :type (or null string)
                   :documentation "e.g. \"FLAC\", \"MP3\".")
  (bits-per-sample nil :type (or null integer)
                   :documentation "e.g. 24.")
  (sample-rate     nil :type (or null integer)
                   :documentation "Hz, e.g. 48000.")
  (bitrate         nil :type (or null integer)
                   :documentation "kbps, e.g. 1596.")
  (channels        nil :type (or null string)
                   :documentation "e.g. \"stereo\"."))

(cl-defstruct (emms-lyrics-sync-result
               (:constructor emms-lyrics-sync-result--make)
               (:copier nil))
  "Lyrics fetch result returned by a source function."
  (source  nil :type symbol
           :documentation "Source identifier: 'tag 'local-lrc 'lrclib 'netease 'qqmusic 'lyricsovh.")
  (format  nil :type symbol
           :documentation "'lrc | 'lrc-a2 | 'plain")
  (content ""  :type string
           :documentation "Raw lyrics string (LRC or plain text).")
  (doc     nil :type (or null emms-lyrics-sync-lrc-doc)
           :documentation "Parsed `emms-lyrics-sync-lrc-doc'; nil until parse step runs."))

;;; ── Internal State ───────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-core--cache (make-hash-table :test #'equal)
  "In-memory cache: file-path → `emms-lyrics-sync-result'.")

(defvar emms-lyrics-sync-core--current-track nil
  "The `emms-lyrics-sync-track' for the currently playing song, or nil.")

(defvar emms-lyrics-sync-core--current-result nil
  "The `emms-lyrics-sync-result' for the currently playing song, or nil.")

(defvar emms-lyrics-sync-core--fetch-token 0
  "Monotonically increasing token to cancel stale async fetches.
Incremented on every track change; source callbacks check their captured
token against this value before updating state.")

;;; ── mpv IPC State ────────────────────────────────────────────────────────────

(defvar emms-lyrics-sync-core--mpv-process nil
  "Persistent network process for the mpv IPC socket connection.")

(defvar emms-lyrics-sync-core--mpv-position 0.0
  "Most recently received playback position in seconds from mpv IPC.
Updated asynchronously by the process filter; read by the display timer.")

(defvar emms-lyrics-sync-core--mpv-buffer ""
  "Accumulation buffer for partial mpv IPC JSON responses.")

(defvar emms-lyrics-sync-core--mpv-request-id 0
  "Monotonically increasing request ID for mpv IPC JSON commands.")

;;; ── mpv IPC Implementation ───────────────────────────────────────────────────

(defun emms-lyrics-sync-core--mpv-filter (_proc string)
  "Process filter for mpv IPC.
Accumulates STRING and parses complete newline-delimited JSON objects.
Updates `emms-lyrics-sync-core--mpv-position' when a time-pos response arrives."
  (setq emms-lyrics-sync-core--mpv-buffer
        (concat emms-lyrics-sync-core--mpv-buffer string))
  ;; Process all complete lines
  (while (string-match "\n" emms-lyrics-sync-core--mpv-buffer)
    (let ((line (substring emms-lyrics-sync-core--mpv-buffer
                           0 (match-beginning 0))))
      (setq emms-lyrics-sync-core--mpv-buffer
            (substring emms-lyrics-sync-core--mpv-buffer (match-end 0)))
      ;; Parse and extract time-pos
      (condition-case nil
          (let* ((obj  (json-parse-string line
                                          :object-type  'alist
                                          :null-object  nil
                                          :false-object nil))
                 (err  (cdr (assoc "error" obj)))
                 (data (cdr (assoc "data"  obj))))
            (when (and (equal err "success") (numberp data))
              (setq emms-lyrics-sync-core--mpv-position (float data))))
        ;; Ignore malformed lines (mpv also emits event objects)
        (json-parse-error nil)
        (error nil)))))

(defun emms-lyrics-sync-core--mpv-sentinel (_proc event)
  "Sentinel for the mpv IPC process.
Cleans up state on disconnect so the next tick reconnects automatically."
  (when (string-match-p "\\(?:deleted\\|closed\\|failed\\|exited\\|finished\\)"
                        event)
    (setq emms-lyrics-sync-core--mpv-process nil
          emms-lyrics-sync-core--mpv-buffer  "")))

(defun emms-lyrics-sync-core--mpv-connect ()
  "Attempt to connect to the mpv IPC socket.
Returns t on success, nil if the socket is absent or connection fails.
Safe to call repeatedly — returns immediately if already connected."
  (when (and emms-lyrics-sync-mpv-ipc-enabled
             (not (process-live-p emms-lyrics-sync-core--mpv-process))
             (file-exists-p emms-lyrics-sync-mpv-socket))
    (condition-case err
        (progn
          (setq emms-lyrics-sync-core--mpv-process
                (make-network-process
                 :name     "emms-lyrics-sync-mpv-ipc"
                 :family   'local
                 :service  emms-lyrics-sync-mpv-socket
                 :filter   #'emms-lyrics-sync-core--mpv-filter
                 :sentinel #'emms-lyrics-sync-core--mpv-sentinel
                 :noquery  t
                 :nowait   nil))
          ;; Reset position cache on fresh connection
          (setq emms-lyrics-sync-core--mpv-position 0.0
                emms-lyrics-sync-core--mpv-buffer   "")
          t)
      (error
       (message "emms-lyrics: mpv IPC connect failed: %S" err)
       nil))))

(defun emms-lyrics-sync-core--mpv-request-position ()
  "Send an async time-pos request to mpv.
The response is handled by `emms-lyrics-sync-core--mpv-filter'.
Safe to call when not connected — silently does nothing."
  (when (process-live-p emms-lyrics-sync-core--mpv-process)
    (condition-case nil
        (progn
          (cl-incf emms-lyrics-sync-core--mpv-request-id)
          (process-send-string
           emms-lyrics-sync-core--mpv-process
           (format "{\"command\":[\"get_property\",\"time-pos\"],\"request_id\":%d}\n"
                   emms-lyrics-sync-core--mpv-request-id)))
      (error
       ;; Socket died — clean up so the next call reconnects
       (setq emms-lyrics-sync-core--mpv-process nil)))))

(defun emms-lyrics-sync-core--mpv-disconnect ()
  "Close the mpv IPC connection cleanly."
  (when (process-live-p emms-lyrics-sync-core--mpv-process)
    (ignore-errors (delete-process emms-lyrics-sync-core--mpv-process)))
  (setq emms-lyrics-sync-core--mpv-process  nil
        emms-lyrics-sync-core--mpv-buffer   ""
        emms-lyrics-sync-core--mpv-position 0.0))

;;; ── Public: Playback Position ────────────────────────────────────────────────

(defun emms-lyrics-sync-core--playback-position-ms ()
  "Return the current playback position in milliseconds.

Strategy:
  1. Ensure mpv IPC is connected (reconnects automatically if dropped).
  2. Send a non-blocking time-pos request (result arrives via filter).
  3. Return the most recently cached position from the last response.
  4. Fall back to `emms-playing-time' if IPC is disabled or unavailable.

Because the request and response are asynchronous, the returned value is
the position from the previous 100 ms tick — an imperceptible lag."
  (if emms-lyrics-sync-mpv-ipc-enabled
      (progn
        ;; Reconnect if needed (no-op when already connected)
        (unless (process-live-p emms-lyrics-sync-core--mpv-process)
          (emms-lyrics-sync-core--mpv-connect))
        ;; Fire off request for next tick
        (emms-lyrics-sync-core--mpv-request-position)
        ;; Return last known position
        (round (* emms-lyrics-sync-core--mpv-position 1000.0)))
    ;; Fallback: emms-playing-time (seconds, updated ~1/s)
    (round (* (or (bound-and-true-p emms-playing-time) 0.0) 1000.0))))

;;; ── Track Metadata Extraction ────────────────────────────────────────────────

(defun emms-lyrics-sync-core--extract-track (emms-track)
  "Build an `emms-lyrics-sync-track' from EMMS-TRACK (an EMMS track plist)."
  (let ((get (lambda (key) (emms-track-get emms-track key nil))))
    (emms-lyrics-sync-track--make
     :file-path       (when (eq (emms-track-type emms-track) 'file)
                        (emms-track-name emms-track))
     :title           (funcall get 'info-title)
     :artist          (funcall get 'info-artist)
     :composer        (funcall get 'info-composer)
     :album           (funcall get 'info-album)
     :track-number    (funcall get 'info-tracknumber)
     :genre           (funcall get 'info-genre)
     :duration        (funcall get 'info-playing-time)
     :codec           (funcall get 'info-codec)
     :bits-per-sample (funcall get 'info-bits-per-sample)
     :sample-rate     (funcall get 'info-samplerate)
     :bitrate         (funcall get 'info-bitrate)
     :channels        (funcall get 'info-channels))))

;;; ── Skip Predicates ──────────────────────────────────────────────────────────

(defun emms-lyrics-sync-skip-no-title-p (track)
  "Return t if TRACK has no title tag."
  (null (emms-lyrics-sync-track-title track)))

(defun emms-lyrics-sync-skip-stream-p (track)
  "Return t if TRACK is an internet stream (no file path)."
  (null (emms-lyrics-sync-track-file-path track)))

(defun emms-lyrics-sync-skip-genre-p (track)
  "Return t if TRACK's genre is in `emms-lyrics-sync-skip-genres'."
  (when-let ((genre (emms-lyrics-sync-track-genre track)))
    (cl-some (lambda (g) (string-equal-ignore-case genre g))
             emms-lyrics-sync-skip-genres)))

(defun emms-lyrics-sync-core--should-skip-p (track)
  "Return t if any predicate in `emms-lyrics-sync-skip-predicates' matches TRACK."
  (cl-some (lambda (pred) (funcall pred track))
           emms-lyrics-sync-skip-predicates))

;;; ── Cache Helpers ────────────────────────────────────────────────────────────

(defun emms-lyrics-sync-core--sidecar-path (file-path)
  "Return the .lrc sidecar path for FILE-PATH (same dir, same stem).
Returns nil for nil FILE-PATH (streams)."
  (when file-path
    (concat (file-name-sans-extension (expand-file-name file-path)) ".lrc")))

(defun emms-lyrics-sync-core--cache-lrc-path (track)
  "Return the central-cache .lrc path for TRACK, or nil.
Format: <emms-lyrics-sync-cache-dir>/<artist> - <title>.lrc"
  (when-let* ((artist (emms-lyrics-sync-track-artist track))
              (title  (emms-lyrics-sync-track-title  track)))
    (expand-file-name
     (concat (emms-lyrics-sync-core--safe-filename artist)
             " - "
             (emms-lyrics-sync-core--safe-filename title)
             ".lrc")
     emms-lyrics-sync-cache-dir)))

(defun emms-lyrics-sync-core--safe-filename (str)
  "Replace filesystem-unsafe characters in STR for use as a filename."
  (replace-regexp-in-string "[/\\\\:*?\"<>|\0]" "_" (string-trim str)))

(defun emms-lyrics-sync-core--check-sidecar (track)
  "Return a cached `emms-lyrics-sync-result' for TRACK from disk, or nil.
Checks the adjacent sidecar first, then the central cache directory."
  (let* ((fp       (emms-lyrics-sync-track-file-path track))
         (sidecar  (emms-lyrics-sync-core--sidecar-path fp))
         (central  (emms-lyrics-sync-core--cache-lrc-path track))
         (found    (cl-find-if #'file-readable-p
                               (delq nil (list sidecar central)))))
    (when found
      (let ((content (with-temp-buffer
                       (insert-file-contents found)
                       (buffer-string))))
        (emms-lyrics-sync-result--make
         :source  'local-lrc
         :format  'lrc
         :content content)))))

(defun emms-lyrics-sync-core--write-sidecar (track content)
  "Write LRC CONTENT to disk for TRACK.
Prefers the adjacent sidecar location when writable; falls back to
`emms-lyrics-sync-cache-dir'.  Silently does nothing on any write failure."
  (let* ((fp      (emms-lyrics-sync-track-file-path track))
         (sidecar (emms-lyrics-sync-core--sidecar-path fp))
         (central (emms-lyrics-sync-core--cache-lrc-path track))
         (target  (cond
                   ;; Adjacent sidecar preferred when dir is writable
                   ((and emms-lyrics-sync-prefer-sidecar
                         sidecar
                         (file-writable-p (file-name-directory sidecar)))
                    sidecar)
                   ;; Central cache fallback
                   (central
                    (make-directory emms-lyrics-sync-cache-dir t)
                    central)
                   (t nil))))
    (when target
      (condition-case err
          (write-region content nil target nil 'silent)
        (error
         (message "emms-lyrics: could not write sidecar %S: %S" target err))))))

;;; ── Result Parsing ───────────────────────────────────────────────────────────

(defun emms-lyrics-sync-core--parse-result (result)
  "Parse RESULT's raw content into an `emms-lyrics-sync-lrc-doc' and store it.
Returns RESULT with the `doc' slot populated.
For plain-text lyrics, wraps each line as an unsynced `emms-lyrics-sync-line'."
  (when result
    (let ((doc (emms-lyrics-sync-lrc-parse (emms-lyrics-sync-result-content result))))
      (setf (emms-lyrics-sync-result-doc result) doc)
      result)))

;;; ── Source Pipeline ──────────────────────────────────────────────────────────

(defun emms-lyrics-sync-core--try-sources (track token sources callback)
  "Try each function in SOURCES for TRACK; call CALLBACK with the first hit.
SOURCES is a list of functions each accepting (track cb).
TOKEN is the fetch token at the time this fetch was launched; if it no longer
matches `emms-lyrics-sync-core--fetch-token' the result is discarded (stale).
CALLBACK is called with an `emms-lyrics-sync-result' or nil."
  (if (null sources)
      ;; All sources exhausted
      (when (= token emms-lyrics-sync-core--fetch-token)
        (funcall callback nil))
    (let ((source (car sources))
          (rest   (cdr sources)))
      (condition-case err
          (funcall source track
                   (lambda (result)
                     ;; Discard stale results from superseded track fetches
                     (if (not (= token emms-lyrics-sync-core--fetch-token))
                         nil
                       (if result
                           (funcall callback result)
                         ;; Try next source
                         (emms-lyrics-sync-core--try-sources
                          track token rest callback)))))
        (error
         (message "emms-lyrics: source %S error: %S" source err)
         (emms-lyrics-sync-core--try-sources track token rest callback))))))

;;; ── Public Entry Points ──────────────────────────────────────────────────────

(defun emms-lyrics-sync-core--on-track-change (track)
  "Handle a track change event for TRACK (an `emms-lyrics-sync-track').
Increments the fetch token, checks caches, then runs the source pipeline.
Calls `emms-lyrics-sync-display-on-track-change' when a result is ready."
  ;; Increment token to invalidate any in-flight fetches for the old track
  (cl-incf emms-lyrics-sync-core--fetch-token)
  (let ((token emms-lyrics-sync-core--fetch-token))
    (setq emms-lyrics-sync-core--current-track  track
          emms-lyrics-sync-core--current-result nil)

    ;; Notify display immediately so it can show track info even without lyrics
    (when (fboundp 'emms-lyrics-sync-display-on-track-change)
      (emms-lyrics-sync-display-on-track-change track nil))

    ;; Skip check
    (when (emms-lyrics-sync-core--should-skip-p track)
      (cl-return-from emms-lyrics-sync-core--on-track-change))

    ;; In-memory cache hit
    (let* ((fp     (emms-lyrics-sync-track-file-path track))
           (cached (and fp (gethash fp emms-lyrics-sync-core--cache))))
      (when cached
        (setq emms-lyrics-sync-core--current-result cached)
        (when (fboundp 'emms-lyrics-sync-display-on-track-change)
          (emms-lyrics-sync-display-on-track-change track cached))
        (cl-return-from emms-lyrics-sync-core--on-track-change)))

    ;; On-disk sidecar hit
    (let ((sidecar-result (emms-lyrics-sync-core--check-sidecar track)))
      (when sidecar-result
        (let ((parsed (emms-lyrics-sync-core--parse-result sidecar-result)))
          (setq emms-lyrics-sync-core--current-result parsed)
          (when-let ((fp (emms-lyrics-sync-track-file-path track)))
            (puthash fp parsed emms-lyrics-sync-core--cache))
          (when (fboundp 'emms-lyrics-sync-display-on-track-change)
            (emms-lyrics-sync-display-on-track-change track parsed))
          (cl-return-from emms-lyrics-sync-core--on-track-change))))

    ;; Run source pipeline asynchronously
    (emms-lyrics-sync-core--try-sources
     track token emms-lyrics-sync-sources
     (lambda (result)
       (when (= token emms-lyrics-sync-core--fetch-token)
         (let ((parsed (emms-lyrics-sync-core--parse-result result)))
           (setq emms-lyrics-sync-core--current-result parsed)
           ;; Cache and persist
           (when parsed
             (when-let ((fp (emms-lyrics-sync-track-file-path track)))
               (puthash fp parsed emms-lyrics-sync-core--cache))
             (when (eq (emms-lyrics-sync-result-format parsed) 'lrc)
               (emms-lyrics-sync-core--write-sidecar
                track (emms-lyrics-sync-result-content parsed))))
           ;; Update display with real lyrics
           (when (fboundp 'emms-lyrics-sync-display-on-track-change)
             (emms-lyrics-sync-display-on-track-change track parsed))))))))

(defun emms-lyrics-sync-core--on-stop ()
  "Handle playback stop/pause.
Disconnects mpv IPC and notifies the display."
  (setq emms-lyrics-sync-core--current-track  nil
        emms-lyrics-sync-core--current-result nil)
  (cl-incf emms-lyrics-sync-core--fetch-token)
  ;; Don't disconnect IPC on stop — mpv may still be running (paused)
  ;; The display is notified via emms-lyrics-sync--on-track-stop in emms-lyrics.el
  )

(defun emms-lyrics-sync-core--disconnect-mpv ()
  "Disconnect mpv IPC.  Called when `emms-lyrics-sync-mode' is disabled."
  (emms-lyrics-sync-core--mpv-disconnect))

(provide 'emms-lyrics-sync-core)
;;; emms-lyrics-sync-core.el ends here
