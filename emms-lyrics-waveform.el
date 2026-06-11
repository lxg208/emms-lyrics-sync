;;; emms-lyrics-waveform.el --- Waveform analysis and rendering  -*- lexical-binding: t -*-

;; Copyright (C) 2025  emms-lyrics contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Generates and renders a waveform loudness bar at the bottom of the
;; lyrics display, with a playback progress cursor.
;;
;; Also serves as a dynamic range / loudness-war diagnostic:
;; heavily compressed / limited tracks show a visually flat, solid bar.
;;
;; Analysis is performed asynchronously in a subprocess (ffmpeg) so it
;; never blocks Emacs.
;;
;; Render modes:
;;   'unicode — sparkline using ▁▂▃▄▅▆▇█ (works in terminal)
;;   'svg     — SVG image (GUI Emacs only, higher resolution)

;;; Code:

(require 'emms-lyrics-core)

(defcustom emms-lyrics-waveform-render-mode 'unicode
  "Waveform render mode.  \\='unicode works in terminals; \\='svg requires GUI Emacs."
  :type '(choice (const unicode) (const svg))
  :group 'emms-lyrics)

(defcustom emms-lyrics-waveform-width 80
  "Number of columns (unicode) or pixels (svg) for the waveform bar."
  :type 'integer
  :group 'emms-lyrics)

(defcustom emms-lyrics-waveform-ffmpeg-executable "ffmpeg"
  "Path to the ffmpeg executable used for waveform analysis."
  :type 'string
  :group 'emms-lyrics)

(defvar-local emms-lyrics-waveform--data nil
  "Cached waveform amplitude vector for the current track.")

(defun emms-lyrics-waveform-analyze-async (file-path callback)
  "Analyze FILE-PATH asynchronously and call CALLBACK with amplitude vector."
  ;; TODO: spawn ffmpeg subprocess:
  ;;   ffmpeg -i FILE -filter:a "aformat=channel_layouts=mono,
  ;;           astats=metadata=1:reset=1" -f null -
  ;; parse per-chunk RMS/peak from stderr, normalize to [0.0, 1.0] vector
  nil)

(defun emms-lyrics-waveform-render (amplitude-vector position-ms duration-ms)
  "Return a rendered waveform string for AMPLITUDE-VECTOR.
POSITION-MS and DURATION-MS are used to place the progress cursor."
  ;; TODO: implement unicode sparkline and SVG render paths
  nil)

(defun emms-lyrics-waveform-insert (buffer position-ms)
  "Update the waveform display in BUFFER for current playback POSITION-MS."
  ;; TODO: implement
  nil)

(provide 'emms-lyrics-waveform)
;;; emms-lyrics-waveform.el ends here
