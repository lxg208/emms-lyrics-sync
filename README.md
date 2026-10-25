Synchronized lyrics display for [EMMS](https://www.gnu.org/software/emms/), inspired by OpenLyrics for foobar2000.


# Status

Functional and in daily use.  Three display modes coexist: a dedicated
cover/metadata panel, a scrollable lyrics panel with current-line
auto-scroll, and a waveform progress bar.  Lyrics are fetched
asynchronously through a pluggable source pipeline.  Seeking via
keyboard, mouse, or waveform click is supported.


# Features


## Display

-   Three stacked side windows, grouped atomically:
    -   **Metadata**: cover art + artist/album/title/tech header
    -   **Lyrics**: full song text, current line highlighted, auto-scrolled
        to position 3 from the top
    -   **Waveform**: L/R loudness bar with playback cursor and mode line
        showing elapsed/duration + artist + title
-   Deterministic SVG placeholder cover for tracks with no embedded or
    sidecar art (color + initial derived from file path hash)
-   Face properties for past / current / future lyric lines, plus A2
    karaoke word-level highlighting for the current line
-   Material-Design-coherent color palette that inherits from the active
    Emacs theme


## Lyrics sources (ordered, pluggable)

1.  Local `.lrc` sidecar file
2.  Embedded tag (`ID3 USLT` / Vorbis `LYRICS`)
3.  [LRCLIB](https://lrclib.net) (free, synced, no API key)
4.  QQ Music (strong Chinese-language catalog, per-phrase timestamps)
5.  NetEase Cloud Music
6.  [lyrics.ovh](https://lyrics.ovh) (plain text fallback)

Sources run sequentially and stop on first success.  Async throughout;
no blocking on track start.  Failed results are cached to avoid repeat
requests.


## Playback position

-   mpv IPC when available (accurate to ~10 ms), with automatic
    reconnect and cold-start seek queueing
-   Falls back to `emms-playing-time` when mpv IPC is disabled or
    unreachable
-   Works on WSL via `socat` bridge to Windows named pipe (see
    `emms-player-wsl-mpv.el`)


## Seeking

-   `RET` on any lyric line seeks to that line's timestamp
-   Click on the waveform seeks to that position
-   Works whether the player is running, paused, or stopped (stopped
    starts the track and queues the seek)


# Quick start

    (use-package emms-lyrics-sync
      :after emms
      :config
      (setq emms-lyrics-sync-display-font-family "SauceCodePro Nerd Font")
      (setq emms-lyrics-sync-display-cjk-font    "FZYouH_504L")
      (setq emms-lyrics-sync-display-cjk-scale   1.10)
      (setq emms-lyrics-sync-display-metadata-lines
            (pcase (system-name)
              ("S195"   16)
              ("Kronos" 16)
              ("M14"    12)
              (_        14)))
      (emms-lyrics-sync-mode 1))

The sidebar appears automatically on track start.  Toggle with the
keymap below.


# Requirements

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Component</th>
<th scope="col" class="org-left">Required</th>
<th scope="col" class="org-left">Notes</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Emacs</td>
<td class="org-left">yes</td>
<td class="org-left">28.1+ (tested on 31.1 and 32.0.50)</td>
</tr>

<tr>
<td class="org-left">EMMS</td>
<td class="org-left">yes</td>
<td class="org-left">&nbsp;</td>
</tr>

<tr>
<td class="org-left"><a href="https://github.com/alphapapa/plz.el">plz.el</a></td>
<td class="org-left">yes</td>
<td class="org-left">async HTTP for remote sources</td>
</tr>

<tr>
<td class="org-left"><code>ffmpeg</code></td>
<td class="org-left">optional</td>
<td class="org-left">waveform generation, embedded cover extract</td>
</tr>

<tr>
<td class="org-left"><code>ffprobe</code></td>
<td class="org-left">optional</td>
<td class="org-left">tech-info augmentation</td>
</tr>

<tr>
<td class="org-left"><code>python3</code> + <code>freetype-py</code></td>
<td class="org-left">optional</td>
<td class="org-left">x-height calibration tool</td>
</tr>

<tr>
<td class="org-left">mpv (with IPC)</td>
<td class="org-left">optional</td>
<td class="org-left">accurate playback position</td>
</tr>
</tbody>
</table>


# Keybindings


## Sidebar (all three windows)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left"><code>q</code></td>
<td class="org-left">Hide the sidebar</td>
</tr>

<tr>
<td class="org-left"><code>SPC</code></td>
<td class="org-left">Pause / resume (<code>emms-playlist-mode-map</code>)</td>
</tr>

<tr>
<td class="org-left"><code>n</code> / <code>p</code></td>
<td class="org-left">Next / previous track</td>
</tr>

<tr>
<td class="org-left"><code>m</code></td>
<td class="org-left">Mute</td>
</tr>

<tr>
<td class="org-left"><code>RET</code></td>
<td class="org-left">Seek to lyric line at point</td>
</tr>

<tr>
<td class="org-left"><code>mouse-1</code></td>
<td class="org-left">Seek (on waveform only)</td>
</tr>

<tr>
<td class="org-left"><code>F3 L s</code></td>
<td class="org-left">Manual lyrics search</td>
</tr>

<tr>
<td class="org-left"><code>F3 L n</code></td>
<td class="org-left">Next search result</td>
</tr>

<tr>
<td class="org-left"><code>F3 L t</code></td>
<td class="org-left">Toggle sidebar</td>
</tr>

<tr>
<td class="org-left"><code>F3 L c</code></td>
<td class="org-left">Live-tune CJK scale</td>
</tr>

<tr>
<td class="org-left"><code>F3 L r</code></td>
<td class="org-left">Reload lyrics</td>
</tr>

<tr>
<td class="org-left"><code>F3 L o</code></td>
<td class="org-left">Open <code>lrc</code> sidecar file</td>
</tr>
</tbody>
</table>


# Configuration


## Display

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Variable</th>
<th scope="col" class="org-left">Default</th>
<th scope="col" class="org-left">Purpose</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left"><code>emms-lyrics-sync-display-side</code></td>
<td class="org-left"><code>right</code></td>
<td class="org-left">Sidebar edge</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-width</code></td>
<td class="org-left"><code>52</code></td>
<td class="org-left">Sidebar width in columns</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-metadata-lines</code></td>
<td class="org-left"><code>15</code></td>
<td class="org-left">Metadata window height</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-waveform-height</code></td>
<td class="org-left"><code>3</code></td>
<td class="org-left">Waveform height in lines</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-waveform-side-padding</code></td>
<td class="org-left"><code>2</code></td>
<td class="org-left">Padding on each side of bar</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-waveform-center-line</code></td>
<td class="org-left"><code>1</code></td>
<td class="org-left">Center line thickness for silence</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-cover-height</code></td>
<td class="org-left"><code>900</code></td>
<td class="org-left">Hard upper bound on cover px</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-cover-placeholder</code></td>
<td class="org-left"><code>t</code></td>
<td class="org-left">Generate placeholder for missing art</td>
</tr>
</tbody>
</table>


## Fonts

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Variable</th>
<th scope="col" class="org-left">Default</th>
<th scope="col" class="org-left">Purpose</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left"><code>emms-lyrics-sync-display-font-family</code></td>
<td class="org-left"><code>nil</code></td>
<td class="org-left">Latin family (base)</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-font-scale</code></td>
<td class="org-left"><code>1.0</code></td>
<td class="org-left">Latin height multiplier</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-cjk-font</code></td>
<td class="org-left"><code>nil</code></td>
<td class="org-left">CJK family</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-cjk-scale</code></td>
<td class="org-left"><code>1.10</code></td>
<td class="org-left">CJK/Latin relative height</td>
</tr>

<tr>
<td class="org-left"><code>emms-lyrics-sync-display-metadata-scale</code></td>
<td class="org-left"><code>1.0</code></td>
<td class="org-left">Metadata-only height multiplier</td>
</tr>
</tbody>
</table>

The CJK/Latin ratio is applied via face text properties (not
fontsets), so each script gets independent family and height.  See
`DESIGN.org` for why fontsets don't work on Emacs 31/32.


## Sources

    (setq emms-lyrics-sync-sources
          '(emms-lyrics-sync-source-local-lrc
            emms-lyrics-sync-source-tag
            emms-lyrics-sync-source-lrclib
            emms-lyrics-sync-source-qqmusic
            emms-lyrics-sync-source-netease
            emms-lyrics-sync-source-lyricsovh))

Set `emms-lyrics-sync-verbose` to `t` to log every source attempt,
failure, and HTTP status to `*Messages*`.


# Tools


## `M-x emms-lyrics-sync-display-calibrate-cjk-scale`

Measures the x-heights of the configured Latin and CJK fonts via
`freetype-py` and sets `display-cjk-scale` to the mathematically
correct ratio.  Requires `python3` and `freetype-py`.

The mathematical ratio matches x-heights; perceptual preference often
calls for a value 5–20% above it.  The user chooses the final value
after calibration.


## `M-x emms-lyrics-sync-display-scan-cjk-scale`

Scans `display-cjk-scale` across a range, redraws the display at each
step, and measures pixel deltas between consecutive lyric lines.  A
value where all deltas are equal means no jitter — that's the highest
usable scale on the current machine.  Reports `uniform`, `ok (1px)`,
`borderline`, or `JITTER` per value.

Note: with the three-window layout, jitter is eliminated by
construction — the waveform window's Y position is fixed regardless
of lyric line heights.  The scanner remains useful for finding scales
that look good aesthetically.


# Known limitations

-   **Emacs 31.1 / 32.0.50 display-engine regressions.**  The
    `line-height` text property is silently ignored.  The
    `(space :width 0 :height H)` display spec is ignored.  `image-crop`
    was replaced by a different-arity interactive command.  `:crop` and
    `:slice` on inline images render at full size.  The current design
    avoids all of these by using separate windows and file-based
    compositing.
-   **Fontset creation is broken on Emacs 31/32.**
    `create-fontset-from-fontset-spec` registers a name, but
    `set-fontset-font` cannot modify the resulting object and
    `fontset-font` returns only fallback bitmaps.  The package does not
    rely on fontsets.
-   **HiDPI on WSLg.**  WSLg reports a different effective char-height
    than X11 and native — cover art and lyrics appear at different
    physical sizes across machines.  Per-machine tuning of
    `display-metadata-lines` and `display-cjk-scale` compensates.
-   **Linear waveform scale.**  Overly compressed (brickwalled) albums
    render as a solid rectangle because there is no dynamic range to
    show.  This is accurate, not a bug — the point of the display is to
    make loudness wars visible.


# External tools

Companion shell utilities (not part of the package, provided by the
author's dotfiles):

-   `emacs-add-tracks` — add audio files from a directory to EMMS, with
    optional playback.  Handles recursive SSH routing so invocations
    from a remote server target the client's Emacs.
-   `ec` — emacsclient wrapper with socket-finder and platform-aware
    Emacs launch (Windows batch on WSL, direct on native Linux).


# License

GNU General Public License v3.0 or later.  See [License](#orgab9e929).

