# emms-lyrics

> ⚠️ **Work in progress — not functional yet.**
> The package is under active development. No user-facing features are
> available at this time. APIs and file layout may change without notice.

Synchronized lyrics display for [EMMS](https://www.gnu.org/software/emms/),
inspired by the OpenLyrics plugin for foobar2000.

## Planned Features

- Synced LRC scrolling with **word-level karaoke highlighting** (A2 extension)
- Cover art (embedded → `cover.jpg` / `front.jpg` → slideshow)
- Waveform loudness bar with playback progress (dynamic range at a glance)
- Rich track metadata header (artist, composer, album, codec, bit-depth, etc.)
- Pluggable, user-orderable source pipeline:
  - Embedded tags (ID3 USLT / Vorbis `LYRICS=`)
  - Local `.lrc` sidecar files
  - [LRCLIB](https://lrclib.net) (free, synced, no API key)
  - NetEase Cloud Music
  - QQ Music
  - [lyrics.ovh](https://lyrics.ovh) (plain text fallback)
- Manual search UI (pre-filled from track metadata, editable)
- Skip predicates for instrumentals / classical / streams
- mpv IPC for accurate playback position; falls back to `emms-playing-time`

## Requirements (planned)

- Emacs 28.1+
- [EMMS](https://www.gnu.org/software/emms/)
- [plz.el](https://github.com/alphapapa/plz.el) (async HTTP)
- `ffmpeg` or `ffprobe` (optional, for waveform analysis)
- mpv with `--input-ipc-server` (optional, for accurate seek position)

## Installation (not yet available)

The package is not published. Watch this repository for updates.

## License

GNU General Public License v3.0 or later.
See [LICENSE](LICENSE) for details.
