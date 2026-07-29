#!/bin/bash
# Build an MP3 test corpus with ffmpeg (differential oracle).
set -e
# Output dir (override with $REED_CORPUS); defaults to <repo>/corpus.
OUT="${REED_CORPUS:-$(cd "$(dirname "$0")/.." && pwd)/corpus}"
mkdir -p "$OUT"
SR=44100
gen() { # name  ffmpeg-source-args
  local name="$1"; shift
  ffmpeg -y -v error "$@" "$OUT/${name}_src.wav"
}
# --- source signals (5s each, stereo float src) ---
gen sine440   -f lavfi -i "sine=frequency=440:sample_rate=$SR:duration=5"
gen sine1k    -f lavfi -i "sine=frequency=1000:sample_rate=$SR:duration=5"
gen sweep     -f lavfi -i "sine=frequency=100:sample_rate=$SR:duration=5" -af "aeval=0.5*sin(2*PI*(100+ (20000-100)*t/5)*t):c=same" 2>/dev/null || \
  ffmpeg -y -v error -f lavfi -i "aevalsrc='0.4*sin(2*PI*(200*t + 2000*t*t))':s=$SR:d=5" "$OUT/sweep_src.wav"
gen noise     -f lavfi -i "anoisesrc=d=5:c=pink:r=$SR:a=0.3"
# two-tone stereo (different per channel)
ffmpeg -y -v error -f lavfi -i "sine=frequency=440:sample_rate=$SR:duration=5" \
  -f lavfi -i "sine=frequency=660:sample_rate=$SR:duration=5" \
  -filter_complex "[0:a][1:a]join=inputs=2:channel_layout=stereo" "$OUT/stereo2_src.wav"
# real music clip: decode any mp3/audio file to wav (10s). Point $REED_MUSIC_SRC
# at a file of your choice; if unset, this step is skipped.
if [ -n "${REED_MUSIC_SRC:-}" ] && [ -f "$REED_MUSIC_SRC" ]; then
  ffmpeg -y -v error -i "$REED_MUSIC_SRC" -t 10 -ar $SR -ac 2 "$OUT/music_src.wav"
else
  echo "REED_MUSIC_SRC unset or missing; skipping the real-music clip." >&2
fi

# --- encode matrix ---
encode() { # src  outname  extra-args
  local src="$1" name="$2"; shift 2
  ffmpeg -y -v error -i "$OUT/${src}_src.wav" "$@" "$OUT/${name}.mp3"
  ffmpeg -y -v error -i "$OUT/${name}.mp3" "$OUT/${name}_ref.wav"
}
# stereo signals across bitrate/mode/rate
encode sine440  sine440_cbr128       -b:a 128k
encode sine1k   sine1k_cbr320        -b:a 320k
encode sweep    sweep_cbr192         -b:a 192k
encode noise    noise_cbr128         -b:a 128k
encode stereo2  stereo2_js128        -b:a 128k -joint_stereo 1
encode stereo2  stereo2_stereo192    -b:a 192k -joint_stereo 0
encode music    music_cbr128         -b:a 128k
encode music    music_cbr320         -b:a 320k
encode music    music_vbr            -q:a 2
encode music    music_mono128        -b:a 128k -ac 1
encode music    music_48k            -b:a 192k -ar 48000
encode music    music_32k            -b:a 128k -ar 32000
# MPEG-2 LSF (low sample rates)
encode music    music_24k            -b:a 64k  -ar 24000
encode music    music_22k            -b:a 64k  -ar 22050
echo "CORPUS DONE"; ls -1 "$OUT"/*.mp3 | wc -l
