#!/bin/bash
# Build a Vorbis test corpus with ffmpeg/libvorbis (differential oracle).
# Each fixture is an .ogg plus the reference decode ffmpeg produces from it.
set -e
OUT="${REED_CORPUS:-$(cd "$(dirname "$0")/.." && pwd)/corpus}"
mkdir -p "$OUT"
SR=44100

# The matrix is chosen to reach every path the decoder has, not to be a broad sample of music:
#   q-1 / q10   the extremes of the quality scale, which change the codebooks completely
#   mono        one channel, so no coupling at all
#   stereo      two channels, which is where square polar coupling lives
#   noise       forces short blocks (transients everywhere) and dense residues
#   sweep       exercises the floor across the whole spectrum rather than at one point
gen() { # name  input-args...  encoder-args...
  local name="$1"; shift
  ffmpeg -y -v error "$@" "$OUT/vorbis_${name}.ogg"
  ffmpeg -y -v error -i "$OUT/vorbis_${name}.ogg" "$OUT/vorbis_${name}_ref.wav"
}

gen sine440   -f lavfi -i "sine=frequency=440:sample_rate=$SR:duration=3" -c:a libvorbis -q:a 4
gen sweep     -f lavfi -i "aevalsrc='0.4*sin(2*PI*(200*t + 2000*t*t))':s=$SR:d=3" -c:a libvorbis -q:a 5
gen noise     -f lavfi -i "anoisesrc=d=3:c=pink:r=$SR:a=0.5" -c:a libvorbis -q:a 5
gen clicks    -f lavfi -i "aevalsrc='0.8*sin(2*PI*1000*t)*lt(mod(t,0.25),0.002)':s=$SR:d=3" -c:a libvorbis -q:a 6
gen stereo2   -f lavfi -i "sine=frequency=440:sample_rate=$SR:duration=3" \
              -f lavfi -i "sine=frequency=659:sample_rate=$SR:duration=3" \
              -filter_complex "[0:a][1:a]join=inputs=2:channel_layout=stereo" -c:a libvorbis -q:a 5

if [ -f "$OUT/music_src.wav" ]; then
  gen music_q0  -i "$OUT/music_src.wav" -c:a libvorbis -q:a 0
  gen music_q4  -i "$OUT/music_src.wav" -c:a libvorbis -q:a 4
  gen music_q10 -i "$OUT/music_src.wav" -c:a libvorbis -q:a 10
  gen music_mono -i "$OUT/music_src.wav" -ac 1 -c:a libvorbis -q:a 4
  gen music_22k -i "$OUT/music_src.wav" -ar 22050 -c:a libvorbis -q:a 4
else
  echo "corpus/music_src.wav missing; skipping the real-music fixtures" >&2
fi
