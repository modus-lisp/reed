#!/bin/bash
# Build a FLAC test corpus with ffmpeg (differential oracle) — but note that FLAC is lossless, so
# the primary check is the STREAMINFO MD5 the encoder wrote, which needs no oracle at all.
#
# The matrix is chosen for the PATHS it takes, not to be a sample of music.  Compression level
# picks the predictor: level 0 uses fixed predictors almost exclusively, level 8 searches high LPC
# orders and deep partition trees.  Silence gives constant subframes; white noise at level 0 gives
# verbatim ones.  Masking low bits gives wasted-bits coding, which nothing else reaches.
set -e
OUT="${REED_CORPUS:-$(cd "$(dirname "$0")/.." && pwd)/corpus}"
mkdir -p "$OUT"
SR=44100
SRC="$OUT/music_src.wav"

enc() { local name="$1"; shift; ffmpeg -y -v error "$@" "$OUT/flac_${name}.flac"; }

if [ -f "$SRC" ]; then
  enc music_c0  -i "$SRC" -c:a flac -compression_level 0
  enc music_c5  -i "$SRC" -c:a flac -compression_level 5
  enc music_c8  -i "$SRC" -c:a flac -compression_level 8
  enc music_c12 -i "$SRC" -c:a flac -compression_level 12
  enc music_mono -i "$SRC" -ac 1 -c:a flac -compression_level 5
  enc music_8k  -i "$SRC" -ar 8000 -c:a flac -compression_level 5
  enc music_24  -i "$SRC" -c:a flac -sample_fmt s32 -bits_per_raw_sample 24 -compression_level 5
  # a small, odd block size: exercises the uncommon-block-size codes and partition order 0
  enc music_odd -i "$SRC" -t 1 -c:a flac -frame_size 1111 -compression_level 5
  # the two stereo modes ffmpeg's encoder never chooses on its own.  Its `auto' picks left/side
  # almost always, so side/right and mid/side have to be asked for by name or they go untested —
  # and the reference encoder's default is mid/side, so real files in the wild use it constantly.
  enc music_rs  -i "$SRC" -t 3 -c:a flac -ch_mode right_side -compression_level 5
  enc music_ms  -i "$SRC" -t 3 -c:a flac -ch_mode mid_side  -compression_level 5
  enc music_ind -i "$SRC" -t 3 -c:a flac -ch_mode indep     -compression_level 5
  # low bits masked: the wasted-bits path, which no ordinary encode reaches
  ffmpeg -y -v error -i "$SRC" -af "aeval=floor(val(0)/256)*256:c=same" -c:a flac \
         -compression_level 5 "$OUT/flac_wasted.flac"
else
  echo "corpus/music_src.wav missing; skipping the music fixtures" >&2
fi

# silence -> constant subframes; white noise at level 0 -> verbatim subframes
ffmpeg -y -v error -f lavfi -i "anullsrc=r=$SR:cl=stereo:d=2" -c:a flac "$OUT/flac_silence.flac"
ffmpeg -y -v error -f lavfi -i "anoisesrc=d=2:c=white:r=$SR:a=0.99" -ac 2 -sample_fmt s16 \
       -c:a flac -compression_level 0 "$OUT/flac_noise.flac"
# the same at 24 bits: wide residuals need Rice parameters above 14, which is the only thing that
# makes an encoder reach for the 5-bit parameter form
ffmpeg -y -v error -f lavfi -i "anoisesrc=d=2:c=pink:r=$SR:a=0.9" -ac 2 -sample_fmt s32 \
       -bits_per_raw_sample 24 -c:a flac -compression_level 5 "$OUT/flac_noise24.flac"
# UNIFORMLY RANDOM samples, which is the one thing no predictor can help with: every subframe
# costs more to model than to store, and the encoder falls back to verbatim.  Filtered noise is not
# enough — anoisesrc is band-limited and a fixed predictor still wins on it.
head -c 176400 /dev/urandom > "$OUT/.urandom.raw"
ffmpeg -y -v error -f s16le -ar $SR -ac 2 -i "$OUT/.urandom.raw" -c:a flac \
       -lpc_type none -compression_level 0 "$OUT/flac_incompressible.flac"
rm -f "$OUT/.urandom.raw"

# the reference decode of each, for the byte-for-byte comparison
for f in "$OUT"/flac_*.flac; do
  n=$(basename "$f" .flac)
  ffmpeg -y -v error -i "$f" -f s16le "$OUT/${n}_ref.s16"
done
