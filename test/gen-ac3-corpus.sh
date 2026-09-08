#!/bin/bash
# Build an AC-3 test corpus with ffmpeg (differential oracle).
#
# The matrix is chosen for the paths it takes.  Bit rate decides how hard the bit allocation has to
# work and therefore how many coefficients get no bits at all; 5.1 brings the LFE channel and the
# coupling of more than two channels; a low-rate stereo encode is where coupling and rematrixing
# both appear; 32 kHz moves every table that is indexed by sample rate.
set -e
OUT="${REED_CORPUS:-$(cd "$(dirname "$0")/.." && pwd)/corpus}"
mkdir -p "$OUT"
SRC="$OUT/music_src.wav"

enc() { local name="$1"; shift; ffmpeg -y -v error "$@" "$OUT/ac3_${name}.ac3"
        ffmpeg -y -v error -i "$OUT/ac3_${name}.ac3" -f s16le "$OUT/ac3_${name}_ref.s16"; }

if [ -f "$SRC" ]; then
  enc stereo_192 -i "$SRC" -c:a ac3 -b:a 192k
  enc stereo_384 -i "$SRC" -c:a ac3 -b:a 384k
  enc stereo_96  -i "$SRC" -c:a ac3 -b:a 96k          # low rate: coupling and heavy allocation
  enc mono_96    -i "$SRC" -ac 1 -c:a ac3 -b:a 96k
  enc 51_448     -i "$SRC" -af "pan=5.1|c0=FL|c1=FR|c2=0.5*FL+0.5*FR|c3=0.1*FL|c4=FL|c5=FR" \
                 -c:a ac3 -b:a 448k                   # six channels, including the LFE
  enc 32k_192    -i "$SRC" -ar 32000 -c:a ac3 -b:a 192k
else
  echo "corpus/music_src.wav missing; skipping" >&2
fi
