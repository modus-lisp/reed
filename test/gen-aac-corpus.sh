#!/bin/bash
# Generate a compact AAC-LC (ADTS) test corpus with ffmpeg's native aac encoder,
# across signals, bitrates, channel modes, and sample rates.  For each x.aac we
# also write x_ref.wav (ffmpeg's own decode) as the primary oracle.  Source WAVs
# are deleted after encoding to conserve disk.
set -e
cd "$(dirname "$0")/.."
D=aac-corpus
mkdir -p $D
FF="ffmpeg -hide_banner -v error -y"
DUR=3

mksrc () { # file lavfi-or-input ac ar [is_input]
  if [ "$4" = "in" ]; then $FF -i "$2" -t $DUR -ac $3 -ar 44100 $D/$1
  else $FF -f lavfi -i "$2" -t $DUR -ac $3 $D/$1; fi
}
enc () { # src label rate ac extra...
  local src=$1 label=$2 rate=$3 ac=$4; shift 4
  $FF -i $D/$src -ar $rate -ac $ac "$@" -c:a aac -f adts $D/$label.aac
  $FF -i $D/$label.aac -f wav $D/${label}_ref.wav
}

mksrc src_sine.wav   "sine=frequency=440:sample_rate=44100:duration=$DUR" 1
mksrc src_sweep.wav  "aevalsrc=0.4*sin(1000*t*t*PI):s=44100:d=$DUR" 1
mksrc src_noise.wav  "anoisesrc=d=$DUR:c=pink:r=44100:a=0.5" 1
mksrc src_music.wav  /mnt/siteglass/render/rnd_lib_a1.mp3 2 in
mksrc src_musmono.wav /mnt/siteglass/render/rnd_lib_c1.mp3 1 in

enc src_sine.wav    sine_m_128   44100 1 -b:a 128k
enc src_sweep.wav   sweep_m_96   44100 1 -b:a 96k
enc src_sweep.wav   sweep_m_256  44100 1 -b:a 256k
enc src_noise.wav   noise_m_128  44100 1 -b:a 128k
enc src_music.wav   music_s_128  44100 2 -b:a 128k
enc src_music.wav   music_s_256  44100 2 -b:a 256k
enc src_music.wav   music_s_96   44100 2 -b:a 96k
enc src_music.wav   music_s_vbr  44100 2 -q:a 1.2
enc src_music.wav   music_s_48k  48000 2 -b:a 192k
enc src_music.wav   music_s_32k  32000 2 -b:a 128k
enc src_musmono.wav music_m_128  44100 1 -b:a 128k

rm -f $D/src_*.wav
echo "corpus:"; ls -1 $D/*.aac
