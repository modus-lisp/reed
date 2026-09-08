# reed

A **multi-codec audio library in pure Common Lisp**, with **no FFI** — the audio
analog of [pigment](https://github.com/modus-lisp/pigment) for images. One flat
package (`#:reed`) hosts several codecs, each turning bytes into (or out of) a
shared PCM representation, entirely in Lisp.

Codecs today:

- **MP3** — an MPEG-1/2/2.5 Audio Layer III decoder (frame sync and header
  parsing, ID3 and Xing/Info/VBRI handling, the bit reservoir, Huffman decoding,
  requantization, MS/intensity stereo, alias reduction, the IMDCT, and the
  polyphase synthesis filterbank). Verified bit-accurate to ffmpeg/minimp3.
- **AAC-LC** — an MPEG-4 AAC Low-Complexity decoder (Audio Object Type 2): ADTS
  framing and MP4/M4A demux, the full raw-data-block syntax (SCE/CPE/LFE, section
  and scalefactor data, the 11 spectral Huffman codebooks with escape coding),
  inverse quantization, M/S and intensity stereo, PNS, TNS, and the sine/KBD
  IMDCT filterbank with all four window sequences. Verified to correlation
  1.000000 against ffmpeg and cross-checked against FAAD2.
- **Vorbis I** — floor 1, all three residue formats, square polar channel
  coupling, block switching with the hybrid windows that lap a long block
  against a short one, and Ogg demux. Verified against libvorbis through ffmpeg:
  **ten fixtures at correlation 1.000000** with a relative RMS error under
  1e-4 — mono and stereo, q0 through q10, 22 and 44.1 kHz, transients, noise and
  sweeps. Floor 0 (line spectral pairs) is refused by name; no encoder in use has
  emitted it since the format was frozen. The inverse MDCT is one complex FFT of
  size N/4, checked at every block size against the direct sum it replaced, which
  stays in the tree as its oracle. `src/vorbis/NOTES.md` has the account,
  including the derivation and the two places the specification says the opposite
  of every other codec here.
- **G.711** — ITU-T PCMU (µ-law) and PCMA (A-law) companding, encode and decode.
  Bit-exact to the ITU reference; used on the wire by
  [webrtc-media](https://github.com/modus-lisp/webrtc-media)'s RTP/SRTP audio.
- **Opus** (RFC 6716) — a **full decoder: CELT + SILK + hybrid + Ogg**. The
  shared range decoder and TOC/packet framing (code 0/1/2/3) feed three modes:
  - **CELT** (music path): coarse/fine energy, band allocation, PVQ (CWRS),
    transient handling, anti-collapse, the inverse MDCT with overlap-add, the
    pitch post-filter and de-emphasis, at 48 kHz, mono/stereo, all four frame
    sizes (2.5/5/10/20 ms) and bandwidths (NB/WB/SWB/FB).
  - **SILK** (speech path): a bit-exact fixed-point port — per-frame VAD framing,
    predictive mid/side stereo, subframe gains, NLSF stage-1/2 VQ with
    stabilization + interpolation + NLSF→LPC, LTP pitch lags and 5-tap filters,
    the shell-coded excitation, LTP/LPC synthesis, and libopus's exact polyphase
    resampler from the SILK internal rate (NB 8 / MB 12 / WB 16 kHz) up to 48 kHz.
    Mono and stereo, 10/20/40/60 ms, including mono↔stereo transitions.
  - **Hybrid** (SWB/FB, configs 12–15): SILK decodes the low band and CELT the
    high band (start band 17) from the *same* range decoder in sequence, summed;
    plus the SILK↔CELT redundancy frames that cross-fade mode switches.
  - **Ogg `.opus` demux** (RFC 3533 + RFC 7845): OggS page parse and packet
    reassembly, the OpusHead header (pre-skip, output gain, mapping family 0),
    OpusTags skip, pre-skip/end-granule trimming — so real `.opus` files decode.
  Verified against the official libopus conformance tools (`opus_compare`):
  **all 12 official RFC 6716 test vectors pass**, reproducing the encoder's
  range-coder final state **bit-exactly on every packet** of all twelve. Real
  `.opus` files decode to correlation **1.0** (music) / **0.9998** (voice) vs
  libopus with exact pre-skip alignment. The decoder follows the original RFC
  6716 semantics by default (matching the test vectors); the RFC 8251 decoder
  update (a libopus-1.4-compatible hybrid fold) is available via
  `*opus-rfc8251*`. PLC/FEC (LBRR/CNG) and multichannel mapping families 1/255
  are the only deferred pieces (file decode needs neither).

Planned next: **HE-AAC/SBR** (see *Next steps*).

Every existing Common Lisp MP3 option binds a C library (`cl-mpg123` →
`libmpg123`). `reed` fills the gap with a self-contained, dependency-free
library. It joins the [modus-lisp](https://github.com/modus-lisp) stack of
pure-CL, no-FFI libraries (weft, loom, scribe, gesso, pigment, folio, cram).

MP3's patents expired worldwide in 2017 and the core AAC-LC patents have since
expired as well, so these are unencumbered clean-room implementations. Tables and
algorithms follow ISO/IEC 11172-3 (MPEG-1) and 13818-3 (MPEG-2 LSF) for MP3 and
ISO/IEC 14496-3 (MPEG-4 Audio) for AAC-LC; G.711 follows ITU-T Rec. G.711.

## Status

### MP3 (MPEG-1/2/2.5 Layer III)

| Feature | Status |
| --- | --- |
| MPEG-1 Layer III (44.1/48/32 kHz, mono/stereo/joint/dual) | **complete, verified bit-accurate to ffmpeg** |
| Bit reservoir, all 32 Huffman tables, MS + intensity stereo | complete |
| Short/long/start/stop blocks, mixed blocks, alias reduction, IMDCT | complete |
| MPEG-2 / 2.5 LSF (22.05/24/16/11.025/12/8 kHz) | **implemented, verified** (see note) |
| 16-bit and float32 output, WAV writer, streaming frame API | complete |
| Layer I / Layer II | out of scope (deferred) |
| Free-format bitrate | not decoded (skipped cleanly, no crash) |
| Gapless/Xing seeking | deferred (Xing/Info/VBRI tags are parsed) |

> MPEG-2/2.5 note: `reed` is bit-accurate at every LSF sample rate. At 24 kHz on
> transient/short-block frames, ffmpeg's MP3 decoder produces a slightly different
> result (whole-file corr ≈ 0.9977) — but `reed` matches **minimp3**, an
> independent reference decoder, to full precision (corr 1.000000) on the exact
> same files, so this residual is an ffmpeg-vs-spec decoder difference, not a
> `reed` defect. See `test/reed-24k-vs-minimp3.png`.

### AAC-LC (MPEG-4 Audio Object Type 2)

| Feature | Status |
| --- | --- |
| ADTS framing (sync, header, per-frame config) + frame iteration | **complete** |
| MP4 / M4A demux (ISO-BMFF: moov/trak/stbl, esds AudioSpecificConfig, stsz/stsc/stco/co64) | **complete** |
| Encoder delay + padding removed as the container declares it (edts/elst, Apple iTunSMPB) | **complete, lag-gated** |
| SCE / CPE / LFE elements; DSE / FIL consumed; PCE / CCE rejected cleanly | complete |
| ICS: all window sequences (long / start / eight-short / stop), sine **and** KBD windows, scalefactor grouping | complete |
| Section data, differential scalefactors, 11 spectral Huffman codebooks incl. codebook-11 escape | complete |
| Inverse quant (`x^{4/3}`), scalefactor gain, M/S stereo, intensity stereo, PNS, pulse tool | complete |
| TNS (parcor→LPC, all-pole filter, per-window direction/order) | complete |
| 2048/256-point IMDCT, window transitions, overlap-add; 16-bit + float32 output | complete |
| HE-AAC / SBR + Parametric Stereo | out of scope (deferred) |
| Main-profile prediction, LTP, SSR gain control, ER/LD/ELD syntaxes | out of scope (LC only) |

AAC-LC is the payload of the overwhelming majority of `.m4a`/`.mp4`, HLS, and
streaming AAC. `reed` decodes both the ADTS elementary stream (`.aac`) and MP4
containers (`.m4a`/`.mp4`) through the same core, with the config coming from the
ADTS header or the `esds` box respectively. The core AAC-LC patents have expired;
this is a clean-room implementation following ISO/IEC 14496-3. Verified to
correlation **1.000000** (per-sample RMS within 1 LSB) against ffmpeg across
tonal, sweep, noise, and music signals at 96k/128k/256k/VBR, mono and stereo, and
44.1/48/32 kHz, and cross-checked against **FAAD2** (agreement ≥ 0.9999 on tonal
and music; the perceptual-noise-substitution bands are decoder-defined random and
match the ffmpeg oracle exactly). Decode runs ≈ 3.3× real time. See
`test/reed-vs-ffmpeg-aac.png`.

> **A correlation cannot see a delay.** An AAC encoder cannot start at sample
> zero — its filterbank needs a frame of overlap first — so a correct decode
> begins with priming that is not part of the recording, and how much is the
> container's to declare rather than the bitstream's. `reed` read neither
> declaration until now, and nothing caught it: `test/aac-compare.py`
> cross-correlates to find its own alignment before it measures anything, so a
> decode uniformly 57 ms late scored 1.000000 and passed. What found it was a
> speech recognizer reading the same `.m4a` twice, once through `reed` and once
> through ffmpeg, and disagreeing with itself about two words. `decode-m4a` now
> honours both `edts`/`elst` and Apple's `iTunSMPB` tag (`:trim nil` keeps every
> sample the decoder produced), and `inspect/mp4-delay-gate.lisp` asserts the
> lag against a reference decode is the integer **0** — with the same file
> untrimmed required to come back as exactly the declared delay, so that the
> gate can tell "we removed the priming" from "we happen to agree".

### G.711 (ITU-T PCMU / PCMA)

| Feature | Status |
| --- | --- |
| µ-law (PCMU) encode + decode | **complete, bit-exact to the ITU reference** |
| A-law (PCMA) encode + decode | **complete, bit-exact to the ITU reference** |
| Buffer transforms (PCM frame ↔ codeword frame) + per-sample entry points | complete |

G.711 is stateless per-sample companding between 16-bit linear PCM and 8-bit
logarithmic codewords. `reed`'s encoder matches the canonical ITU-T G.711
reference (the Sun/Reese-Campbell `exp_lut` / segment-search formulation) on all
65 536 possible input samples for both laws, and the decode tables match the
reference on all 256 codewords. (ffmpeg's `pcm_mulaw`/`pcm_alaw` rounds ~1.8 % of
samples to the *adjacent* codeword at segment boundaries and is itself the
outlier — the same pattern as the MP3 24 kHz case above.) See
`test/g711-test.lisp`.

## Verification

ffmpeg (libmp3lame) is the differential oracle: each MP3 is decoded by both
`reed` and `ffmpeg -i x.mp3 x.wav`, aligned (MP3 has a ~1105-sample
encoder/decoder delay), and compared by Pearson correlation and per-sample RMS
over 16-bit samples. MP3 decoders are not required to be bit-exact (IMDCT/rounding
differ), but a correct decoder tracks a reference to very high correlation.

Corpus: sine tones, a log sweep, pink noise, a synthetic two-tone stereo signal,
and a real music clip, encoded across bitrates (CBR 128/192/320, VBR `-q2`),
channel modes (stereo / joint-stereo / mono), and sample rates.

```
file                 corr        rms (of int16)
sine440_cbr128       1.000000    0.28
sine1k_cbr320        1.000000    0.24
sweep_cbr192         1.000000    0.53
noise_cbr128         1.000000    0.21
stereo2_js128        1.000000    0.28   (joint stereo)
stereo2_stereo192    1.000000    0.28   (L/R stereo)
music_cbr128         1.000000    0.25
music_cbr320         1.000000    0.24
music_vbr            1.000000    0.68
music_mono128        1.000000    0.25
music_32k            1.000000    0.25
music_48k            1.000000    0.25
music_22k (MPEG-2)   1.000000    0.24
music_24k (MPEG-2)   0.997695  212.8    (vs ffmpeg; = 1.000000 vs minimp3, see note)
```

The `music_24k` line is the only entry below 1.0 against ffmpeg. Cross-checking
against **minimp3** (a second, independent decoder) shows `reed` and minimp3
agree to corr **1.000000** whole-file on that exact file, while *both* differ
from ffmpeg by the same 0.9977 on a handful of transient frames — i.e. ffmpeg's
decoder is the outlier at 24 kHz, not `reed`. At 22.05 kHz all three agree.

Real-world files decoded and matched against ffmpeg (all corr = 1.000000):

```
greeting.mp3       0.8s, 24kHz  mono, MPEG-2   corr=1.000000
notification.mp3   1.7s, 44.1kHz stereo        corr=1.000000
rnd_lib_a1/a3/a4/c1/c2/c3.mp3  ~200-500s each, 44.1kHz mono   corr=1.000000
```

Robustness: truncated files, random garbage, a 5 MB ID3v2 tag, mid-frame stream
starts, empty input, and free-format (bitrate-index 0) frames all decode without
crashing, hanging, or raising an unhandled condition (garbage/free-format yield
zero output; valid frames after a bad prefix are resynced).

`test/reed-vs-ffmpeg.png` overlays the reed and ffmpeg waveforms and
spectrograms for a music clip (visually identical);
`test/reed-24k-vs-minimp3.png` shows reed ≡ minimp3 with ffmpeg diverging on
24 kHz transients. `test/reed-opus-music.wav` is a short decoded-audio artifact
(from the Opus decoder). The larger decoded WAVs regenerate from the corpus
scripts and are not committed.

Regenerate everything: `bash test/gen-corpus.sh` then
`sbcl --load test/decode-all.lisp` and `python3 test/compare.py <ref> <out>`.

Two gates measure what a correlation cannot. `inspect/dsp-gate.lisp` covers the
resampler, downmix, mixer, and player. `inspect/mp4-delay-gate.lisp` covers
where an `.m4a`'s audio starts: its bar is that the lag against ffmpeg's decode
is the integer 0, with the same file decoded `:trim nil` required to come back
as exactly the delay the container declared. Its fixtures come from
`python3 test/gen-mp4-delay.py`, which builds one file per declaration mechanism
and disables the other in each, so neither can pass on the strength of the one
it is not testing.

### Performance

Pure double-float DSP, no SIMD, single thread: ~40–75× real time on this
machine (e.g. a 218 s file decodes in ~2.9 s). Comfortably real-time.

## Usage

```lisp
(ql:quickload :reed)

;; one-shot decode to a PCM struct
(let ((pcm (reed:decode-mp3-file "song.mp3")))          ; :format :pcm16 (default) or :float32
  (reed:pcm-channels pcm)      ; => 2
  (reed:pcm-sample-rate pcm)   ; => 44100
  (reed:pcm-frame-count pcm)   ; => samples per channel
  (reed:pcm-samples pcm)       ; => interleaved (signed-byte 16) vector
  (reed:write-wav pcm "song.wav"))

;; from an octet vector
(reed:decode-mp3 octets :format :float32)   ; samples are single-float in [-1,1]

;; streaming, one frame at a time
(let ((d (reed:make-decoder octets)))
  (loop for frame = (reed:decode-next-frame d) while frame
        do ...))                              ; FRAME is interleaved samples

;; WAV bytes without a file
(reed:write-wav pcm t)                        ; => RIFF/WAVE octet vector

;; --- AAC-LC: ADTS (.aac) or MP4/M4A (.m4a/.mp4), auto-detected ---
(reed:decode-aac-file "clip.m4a")             ; => pcm struct  (also "stream.aac")
(reed:decode-aac octets :format :float32)     ; ADTS/raw octets, or an .m4a in memory

;; --- Opus (RFC 6716): full CELT + SILK + hybrid, Ogg .opus or raw packets ---
(reed:decode-opus-file "clip.opus")           ; => pcm @ 48 kHz (Ogg demux, pre-skip applied)
(reed:decode-opus octets)                     ; auto-detects Ogg vs a raw Opus packet
;; set reed:*opus-rfc8251* to t for the RFC 8251 update (libopus-1.4-compatible hybrid)

;; --- G.711 companding (buffer transforms) ---
;; (signed-byte 16) PCM  <->  (unsigned-byte 8) codewords
(let ((codes (reed:pcmu-encode pcm-samples)))  ; µ-law; or reed:pcma-encode for A-law
  (reed:pcmu-decode codes))                     ; => (signed-byte 16) PCM
;; mulaw-*/alaw-* are aliases for pcmu-*/pcma-*; *-1 variants compand one sample.
```

## Architecture

The package is flat (`#:reed`); modularity is in the file layout. `src/`, loaded
in order:

**`src/common/`** — shared substrate across all codecs:

- `packages` — the single `#:reed` package (every codec's exports)
- `bitreader` — MSB-first bit reader over a byte window (reusable)
- `pcm` — the **uniform PCM representation** (`pcm` struct: interleaved
  `samples`, `channels`, `sample-rate`, `format`, `frame-count`) plus the
  RIFF/WAVE writer. Every decoder emits into this type.

**`src/mp3/`** — the MP3 (MPEG-1/2/2.5 Layer III) pipeline:

- `tables` — the large generated constants: the 32 Huffman decode trees and the
  512-entry synthesis window
- `huffman` — big-value and count1 (quad) Huffman decoding, linbits/ESC, signs
- `header` — frame header, ID3v2/v1, Xing/Info/VBRI detection, sync search
- `sideinfo` — Layer III side information (MPEG-1 2-granule and MPEG-2 1-granule)
- `requantize` — scalefactor-band tables, the x^(4/3) power table, dequantization
- `stereo` — MS and intensity stereo
- `imdct` — reorder, alias reduction, the IMDCT with all four window types,
  overlap-add, frequency inversion
- `synthesis` — the 32-band polyphase synthesis filterbank
- `layer3` — scalefactor and Huffman main-data parsing, per-granule pipeline
- `decode` — container framing, bit-reservoir assembly, output formats

**`src/aac/`** — the AAC-LC (MPEG-4 AOT 2) pipeline:

- `tables` — generated ISO/IEC 14496-3 constants: the 11 spectral Huffman
  codebooks and the scalefactor codebook, scalefactor-window-band offsets, TNS
  coefficient maps, sample-rate table (`test/gen-aac-tables.py`)
- `structs` — persistent per-channel state (coefficients, overlap buffer, ICS
  info, TNS)
- `huffman` — prefix-code decoders, spectral-band decode (tuple expansion,
  signs, codebook-11 escape), `x^{4/3}` inverse quant, sine + Kaiser-Bessel
  derived windows
- `filterbank` — 2048/256-point IMDCT (cosine-matrix), window assembly for all
  four window sequences, overlap-add
- `tools` — TNS (parcor→LPC + all-pole filter), M/S and intensity stereo
- `decode` — raw_data_block syntax (SCE/CPE/LFE/DSE/FIL), ADTS framing, PNS,
  pulses, the `decode-aac` / `decode-aac-file` entry points
- `mp4` — a minimal ISO-BMFF (MP4/M4A) demuxer: box walking, the `esds`
  AudioSpecificConfig, and `stsz`/`stsc`/`stco`/`co64` sample extraction

**`src/g711.lisp`** — ITU-T G.711 PCMU/PCMA companding.

### Codec entry-point convention

New codecs slot into this shape:

- **Container/frame codecs** (MP3 today; AAC-LC and Opus next) expose a
  `decode-<codec>` / `decode-<codec>-file` pair returning a `pcm` struct, and may
  add a streaming `make-decoder` / `decode-next-frame` API. Their pipeline files
  live under `src/<codec>/`, and they emit into the shared `pcm` representation.
- **Sample companders** (G.711) are stateless buffer transforms:
  `<name>-encode` / `<name>-decode` over sample vectors, with `*-1` per-sample
  variants. These operate on the raw sample vectors that back a `pcm` (the
  on-wire buffers RTP wants), so they interoperate with `pcm` without forcing a
  struct onto the packet path.

## Next steps

MP3 is correctness-complete (bit-accurate to minimp3 across the corpus), AAC-LC
tracks ffmpeg to correlation 1.000000 (ADTS and MP4), and G.711 is bit-exact to
the ITU reference. The library is built to grow:

1. **Opus** (`src/opus/`): the decoder is complete and conformance-verified —
   CELT, SILK, hybrid, and Ogg `.opus` demux, passing all 12 official RFC 6716
   vectors. The only deferred pieces are **PLC/FEC** (LBRR frames are parsed and
   skipped; packet-loss concealment / CNG matter for a lossy transport, not file
   decode) and **multichannel** mapping families 1/255 (family 0 mono/stereo is
   done). These are additive and outside the file-decode path.
2. **HE-AAC**: SBR (Spectral Band Replication) and Parametric Stereo on top of
   the AAC-LC core, for low-bitrate AAC+ streams.
3. AAC: an FFT-based IMDCT to replace the direct cosine-matrix filterbank (the
   current ≈3.3× real-time cost is dominated by it); the 960/480-sample frame
   lengths; a streaming `decode-next-frame` API.
4. MP3: gapless playback via the Xing/LAME encoder-delay tags; seeking via the
   TOC; free-format (bitrate index 0) frame decoding; Layer I / Layer II.

## License

MIT © 2026 ynniv. See `LICENSE`.
