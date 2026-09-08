;;;; reed.asd — a pure Common Lisp audio-codec library.
(asdf:defsystem "reed"
  :description "A multi-codec audio library in pure Common Lisp — the audio
analog of pigment for images: one flat package (#:reed) hosting several codecs,
container to PCM, with no FFI.  Codecs: an MPEG-1/2/2.5 Layer III (MP3) decoder
(container parse, bit reservoir, all 32 Huffman tables, requantization,
MS/intensity stereo, alias reduction, IMDCT, polyphase synthesis; bit-accurate
to ffmpeg/minimp3); an MPEG-4 AAC-LC decoder with ADTS and MP4/M4A demux
(correlation 1.0 vs ffmpeg); a full Opus decoder (RFC 6716: range coder, CELT,
SILK, hybrid, and Ogg .opus demux; passes all 12 official RFC 6716 test
vectors); and the ITU-T G.711 PCMU (mu-law) / PCMA (A-law) companders
(bit-exact to the ITU reference).  Every decoder emits a uniform 16-bit or
float32 interleaved PCM struct.  Clean-room implementations of unencumbered
codecs (MP3 patents expired 2017)."
  :version "0.3.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components
                (;; shared substrate: package, reusable bit reader, the uniform
                 ;; PCM representation + WAV writer
                 (:module "common"
                  :serial t
                  :components ((:file "packages")
                               (:file "bitreader")
                               (:file "pcm")
                               ;; what sits between a decoder and a device:
                               ;; rate conversion, downmix, gain, the mixer
                               (:file "dsp")))
                 ;; Layer II's tables come first because the shared frame header needs its bit
                 ;; rate table: Layer II and Layer III do not agree about what a bit rate index
                 ;; means, and one header parser serves both.
                 (:file "mp2/tables")
                 ;; MP3 (MPEG-1/2/2.5 Layer III) decode pipeline
                 (:module "mp3"
                  :serial t
                  :components ((:file "tables")
                               (:file "huffman")
                               (:file "header")
                               (:file "sideinfo")
                               (:file "requantize")
                               (:file "stereo")
                               (:file "imdct")
                               (:file "synthesis")
                               (:file "layer3")
                               (:file "decode")))
                 ;; Layer II proper, which reuses Layer III's polyphase filterbank
                 (:file "mp2/decode")
                 ;; AAC-LC (MPEG-4 Audio Object Type 2) decode pipeline
                 (:module "aac"
                  :serial t
                  :components ((:file "tables")
                               (:file "structs")
                               (:file "huffman")
                               (:file "filterbank")
                               (:file "tools")
                               (:file "decode")
                               (:file "mp4")))
                 ;; Opus (RFC 6716) — full decoder: CELT + SILK + hybrid,
                 ;; with Ogg (.opus) container demux
                 (:module "opus"
                  :serial t
                  :components ((:file "range")
                               (:file "framing")
                               (:file "tables")
                               (:file "mdct")
                               (:file "celt")
                               (:file "silk-tables")
                               (:file "silk")
                               (:file "decode")
                               (:file "ogg")))
                 ;; Vorbis I — floor 1, all three residue formats, channel coupling,
                 ;; block switching, and Ogg demux
                 (:module "vorbis"
                  :serial t
                  :components ((:file "bits")
                               (:file "codebook")
                               (:file "floor")
                               (:file "residue")
                               (:file "setup")
                               (:file "decode")))
                 ;; G.711 PCMU/PCMA companding
                 (:file "g711")
                 ;; playing a file rather than converting one: incremental
                 ;; decode -> resample -> fixed-size frames on demand
                 (:file "player")))))
