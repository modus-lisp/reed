;;;; reed.asd — a pure Common Lisp audio-codec library.
(asdf:defsystem "reed"
  :description "A multi-codec audio library in pure Common Lisp — the audio
analog of pigment for images: one flat package (#:reed) hosting several codecs,
container to PCM, with no FFI.  Today it ships an MPEG-1/2/2.5 Layer III (MP3)
decoder — MPEG container parse (ID3v1/v2 skip, frame sync, Xing/Info/VBRI tags),
bit reservoir, Huffman decode (all 32 tables), requantization, MS/intensity
stereo, alias reduction, the 18-point IMDCT with all window types, and the
32-band polyphase synthesis filterbank, emitting 16-bit or float32 interleaved
PCM (verified bit-accurate against ffmpeg/minimp3) — and the ITU-T G.711 PCMU
(mu-law) / PCMA (A-law) companders.  AAC-LC and Opus modules are planned.  MP3
patents expired in 2017; this is an unencumbered clean-room implementation."
  :version "0.2.0"
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
                               (:file "pcm")))
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
                 ;; G.711 PCMU/PCMA companding
                 (:file "g711")))))
