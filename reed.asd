;;;; reed.asd — a pure Common Lisp MP3 decoder.
(asdf:defsystem "reed"
  :description "A from-scratch MP3 decoder in pure Common Lisp: parse the MPEG
audio container (ID3v1/v2 skip, frame sync, header, Xing/Info/VBRI tags) and
decode MPEG-1/2/2.5 Layer III to PCM — bit reservoir, Huffman decode (all 32
tables), requantization, MS/intensity stereo, alias reduction, the 18-point
IMDCT with all window types, and the 32-band polyphase synthesis filterbank —
emitting 16-bit or float32 interleaved samples.  No FFI, no libmpg123: the
whole codec is bytes to PCM in Lisp.  Verified against ffmpeg (libmp3lame).
MP3 patents expired in 2017; this is an unencumbered clean-room implementation."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "bitreader")
                             (:file "tables")
                             (:file "huffman")
                             (:file "header")
                             (:file "sideinfo")
                             (:file "requantize")
                             (:file "stereo")
                             (:file "imdct")
                             (:file "synthesis")
                             (:file "layer3")
                             (:file "decode")))))
