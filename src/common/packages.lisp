;;;; src/common/packages.lisp — reed package definition.
;;;;
;;;; reed is a pure-Common-Lisp audio-codec library: a multi-codec umbrella (the
;;;; audio analog of pigment for images) hosting an MPEG-1/2/2.5 Layer III (MP3)
;;;; decoder and the ITU-T G.711 companders today, with AAC-LC and Opus modules
;;;; planned.  No FFI: every codec, container to PCM, is Lisp.  The package stays
;;;; flat (#:reed) across all codecs — modularity is in the file layout, not in
;;;; sub-packages.
(defpackage #:reed
  (:use #:cl)
  (:export
   ;; ==== shared PCM representation (every codec's common currency) ====
   #:pcm #:pcm-p #:make-pcm
   #:pcm-samples #:pcm-channels #:pcm-sample-rate #:pcm-format #:pcm-frame-count
   ;; ---- WAV helper ----
   #:write-wav #:write-wav-file #:pcm->wav-octets

   ;; ==== DSP: what sits between a decoder and a device ====
   ;; sample plumbing
   #:pcm16-vector #:make-pcm16 #:clamp16
   ;; downmix / gain / the mixer
   #:downmix #:apply-gain #:mix
   ;; rate conversion (stateful, so a stream converted in pieces has no seams)
   #:resampler #:resampler-p #:make-resampler #:resample #:resample-pcm
   #:resampler-in-rate #:resampler-out-rate

   ;; ==== playing a file: fixed-size frames on demand ====
   #:player #:player-p #:make-mp3-player #:make-mp3-source #:make-buffer-source
   #:player-next-frame #:player-finished-p #:player-frames
   #:player-source-rate #:player-source-channels #:player-gain #:drain-source

   ;; ==== MP3 (MPEG-1/2/2.5 Layer III) ====
   ;; one-shot decode -> pcm struct
   #:decode-mp3 #:decode-mp3-file
   ;; streaming / frame API
   #:make-decoder #:decoder #:decoder-p
   #:decode-next-frame #:decoder-sample-rate #:decoder-channels
   ;; conditions
   #:mp3-error #:mp3-error-message

   ;; ==== AAC-LC (MPEG-4 Audio Object Type 2) ====
   #:decode-aac #:decode-aac-file
   ;; streaming AAC, for a container that hands over one access unit at a time
   #:make-aac-decoder #:decode-aac-packet
   #:aac-error #:aac-error-message

   ;; ==== Opus (RFC 6716) — full decoder: CELT + SILK + hybrid + Ogg ====
   #:make-opus-decoder #:opus-decoder-state #:opus-decoder-state-p
   #:decode-opus-packet #:decode-opus-celt #:decode-opus #:decode-opus-file
   #:decode-opus-ogg #:opus-head #:opus-head-pre-skip #:opus-head-channels
   #:opus-head-output-gain #:opus-head-mapping-family #:*opus-rfc8251*
   #:parse-opus-packet #:parse-toc #:opus-toc #:opus-toc-p
   #:opus-toc-config #:opus-toc-mode #:opus-toc-bandwidth
   #:opus-toc-frame-size #:opus-toc-channels #:opus-toc-stereo #:opus-toc-code
   #:opus-frame #:opus-frame-data #:opus-frame-start #:opus-frame-size
   #:opus-error #:opus-error-message
   ;; range coder (exposed for Stage 2/testing)
   #:ec-dec-init #:ec-decode #:ec-dec-update #:ec-dec-bit-logp
   #:ec-dec-icdf #:ec-dec-bits #:ec-dec-uint #:ec-tell #:ec-tell-frac

   ;; ==== G.711 (ITU-T PCMU / PCMA companding) ====
   ;; buffer transforms: (signed-byte 16) PCM <-> (unsigned-byte 8) codewords
   #:pcmu-encode #:pcmu-decode #:pcma-encode #:pcma-decode
   #:mulaw-encode #:mulaw-decode #:alaw-encode #:alaw-decode
   ;; per-sample companders
   #:pcmu-encode-1 #:pcmu-decode-1 #:pcma-encode-1 #:pcma-decode-1))
