;;;; src/packages.lisp — reed package definition.
;;;;
;;;; reed is a pure-Common-Lisp MPEG-1/2/2.5 Audio Layer III (MP3) decoder.
;;;; No FFI: the whole codec, container to PCM, is Lisp.
(defpackage #:reed
  (:use #:cl)
  (:export
   ;; ---- one-shot decode API ----
   #:decode-mp3 #:decode-mp3-file
   ;; ---- result struct ----
   #:pcm #:pcm-p #:make-pcm
   #:pcm-samples #:pcm-channels #:pcm-sample-rate #:pcm-format #:pcm-frame-count
   ;; ---- streaming / frame API ----
   #:make-decoder #:decoder #:decoder-p
   #:decode-next-frame #:decoder-sample-rate #:decoder-channels
   ;; ---- WAV helper ----
   #:write-wav #:write-wav-file #:pcm->wav-octets
   ;; ---- conditions ----
   #:mp3-error #:mp3-error-message))
