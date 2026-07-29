;;;; src/opus/decode.lisp — Opus packet driver + public API.
;;;;
;;;; A full RFC 6716 decoder: the range coder (range.lisp), TOC/packet framing
;;;; (framing.lisp), the CELT decoder (celt.lisp) and the SILK decoder
;;;; (silk.lisp).  A packet is split into its constituent range-coded frames;
;;;; each frame is decoded by %opus-decode-frame, a port of opus_decode_frame
;;;; (opus_decoder.c): SILK-only, CELT-only and *hybrid* (SILK low band + CELT
;;;; high band on the SAME range decoder, CELT starting at band 17), plus the
;;;; SILK<->CELT redundancy frames used to cross-fade mode switches.  Ogg/.opus
;;;; container demux lives in ogg.lisp.
;;;;
;;;; Public API:
;;;;   (decode-opus octets &key channels)   -> pcm   ; Ogg .opus OR raw packets
;;;;   (decode-opus-file path &key channels) -> pcm  ; sniffs the container
;;;;   (make-opus-decoder ...) + (decode-opus-packet state octets ...) -> pcm
(in-package #:reed)

(defstruct (opus-decoder-state (:constructor %make-opus-decoder-state))
  celt silk (channels 2 :type fixnum) (sample-rate 48000 :type fixnum)
  (last-final-range 0 :type (unsigned-byte 32))
  prev-mode                              ; nil | :silk | :hybrid | :celt
  (prev-redundancy nil))

(defun make-opus-decoder (&key (channels 2))
  "Create a persistent Opus decoder state for CHANNELS output channels."
  (%make-opus-decoder-state :celt (make-celt-decoder channels)
                            :silk (make-silk-decoder) :channels channels))

(defun %silk-internal-fs-khz (bandwidth)
  (ecase bandwidth (:nb 8) (:mb 12) (:wb 16)))

(defun %celt-endband (bandwidth)
  (ecase bandwidth (:nb 13) (:mb 17) (:wb 17) (:swb 19) (:fb 21)))

(defun %frame-size->lm (fs)
  (ecase fs (120 0) (240 1) (480 2) (960 3)))

(defun %smooth-fade (in1 in1o in2 in2o out outo overlap channels win)
  "opus_decoder.c smooth_fade: out = w*in2 + (1-w)*in1 with w = window[i]^2 over
OVERLAP samples (48 kHz, inc = 1).  Arrays may be single- or double-float."
  (declare (type f64vec win))
  (dotimes (c channels)
    (dotimes (i overlap)
      (let* ((w (let ((x (aref win i))) (* x x)))
             (a (float (aref in1 (+ in1o (* channels i) c)) 1d0))
             (b (float (aref in2 (+ in2o (* channels i) c)) 1d0)))
        (setf (aref out (+ outo (* channels i) c))
              (float (+ (* w b) (* (- 1d0 w) a)) 1f0))))))

(defun %opus-decode-frame (st toc data base len)
  "Decode one Opus frame (a single range-coded unit) at DATA[base,base+len) into
an interleaved single-float PCM vector of length CHANNELS*audiosize.  Mirrors
opus_decode_frame (opus_decoder.c, data != NULL path): hybrid SILK+CELT sharing
one range decoder with CELT start-band 17, redundancy cross-fades, and the SILK
(low, resampled to 48 kHz) + CELT (high) mix.  Updates the decoder's
last-final-range to dec.rng ^ redundant_rng (the RFC conformance value)."
  (let* ((mode (opus-toc-mode toc))
         (bandwidth (opus-toc-bandwidth toc))
         (nchan (opus-decoder-state-channels st))
         (stream-ch (if (opus-toc-stereo toc) 2 1))
         (celt (opus-decoder-state-celt st))
         (silk (opus-decoder-state-silk st))
         (f20 960) (f5 240) (f2-5 120)
         (audiosize (opus-toc-frame-size toc))
         (dec (ec-dec-init data :base base :storage len))
         (win +celt-window+)
         (pcm (make-array (* nchan audiosize) :element-type 'single-float :initial-element 0f0))
         (prev-mode (opus-decoder-state-prev-mode st))
         (prev-redundancy (opus-decoder-state-prev-redundancy st))
         (redundancy nil) (celt-to-silk nil) (redundancy-bytes 0)
         (redundant-rng 0) (redundant-audio nil)
         (start-band 0) (clen len) (silk-i16 nil))
    (setf (celt-decoder-stream-channels celt) stream-ch)
    ;; ---- SILK low band ----
    (when (not (eq mode :celt))
      (when (eq prev-mode :celt)                       ; silk_InitDecoder on CELT->SILK/hybrid
        (setf silk (make-silk-decoder) (opus-decoder-state-silk st) silk))
      (let* ((fs-khz (if (eq mode :hybrid) 16 (%silk-internal-fs-khz bandwidth)))
             (payload-ms (max 10 (truncate (* audiosize 1000) 48000)))
             (i16 (make-array (* nchan audiosize) :element-type 'fixnum :initial-element 0))
             (decoded 0) (first t))
        (loop while (< decoded audiosize) do
          (incf decoded (silk-decode silk dec stream-ch nchan fs-khz payload-ms first i16 decoded))
          (setf first nil))
        (setf silk-i16 i16)))
    ;; ---- redundancy detection (consumes bits from the shared decoder) ----
    (when (and (not (eq mode :celt))
               (<= (+ (ec-tell dec) 17 (if (eq mode :hybrid) 20 0)) (* 8 clen)))
      (setf redundancy (if (eq mode :hybrid) (= 1 (ec-dec-bit-logp dec 12)) t))
      (when redundancy
        (setf celt-to-silk (= 1 (ec-dec-bit-logp dec 1))
              redundancy-bytes (if (eq mode :hybrid)
                                   (+ (ec-dec-uint dec 256) 2)
                                   (- clen (ash (+ (ec-tell dec) 7) -3))))
        (decf clen redundancy-bytes)
        (if (< (* clen 8) (ec-tell dec))
            (setf clen 0 redundancy-bytes 0 redundancy nil)
            (decf (ec-dec-storage dec) redundancy-bytes))))
    (when (not (eq mode :celt)) (setf start-band 17))
    (setf (celt-decoder-end celt) (%celt-endband bandwidth))
    ;; ---- 5 ms redundant CELT frame for CELT->SILK (decoded before main CELT) ----
    (when (and redundancy celt-to-silk)
      (setf (celt-decoder-start celt) 0
            redundant-audio (celt-decode-frame celt data (+ base clen) redundancy-bytes 1)
            redundant-rng (celt-decoder-rng celt)))
    ;; ---- CELT high band (main) ----
    (setf (celt-decoder-start celt) start-band)
    (cond
      ((not (eq mode :silk))
       (when (and prev-mode (not (eq mode prev-mode)) (not prev-redundancy))
         (%celt-reset celt))                           ; discard stale CELT state on switch
       (let* ((celt-fs (min f20 audiosize))
              (cout (celt-decode-with-ec celt dec clen (%frame-size->lm celt-fs))))
         (dotimes (i (length cout)) (setf (aref pcm i) (float (aref cout i) 1f0)))))
      (t                                               ; SILK-only frame
       (when (and (eq prev-mode :hybrid)
                  (not (and redundancy celt-to-silk prev-redundancy)))
         ;; hybrid -> SILK: let the CELT MDCT fade the high band out (silence frame)
         (setf (celt-decoder-start celt) 0)
         (let ((cout (celt-decode-frame celt
                                        (make-array 2 :element-type '(unsigned-byte 8)
                                                      :initial-element #xff)
                                        0 2 0)))
           (dotimes (i (length cout)) (setf (aref pcm i) (float (aref cout i) 1f0)))))))
    ;; ---- mix SILK low band on top of CELT ----
    (when silk-i16
      (dotimes (i (* nchan audiosize))
        (incf (aref pcm i) (* (float (aref silk-i16 i) 1f0) #.(/ 1f0 32768f0)))))
    ;; ---- 5 ms redundant CELT frame for SILK->CELT (after the mix) ----
    (when (and redundancy (not celt-to-silk))
      (%celt-reset celt)
      (setf (celt-decoder-start celt) 0
            redundant-audio (celt-decode-frame celt data (+ base clen) redundancy-bytes 1)
            redundant-rng (celt-decoder-rng celt))
      (%smooth-fade pcm (* nchan (- audiosize f2-5))
                    redundant-audio (* nchan f2-5)
                    pcm (* nchan (- audiosize f2-5))
                    f2-5 nchan win))
    ;; ---- CELT->SILK redundancy: copy first 2.5 ms, cross-fade next 2.5 ms ----
    (when (and redundancy celt-to-silk
               (or (not (eq prev-mode :silk)) prev-redundancy))
      (dotimes (c nchan)
        (dotimes (i f2-5)
          (setf (aref pcm (+ (* nchan i) c)) (float (aref redundant-audio (+ (* nchan i) c)) 1f0))))
      (%smooth-fade redundant-audio (* nchan f2-5)
                    pcm (* nchan f2-5)
                    pcm (* nchan f2-5)
                    f2-5 nchan win))
    ;; ---- final range (the RFC conformance value) + carry state ----
    (setf (opus-decoder-state-last-final-range st) (logxor (ec-dec-rng dec) redundant-rng)
          (opus-decoder-state-prev-mode st) mode
          (opus-decoder-state-prev-redundancy st) (and redundancy (not celt-to-silk)))
    pcm))

(defun decode-opus-packet (state data &key (start 0) (end (length data)))
  "Decode one Opus packet (any config: SILK / CELT / hybrid) into a float32 PCM
struct at 48 kHz.  STATE is a persistent opus-decoder-state (SILK/CELT history +
mode carry across packets).  Multi-frame packets (codes 1/2/3) decode each frame
in turn; last-final-range holds the last frame's dec.rng ^ redundant_rng."
  (multiple-value-bind (toc frames) (parse-opus-packet data :start start :end end)
    (let* ((nchan (opus-decoder-state-channels state))
           (audiosize (opus-toc-frame-size toc))
           (total (* audiosize (length frames)))
           (out (make-array (* nchan total) :element-type 'single-float :initial-element 0f0))
           (off 0))
      (dolist (fr frames)
        (let ((pcm (%opus-decode-frame state toc (opus-frame-data fr)
                                       (opus-frame-start fr) (opus-frame-size fr))))
          (replace out pcm :start1 off)
          (incf off (length pcm))))
      (make-pcm :samples out :channels nchan :sample-rate 48000
                :format :float32 :frame-count total))))

(defun decode-opus-celt (packets &key (channels 2))
  "Decode a sequence of raw Opus PACKETS (each an octet vector) into a single
concatenated float32 PCM struct.  Convenience driver over decode-opus-packet
with a fresh persistent state."
  (let* ((state (make-opus-decoder :channels channels))
         (parts (mapcar (lambda (p) (decode-opus-packet state p)) packets))
         (total (reduce #'+ parts :key #'pcm-frame-count))
         (out (make-array (* channels total) :element-type 'single-float :initial-element 0f0))
         (off 0))
    (dolist (p parts)
      (let ((s (pcm-samples p)))
        (replace out s :start1 off)
        (incf off (length s))))
    (make-pcm :samples out :channels channels :sample-rate 48000
              :format :float32 :frame-count total)))

;;; ---- public entry points (auto-detect Ogg vs raw) ----------------------
(defun decode-opus (octets &key channels)
  "Decode an Opus bitstream OCTETS to a 48 kHz float32 PCM struct.  If OCTETS is
an Ogg stream (starts with the \"OggS\" capture pattern) it is demuxed per RFC
7845 (OpusHead pre-skip + output gain applied); otherwise OCTETS is treated as a
single raw Opus packet.  CHANNELS overrides the container channel count."
  (if (and (>= (length octets) 4)
           (= (aref octets 0) #x4f) (= (aref octets 1) #x67)
           (= (aref octets 2) #x67) (= (aref octets 3) #x53)) ; "OggS"
      (decode-opus-ogg octets :channels channels)
      (decode-opus-packet (make-opus-decoder :channels (or channels 2)) octets)))

(defun decode-opus-file (path &key channels)
  "Decode an Opus file (.opus / .ogg Ogg container, or a raw packet dump) at PATH
to a 48 kHz float32 PCM struct."
  (let* ((octets (with-open-file (s path :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                     (read-sequence v s) v))))
    (decode-opus octets :channels channels)))
