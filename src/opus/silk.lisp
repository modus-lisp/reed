;;;; src/opus/silk.lisp — the SILK decoder (RFC 6716 §4.2), Stage 2.
;;;;
;;;; SILK is the speech/LP path of Opus.  This is a bit-exact port of libopus's
;;;; fixed-point SILK *decoder* (silk/decode_*.c, NSQ-inverse, NLSF/LTP, the
;;;; shell excitation coder and the polyphase resampler) built on reed's shared
;;;; range decoder (range.lisp).  SILK decodes at an internal rate (NB=8, MB=12,
;;;; WB=16 kHz) and the resampler brings it to Opus's 48 kHz output.  Configs
;;;; 0–11 (SILK-only, NB/MB/WB, mono+stereo, 10/20/40/60 ms) route here.
;;;;
;;;; Deferred to Stage 3 (harmless for clean-channel decode): LBRR/FEC frames
;;;; (consumed/skipped here), packet-loss concealment (PLC) and comfort noise
;;;; (CNG) — both only alter output when a packet is actually lost, which never
;;;; happens on the conformance path (lostFlag = FLAG_DECODE_NORMAL).
(in-package #:reed)

;;; ======================================================================
;;;  Fixed-point primitives (libopus silk/macros.h + SigProc_FIX.h).  All
;;;  register math is 32-bit two's-complement; CL integers are unbounded so we
;;;  wrap explicitly only where libopus relies on int32 overflow (the *_ovflw
;;;  ops and left shifts), and use arithmetic (floor) shift for >> and truncate
;;;  for /.
;;; ======================================================================
(declaim (inline w32 s16 sh-r sh-l rror smulwb smlawb smulww smlaww smulbb
                 smlabb sat16 clz32))

(defun w32 (x)
  "Reduce X to a signed 32-bit two's-complement value."
  (let ((m (logand x #xFFFFFFFF)))
    (if (>= m #x80000000) (- m #x100000000) m)))

(defun s16 (x)
  "Sign-extend the low 16 bits of X (C cast to opus_int16)."
  (let ((m (logand x #xFFFF)))
    (if (>= m #x8000) (- m #x10000) m)))

(defun sh-r (a n) "Arithmetic right shift (silk_RSHIFT)." (ash a (- n)))
(defun sh-l (a n) "Left shift (silk_LSHIFT, no wrap in normal use)." (ash a n))

(defun rror (a n)
  "silk_RSHIFT_ROUND(a,shift)."
  (if (= n 1)
      (+ (ash a -1) (logand a 1))
      (ash (+ (ash a (- (1- n))) 1) -1)))

(defun smulwb (a b) (ash (* a (s16 b)) -16))
(defun smlawb (a b c) (+ a (ash (* b (s16 c)) -16)))
(defun smulww (a b) (ash (* a b) -16))
(defun smlaww (a b c) (+ a (ash (* b c) -16)))
(defun smulbb (a b) (* (s16 a) (s16 b)))
(defun smlabb (a b c) (+ a (* (s16 b) (s16 c))))

(defun sat16 (a) (cond ((> a 32767) 32767) ((< a -32768) -32768) (t a)))
(defun sat32 (a) (cond ((> a 2147483647) 2147483647)
                       ((< a -2147483648) -2147483648) (t a)))

(defun add-sat32 (a b) (sat32 (+ a b)))

(defun lshift-sat32 (a shift)
  (let ((lo (ash -2147483648 (- shift)))
        (hi (ash 2147483647 (- shift))))
    (ash (cond ((> a hi) hi) ((< a lo) lo) (t a)) shift)))

(defun clz32 (x)
  "Count leading zeros of X as a 32-bit unsigned."
  (- 32 (integer-length (logand x #xFFFFFFFF))))

(defun ilimit (a l1 l2)
  "silk_LIMIT_int(a,l1,l2)."
  (if (> l1 l2)
      (cond ((> a l1) l1) ((< a l2) l2) (t a))
      (cond ((> a l2) l2) ((< a l1) l1) (t a))))

(defun silk-rand (seed) (w32 (+ 907633515 (w32 (* seed 196314165)))))

(defun inverse32-varq (b32 qres)
  "silk_INVERSE32_varQ."
  (let* ((b-headrm (- (clz32 (abs b32)) 1))
         (b32-nrm (ash b32 b-headrm))
         (b32-inv (truncate (ash 2147483647 -2) (ash b32-nrm -16)))
         (result (ash b32-inv 16))
         (err-q32 (ash (- (ash 1 29) (smulwb b32-nrm b32-inv)) 3))
         (lshift (- 61 b-headrm qres)))
    (setf result (smlaww result err-q32 b32-inv))
    (if (<= lshift 0)
        (lshift-sat32 result (- lshift))
        (if (< lshift 32) (ash result (- lshift)) 0))))

(defun smmul (a b) (ash (* a b) -32))

(defun div32-varq (a32 b32 qres)
  "silk_DIV32_varQ."
  (let* ((a-headrm (- (clz32 (abs a32)) 1))
         (a32-nrm (ash a32 a-headrm))
         (b-headrm (- (clz32 (abs b32)) 1))
         (b32-nrm (ash b32 b-headrm))
         (b32-inv (truncate (ash 2147483647 -2) (ash b32-nrm -16)))
         (result (smulwb a32-nrm b32-inv)))
    (setf a32-nrm (w32 (- a32-nrm (w32 (ash (smmul b32-nrm result) 3)))))
    (setf result (smlawb result a32-nrm b32-inv))
    (let ((lshift (- (+ 29 a-headrm) b-headrm qres)))
      (if (< lshift 0)
          (lshift-sat32 result (- lshift))
          (if (< lshift 32) (ash result (- lshift)) 0)))))

;;; ======================================================================
;;;  Constants (silk/define.h)
;;; ======================================================================
(defconstant +silk-max-frame-length+ 320)
(defconstant +silk-max-nb-subfr+ 4)
(defconstant +silk-ltp-order+ 5)
(defconstant +silk-max-lpc-order+ 16)
(defconstant +silk-shell-fl+ 16)          ; SHELL_CODEC_FRAME_LENGTH
(defconstant +silk-max-pulses+ 16)
(defconstant +silk-n-rate-levels+ 10)
(defconstant +silk-nlsf-qmax+ 4)          ; NLSF_QUANT_MAX_AMPLITUDE
(defconstant +silk-quant-level-adjust-q10+ 80)
(defconstant +type-no-voice+ 0)
(defconstant +type-unvoiced+ 1)
(defconstant +type-voiced+ 2)
(defconstant +code-independently+ 0)
(defconstant +code-independently-no-ltp+ 1)
(defconstant +code-conditionally+ 2)
(defconstant +stereo-interp-len-ms+ 8)
(defconstant +offset-uvl+ 100) (defconstant +offset-uvh+ 240)
(defconstant +offset-vl+ 32) (defconstant +offset-vh+ 100)

;;; ======================================================================
;;;  Decoder state
;;; ======================================================================
(defstruct silk-resampler
  (siir (make-array 6 :element-type 'fixnum :initial-element 0))
  (sfir (make-array 8 :element-type 'fixnum :initial-element 0))
  (delaybuf (make-array 48 :element-type 'fixnum :initial-element 0))
  (input-delay 0 :type fixnum)
  (inv-ratio-q16 0 :type fixnum)
  (fs-in-khz 0 :type fixnum)
  (fs-out-khz 48 :type fixnum)
  (batch 0 :type fixnum))

(defstruct silk-channel
  (fs-khz 0 :type fixnum)
  (nb-subfr 0 :type fixnum)
  (subfr-length 0 :type fixnum)
  (frame-length 0 :type fixnum)
  (ltp-mem-length 0 :type fixnum)
  (lpc-order 0 :type fixnum)
  (nlsf-cb :nbmb)
  (prev-gain-q16 65536 :type fixnum)
  (exc-q14 (make-array +silk-max-frame-length+ :element-type 'fixnum :initial-element 0))
  (slpc-buf (make-array +silk-max-lpc-order+ :element-type 'fixnum :initial-element 0))
  (outbuf (make-array 480 :element-type 'fixnum :initial-element 0))
  (prevnlsf (make-array +silk-max-lpc-order+ :element-type 'fixnum :initial-element 0))
  (lastgainindex 10 :type fixnum)
  (lagprev 100 :type fixnum)
  (prev-signal-type +type-no-voice+ :type fixnum)
  (first-frame-after-reset 1 :type fixnum)
  (ec-prev-signal-type 0 :type fixnum)
  (ec-prev-lag-index 0 :type fixnum)
  (nframes-decoded 0 :type fixnum)
  (nframes-per-packet 1 :type fixnum)
  (vad-flags (make-array 3 :element-type 'fixnum :initial-element 0))
  (lbrr-flag 0 :type fixnum)
  (lbrr-flags (make-array 3 :element-type 'fixnum :initial-element 0))
  (resampler (make-silk-resampler))
  (pitch-contour-icdf nil)
  (pitch-lag-low-icdf nil)
  ;; indices (transient per frame)
  (signal-type 0 :type fixnum)
  (quant-offset-type 0 :type fixnum)
  (gains-idx (make-array 4 :element-type 'fixnum :initial-element 0))
  (nlsf-idx (make-array 17 :element-type 'fixnum :initial-element 0))
  (nlsf-interp-q2 4 :type fixnum)
  (lag-index 0 :type fixnum)
  (contour-index 0 :type fixnum)
  (per-index 0 :type fixnum)
  (ltp-index (make-array 4 :element-type 'fixnum :initial-element 0))
  (ltp-scale-index 0 :type fixnum)
  (seed 0 :type fixnum)
  ;; control (transient)
  (gains-q16 (make-array 4 :element-type 'fixnum :initial-element 0))
  (pred-coef (make-array '(2 16) :element-type 'fixnum :initial-element 0))
  (ltp-coef (make-array 20 :element-type 'fixnum :initial-element 0))
  (pitchl (make-array 4 :element-type 'fixnum :initial-element 0))
  (ltp-scale-q14 0 :type fixnum))

(defstruct silk-stereo
  (pred-prev (make-array 2 :element-type 'fixnum :initial-element 0))
  (smid (make-array 2 :element-type 'fixnum :initial-element 0))
  (sside (make-array 2 :element-type 'fixnum :initial-element 0)))

(defstruct silk-decoder
  (channels (vector (make-silk-channel) (make-silk-channel)))
  (stereo (make-silk-stereo))
  (prev-decode-only-middle 0 :type fixnum)
  (n-internal 1 :type fixnum)
  (n-api 1 :type fixnum))

;;; ======================================================================
;;;  Resampler (silk/resampler*.c) — SILK internal rate → 48 kHz.
;;; ======================================================================
(defun copy-resampler-state (src dst)
  "Deep-copy resampler state SRC into DST (silk_memcpy of the struct)."
  (replace (silk-resampler-siir dst) (silk-resampler-siir src))
  (replace (silk-resampler-sfir dst) (silk-resampler-sfir src))
  (replace (silk-resampler-delaybuf dst) (silk-resampler-delaybuf src))
  (setf (silk-resampler-input-delay dst) (silk-resampler-input-delay src)
        (silk-resampler-inv-ratio-q16 dst) (silk-resampler-inv-ratio-q16 src)
        (silk-resampler-fs-in-khz dst) (silk-resampler-fs-in-khz src)
        (silk-resampler-fs-out-khz dst) (silk-resampler-fs-out-khz src)
        (silk-resampler-batch dst) (silk-resampler-batch src)))

(defun silk-resampler-init (rs fs-in-hz fs-out-hz)
  "Initialise RS for the decode path (up-sampling to 48 kHz via the IIR+FIR
polyphase resampler; up2x always 1 for the 3:1/4:1/6:1 SILK ratios)."
  (let ((up2x 1))
    (setf (silk-resampler-fs-in-khz rs) (truncate fs-in-hz 1000)
          (silk-resampler-fs-out-khz rs) (truncate fs-out-hz 1000)
          (silk-resampler-batch rs) (* (truncate fs-in-hz 1000) 10)
          ;; delay_matrix_dec column for 48 kHz out
          (silk-resampler-input-delay rs)
          (ecase fs-in-hz (8000 0) (12000 4) (16000 7)))
    (let ((inv (ash (truncate (ash fs-in-hz (+ 14 up2x)) fs-out-hz) 2)))
      (loop while (< (smulww inv fs-out-hz) (ash fs-in-hz up2x)) do (incf inv))
      (setf (silk-resampler-inv-ratio-q16 rs) inv))))

(defun silk-resampler-up2-hq (siir out out-off in in-off len)
  "silk_resampler_private_up2_HQ — 3rd-order allpass 2× upsampler (Q10 state)."
  (declare (type (simple-array fixnum (*)) siir out in))
  (let ((c00 (aref +silk-resampler-up2-hq-0+ 0)) (c01 (aref +silk-resampler-up2-hq-0+ 1))
        (c02 (aref +silk-resampler-up2-hq-0+ 2)) (c10 (aref +silk-resampler-up2-hq-1+ 0))
        (c11 (aref +silk-resampler-up2-hq-1+ 1)) (c12 (aref +silk-resampler-up2-hq-1+ 2)))
    (dotimes (k len)
      (let* ((in32 (ash (aref in (+ in-off k)) 10)) (yy 0) (xx 0) (o1 0) (o2 0))
        ;; even output
        (setf yy (- in32 (aref siir 0)) xx (smulwb yy c00) o1 (+ (aref siir 0) xx)
              (aref siir 0) (+ in32 xx))
        (setf yy (- o1 (aref siir 1)) xx (smulwb yy c01) o2 (+ (aref siir 1) xx)
              (aref siir 1) (+ o1 xx))
        (setf yy (- o2 (aref siir 2)) xx (smlawb yy yy c02) o1 (+ (aref siir 2) xx)
              (aref siir 2) (+ o2 xx))
        (setf (aref out (+ out-off (* 2 k))) (sat16 (rror o1 10)))
        ;; odd output
        (setf yy (- in32 (aref siir 3)) xx (smulwb yy c10) o1 (+ (aref siir 3) xx)
              (aref siir 3) (+ in32 xx))
        (setf yy (- o1 (aref siir 4)) xx (smulwb yy c11) o2 (+ (aref siir 4) xx)
              (aref siir 4) (+ o1 xx))
        (setf yy (- o2 (aref siir 5)) xx (smlawb yy yy c12) o1 (+ (aref siir 5) xx)
              (aref siir 5) (+ o2 xx))
        (setf (aref out (+ out-off (* 2 k) 1)) (sat16 (rror o1 10)))))))

(defun silk-resampler-iir-fir (rs out out-off in in-off in-len)
  "silk_resampler_private_IIR_FIR."
  (declare (type (simple-array fixnum (*)) out in))
  (let* ((batch (silk-resampler-batch rs))
         (buf (make-array (+ (* 2 batch) 8) :element-type 'fixnum :initial-element 0))
         (fir (silk-resampler-sfir rs))
         (incr (silk-resampler-inv-ratio-q16 rs))
         (nin 0))
    (dotimes (i 8) (setf (aref buf i) (aref fir i)))
    (loop
      (setf nin (min in-len batch))
      (silk-resampler-up2-hq (silk-resampler-siir rs) buf 8 in in-off nin)
      (let ((max-index (ash nin 17)) (index 0))
        (loop while (< index max-index) do
          (let* ((tbl (smulwb (logand index #xFFFF) 12))
                 (bp (ash index -16))
                 (res (smulbb (aref buf bp) (aref +silk-resampler-frac-fir-12+ (+ (* tbl 4) 0)))))
            (setf res (smlabb res (aref buf (+ bp 1)) (aref +silk-resampler-frac-fir-12+ (+ (* tbl 4) 1))))
            (setf res (smlabb res (aref buf (+ bp 2)) (aref +silk-resampler-frac-fir-12+ (+ (* tbl 4) 2))))
            (setf res (smlabb res (aref buf (+ bp 3)) (aref +silk-resampler-frac-fir-12+ (+ (* tbl 4) 3))))
            (setf res (smlabb res (aref buf (+ bp 4)) (aref +silk-resampler-frac-fir-12+ (+ (* (- 11 tbl) 4) 3))))
            (setf res (smlabb res (aref buf (+ bp 5)) (aref +silk-resampler-frac-fir-12+ (+ (* (- 11 tbl) 4) 2))))
            (setf res (smlabb res (aref buf (+ bp 6)) (aref +silk-resampler-frac-fir-12+ (+ (* (- 11 tbl) 4) 1))))
            (setf res (smlabb res (aref buf (+ bp 7)) (aref +silk-resampler-frac-fir-12+ (+ (* (- 11 tbl) 4) 0))))
            (setf (aref out out-off) (sat16 (rror res 15)))
            (incf out-off)
            (incf index incr))))
      (incf in-off nin) (decf in-len nin)
      (if (> in-len 0)
          (dotimes (i 8) (setf (aref buf i) (aref buf (+ (* nin 2) i))))
          (return)))
    (dotimes (i 8) (setf (aref fir i) (aref buf (+ (* nin 2) i))))
    out-off))

(defun silk-resampler (rs out out-off in in-off in-len)
  "silk_resampler top level: prepend the 1 ms delay buffer, run IIR+FIR in two
halves, then stash the tail for the next call.  Output count = in-len*48/fs_in."
  (declare (type (simple-array fixnum (*)) out in))
  (let* ((fsin (silk-resampler-fs-in-khz rs))
         (fsout (silk-resampler-fs-out-khz rs))
         (delay (silk-resampler-input-delay rs))
         (db (silk-resampler-delaybuf rs))
         (n (- fsin delay)))
    (dotimes (i n) (setf (aref db (+ delay i)) (aref in (+ in-off i))))
    (silk-resampler-iir-fir rs out out-off db 0 fsin)
    (silk-resampler-iir-fir rs out (+ out-off fsout) in (+ in-off n) (- in-len fsin))
    (dotimes (i delay) (setf (aref db i) (aref in (+ in-off (- in-len delay) i))))))

;;; ======================================================================
;;;  Shell / pulse excitation (silk/shell_coder.c, decode_pulses.c)
;;; ======================================================================
(defun silk-shell-decode-split (out i1 i2 d p tbl)
  (if (> p 0)
      (let ((c1 (ec-dec-icdf d (%off tbl (aref +silk-shell-code-table-offsets+ p)) 8)))
        (setf (aref out i1) c1 (aref out i2) (- p c1)))
      (setf (aref out i1) 0 (aref out i2) 0)))

(declaim (inline %off))
(defun %off (vec off)
  "Return a displaced view of VEC starting at OFF (icdf tables read from an
offset within a flat table)."
  (make-array (- (length vec) off) :element-type (array-element-type vec)
              :displaced-to vec :displaced-index-offset off))

(defun silk-shell-decoder (pulses base d nbpulses)
  "silk_shell_decoder: recursively split NBPULSES over a 16-sample block."
  (let ((p3 (make-array 2)) (p2 (make-array 4)) (p1 (make-array 8)))
    (silk-shell-decode-split p3 0 1 d nbpulses +silk-shell-code-table3+)
    (silk-shell-decode-split p2 0 1 d (aref p3 0) +silk-shell-code-table2+)
    (silk-shell-decode-split p1 0 1 d (aref p2 0) +silk-shell-code-table1+)
    (silk-shell-decode-split pulses (+ base 0) (+ base 1) d (aref p1 0) +silk-shell-code-table0+)
    (silk-shell-decode-split pulses (+ base 2) (+ base 3) d (aref p1 1) +silk-shell-code-table0+)
    (silk-shell-decode-split p1 2 3 d (aref p2 1) +silk-shell-code-table1+)
    (silk-shell-decode-split pulses (+ base 4) (+ base 5) d (aref p1 2) +silk-shell-code-table0+)
    (silk-shell-decode-split pulses (+ base 6) (+ base 7) d (aref p1 3) +silk-shell-code-table0+)
    (silk-shell-decode-split p2 2 3 d (aref p3 1) +silk-shell-code-table2+)
    (silk-shell-decode-split p1 4 5 d (aref p2 2) +silk-shell-code-table1+)
    (silk-shell-decode-split pulses (+ base 8) (+ base 9) d (aref p1 4) +silk-shell-code-table0+)
    (silk-shell-decode-split pulses (+ base 10) (+ base 11) d (aref p1 5) +silk-shell-code-table0+)
    (silk-shell-decode-split p1 6 7 d (aref p2 3) +silk-shell-code-table1+)
    (silk-shell-decode-split pulses (+ base 12) (+ base 13) d (aref p1 6) +silk-shell-code-table0+)
    (silk-shell-decode-split pulses (+ base 14) (+ base 15) d (aref p1 7) +silk-shell-code-table0+)))

(defun silk-decode-signs (d pulses length signal-type quant-offset-type sum-pulses)
  "silk_decode_signs."
  (let* ((icdf (make-array 2 :initial-element 0))
         (base (* 7 (+ quant-offset-type (ash signal-type 1))))
         (nblk (sh-r (+ length (ash +silk-shell-fl+ -1)) 4)))
    (dotimes (i nblk)
      (let ((p (aref sum-pulses i)))
        (when (> p 0)
          (setf (aref icdf 0) (aref +silk-sign-icdf+ (+ base (min (logand p #x1f) 6))))
          (dotimes (j +silk-shell-fl+)
            (let ((qi (+ (* i +silk-shell-fl+) j)))
              (when (> (aref pulses qi) 0)
                (setf (aref pulses qi)
                      (* (aref pulses qi) (- (ash (ec-dec-icdf d icdf 8) 1) 1)))))))))))

(defun silk-decode-pulses (d pulses signal-type quant-offset-type frame-length)
  "silk_decode_pulses: rate level → per-block pulse counts (with LSB extension)
→ shell-decode locations → LSBs → signs."
  (dotimes (i (length pulses)) (setf (aref pulses i) 0))
  (let* ((rate-level (ec-dec-icdf d (%off +silk-rate-levels-icdf+ (* (ash signal-type -1) 9)) 8))
         (iter (sh-r frame-length 4))
         (sum-pulses (make-array 20 :initial-element 0))
         (nlshifts (make-array 20 :initial-element 0)))
    (when (< (* iter +silk-shell-fl+) frame-length) (incf iter)) ; 10ms@12kHz
    (dotimes (i iter)
      (setf (aref nlshifts i) 0
            (aref sum-pulses i) (ec-dec-icdf d (%off +silk-pulses-per-block-icdf+ (* rate-level 18)) 8))
      (loop while (= (aref sum-pulses i) (1+ +silk-max-pulses+)) do
        (incf (aref nlshifts i))
        (setf (aref sum-pulses i)
              (ec-dec-icdf d (%off +silk-pulses-per-block-icdf+
                                   (+ (* (1- +silk-n-rate-levels+) 18)
                                      (if (= (aref nlshifts i) 10) 1 0)))
                           8))))
    (dotimes (i iter)
      (if (> (aref sum-pulses i) 0)
          (silk-shell-decoder pulses (* i +silk-shell-fl+) d (aref sum-pulses i))
          (dotimes (k +silk-shell-fl+) (setf (aref pulses (+ (* i +silk-shell-fl+) k)) 0))))
    (dotimes (i iter)
      (when (> (aref nlshifts i) 0)
        (let ((nls (aref nlshifts i)) (base (* i +silk-shell-fl+)))
          (dotimes (k +silk-shell-fl+)
            (let ((absq (aref pulses (+ base k))))
              (dotimes (j nls)
                (setf absq (+ (ash absq 1) (ec-dec-icdf d +silk-lsb-icdf+ 8))))
              (setf (aref pulses (+ base k)) absq)))
          (setf (aref sum-pulses i) (logior (aref sum-pulses i) (ash nls 5))))))
    (silk-decode-signs d pulses frame-length signal-type quant-offset-type sum-pulses)))

;;; ======================================================================
;;;  NLSF decode (silk/NLSF_decode.c, NLSF_unpack.c, NLSF_stabilize.c, NLSF2A.c)
;;; ======================================================================
(defun %nlsf-cb (ch)
  "Return the NLSF codebook arrays for CH's bandwidth as multiple values."
  (if (eq (silk-channel-nlsf-cb ch) :nbmb)
      (values +silk-nlsf-nbmb-order+ +silk-nlsf-nbmb-nvec+ +silk-nlsf-nbmb-qstep+
              +silk-nlsf-nbmb-cb1-q8+ +silk-nlsf-nbmb-cb1-wght-q9+ +silk-nlsf-nbmb-cb1-icdf+
              +silk-nlsf-nbmb-pred-q8+ +silk-nlsf-nbmb-ec-sel+ +silk-nlsf-nbmb-ec-icdf+
              +silk-nlsf-nbmb-delta-min-q15+)
      (values +silk-nlsf-wb-order+ +silk-nlsf-wb-nvec+ +silk-nlsf-wb-qstep+
              +silk-nlsf-wb-cb1-q8+ +silk-nlsf-wb-cb1-wght-q9+ +silk-nlsf-wb-cb1-icdf+
              +silk-nlsf-wb-pred-q8+ +silk-nlsf-wb-ec-sel+ +silk-nlsf-wb-ec-icdf+
              +silk-nlsf-wb-delta-min-q15+)))

(defun silk-nlsf-unpack (ch cb1-index)
  "silk_NLSF_unpack → (values ec-ix pred-q8)."
  (multiple-value-bind (order nvec qstep cb1 wght icdf pred ecsel ecicdf dmin) (%nlsf-cb ch)
    (declare (ignore nvec qstep cb1 wght icdf ecicdf dmin))
    (let ((ec-ix (make-array order)) (pred-q8 (make-array order))
          (sel-off (* cb1-index (truncate order 2))))
      (loop for i from 0 below order by 2
            for entry = (aref ecsel (+ sel-off (truncate i 2))) do
        (setf (aref ec-ix i) (* (logand (sh-r entry 1) 7) (1+ (* 2 +silk-nlsf-qmax+)))
              (aref pred-q8 i) (aref pred (+ i (* (logand entry 1) (1- order))))
              (aref ec-ix (1+ i)) (* (logand (sh-r entry 5) 7) (1+ (* 2 +silk-nlsf-qmax+)))
              (aref pred-q8 (1+ i)) (aref pred (+ i (* (logand (sh-r entry 4) 1) (1- order)) 1))))
      (values ec-ix pred-q8))))

(defun silk-nlsf-stabilize (nlsf dmin order)
  "silk_NLSF_stabilize: enforce minimum spacing so NLSF→LPC is stable."
  (dotimes (loops 20)
    (let ((min-diff (- (aref nlsf 0) (aref dmin 0))) (idx 0))
      (loop for i from 1 to (1- order)
            for diff = (- (aref nlsf i) (+ (aref nlsf (1- i)) (aref dmin i))) do
        (when (< diff min-diff) (setf min-diff diff idx i)))
      (let ((diff (- (ash 1 15) (+ (aref nlsf (1- order)) (aref dmin order)))))
        (when (< diff min-diff) (setf min-diff diff idx order)))
      (when (>= min-diff 0) (return-from silk-nlsf-stabilize))
      (cond
        ((= idx 0) (setf (aref nlsf 0) (aref dmin 0)))
        ((= idx order) (setf (aref nlsf (1- order)) (- (ash 1 15) (aref dmin order))))
        (t (let ((min-c 0) (max-c (ash 1 15)))
             (dotimes (k idx) (incf min-c (aref dmin k)))
             (incf min-c (sh-r (aref dmin idx) 1))
             (loop for k from order above idx do (decf max-c (aref dmin k)))
             (decf max-c (sh-r (aref dmin idx) 1))
             (let ((cf (ilimit (rror (+ (aref nlsf (1- idx)) (aref nlsf idx)) 1) min-c max-c)))
               (setf (aref nlsf (1- idx)) (- cf (sh-r (aref dmin idx) 1))
                     (aref nlsf idx) (+ (aref nlsf (1- idx)) (aref dmin idx)))))))))
  ;; fallback: insertion sort + clamp
  (loop for i from 1 below order for v = (aref nlsf i) do
    (let ((j (1- i)))
      (loop while (and (>= j 0) (< v (aref nlsf j))) do
        (setf (aref nlsf (1+ j)) (aref nlsf j)) (decf j))
      (setf (aref nlsf (1+ j)) v)))
  (setf (aref nlsf 0) (max (aref nlsf 0) (aref dmin 0)))
  (loop for i from 1 below order do
    (setf (aref nlsf i) (max (aref nlsf i) (sat16 (+ (aref nlsf (1- i)) (aref dmin i))))))
  (setf (aref nlsf (1- order)) (min (aref nlsf (1- order)) (- (ash 1 15) (aref dmin order))))
  (loop for i from (- order 2) downto 0 do
    (setf (aref nlsf i) (min (aref nlsf i) (- (aref nlsf (1+ i)) (aref dmin (1+ i)))))))

(defun silk-nlsf-residual-dequant (indices pred-q8 qstep order)
  "silk_NLSF_residual_dequant → x_Q10[order] via backward prediction."
  (let ((x (make-array order :initial-element 0)) (out 0))
    (loop for i from (1- order) downto 0 do
      (let ((pred (sh-r (smulbb out (aref pred-q8 i)) 8)))
        (setf out (ash (aref indices (1+ i)) 10))
        (cond ((> out 0) (decf out 102))  ; SILK_FIX_CONST(0.1,10)=102
              ((< out 0) (incf out 102)))
        (setf out (smlawb pred out qstep) (aref x i) out)))
    x))

(defun silk-nlsf-decode (ch)
  "silk_NLSF_decode: stage-1 VQ + residual → stabilized NLSF Q15 vector."
  (multiple-value-bind (order nvec qstep cb1 wght icdf pred ecsel ecicdf dmin) (%nlsf-cb ch)
    (declare (ignore nvec icdf pred ecsel))
    (let* ((idxs (silk-channel-nlsf-idx ch))
           (nlsf (make-array order :initial-element 0)))
      (multiple-value-bind (ec-ix pred-q8) (silk-nlsf-unpack ch (aref idxs 0))
        (declare (ignore ec-ix ecicdf))
        (let ((res (silk-nlsf-residual-dequant idxs pred-q8 qstep order))
              (cb-off (* (aref idxs 0) order)))
          (dotimes (i order)
            (let ((v (+ (truncate (ash (aref res i) 14) (aref wght (+ cb-off i)))
                        (ash (aref cb1 (+ cb-off i)) 7))))
              (setf (aref nlsf i) (ilimit v 0 32767))))
          (silk-nlsf-stabilize nlsf dmin order)
          nlsf)))))

(defun silk-lpc-fit (a-q12 a32 qout qin d)
  "silk_LPC_fit: clamp int32 coefs to Q12 int16, applying chirp if needed.  Only
the fall-through case (all 10 iterations exhausted) clips and rewrites a32."
  (let ((idx 0) (broke nil))
    (dotimes (it 10)
      (let ((maxabs 0))
        (dotimes (k d) (let ((av (abs (aref a32 k)))) (when (> av maxabs) (setf maxabs av idx k))))
        (setf maxabs (rror maxabs (- qin qout)))
        (if (> maxabs 32767)
            (let* ((mx (min maxabs 163838))
                   (chirp (- 65470 (truncate (ash (- mx 32767) 14)
                                             (ash (* mx (1+ idx)) -2)))))
              (silk-bwexpander-32 a32 d chirp))
            (progn (setf broke t) (return)))))
    (if broke
        (dotimes (k d) (setf (aref a-q12 k) (rror (aref a32 k) (- qin qout))))
        (dotimes (k d)
          (setf (aref a-q12 k) (sat16 (rror (aref a32 k) (- qin qout)))
                (aref a32 k) (ash (aref a-q12 k) (- qin qout)))))))

(defun silk-bwexpander-32 (ar d chirp-q16)
  "silk_bwexpander_32: coefficient scaling uses SMULWW (floor); the chirp step
uses RSHIFT_ROUND."
  (let ((cm1 (- chirp-q16 65536)))
    (dotimes (i (1- d))
      (setf (aref ar i) (smulww chirp-q16 (aref ar i)))
      (incf chirp-q16 (rror (* chirp-q16 cm1) 16)))
    (setf (aref ar (1- d)) (smulww chirp-q16 (aref ar (1- d))))))

(defun sub-sat32 (a b) (sat32 (- a b)))

(defun silk-lpc-inverse-pred-gain (a-q12 d)
  "silk_LPC_inverse_pred_gain (exact fixed-point, silk/LPC_inv_pred_gain.c):
returns the inverse prediction gain in energy domain Q30 (0 = unstable).  Used
as the stability gate in NLSF2A; must be bit-exact so the chirp iteration count
matches libopus."
  (let ((a-qa (make-array d)) (dc 0)                          ; QA = 24
        (a-limit 16773022) (thr 107374))
    (dotimes (k d) (incf dc (aref a-q12 k)) (setf (aref a-qa k) (ash (aref a-q12 k) 12)))
    (when (>= dc 4096) (return-from silk-lpc-inverse-pred-gain 0))
    (let ((inv-gain (ash 1 30)))
      (loop for k from (1- d) downto 0 do
        (when (or (> (aref a-qa k) a-limit) (< (aref a-qa k) (- a-limit)))
          (return-from silk-lpc-inverse-pred-gain 0))
        (let* ((rc-q31 (- (ash (aref a-qa k) 7)))             ; 31-QA = 7
               (rc-mult1 (- (ash 1 30) (smmul rc-q31 rc-q31))))
          (setf inv-gain (ash (smmul inv-gain rc-mult1) 2))
          (when (< inv-gain thr) (return-from silk-lpc-inverse-pred-gain 0))
          (when (> k 0)
            (let* ((mult2q (- 32 (clz32 (abs rc-mult1))))
                   (rc-mult2 (inverse32-varq rc-mult1 (+ mult2q 30))))
              (dotimes (n (ash (1+ k) -1))
                (let* ((tmp1 (aref a-qa n)) (tmp2 (aref a-qa (- k n 1)))
                       (t64a (rror (* (sub-sat32 tmp1 (rror (* tmp2 rc-q31) 31)) rc-mult2) mult2q))
                       (t64b (rror (* (sub-sat32 tmp2 (rror (* tmp1 rc-q31) 31)) rc-mult2) mult2q)))
                  (when (or (> t64a 2147483647) (< t64a -2147483648)
                            (> t64b 2147483647) (< t64b -2147483648))
                    (return-from silk-lpc-inverse-pred-gain 0))
                  (setf (aref a-qa n) t64a (aref a-qa (- k n 1)) t64b)))))))
      inv-gain)))

(defun silk-nlsf2a (a-q12 nlsf order)
  "silk_NLSF2A: NLSF Q15 → LPC a_Q12[order]."
  (let* ((ord16 #(0 15 8 7 4 11 12 3 2 13 10 5 6 9 14 1))
         (ord10 #(0 9 6 3 4 5 8 1 2 7))
         (ordering (if (= order 16) ord16 ord10))
         (dd (ash order -1))
         (cos-qa (make-array order))
         (pp (make-array (1+ dd) :initial-element 0))
         (qq (make-array (1+ dd) :initial-element 0))
         (a32 (make-array order)))
    (dotimes (k order)
      (let* ((fint (sh-r (aref nlsf k) 8))
             (ffrac (- (aref nlsf k) (ash fint 8)))
             (cv (aref +silk-lsf-cos-tab-q12+ fint))
             (delta (- (aref +silk-lsf-cos-tab-q12+ (1+ fint)) cv)))
        (setf (aref cos-qa (aref ordering k))
              (rror (+ (ash cv 8) (* delta ffrac)) 4))))    ; 20-QA=4, QA=16
    (flet ((find-poly (out cstart)
             (setf (aref out 0) (ash 1 16) (aref out 1) (- (aref cos-qa cstart)))
             (loop for k from 1 below dd for ftmp = (aref cos-qa (+ (* 2 k) cstart)) do
               (setf (aref out (1+ k)) (- (ash (aref out (1- k)) 1)
                                          (rror (* ftmp (aref out k)) 16)))
               (loop for n from k above 1 do
                 (incf (aref out n) (- (aref out (- n 2)) (rror (* ftmp (aref out (1- n))) 16))))
               (decf (aref out 1) ftmp))))
      (find-poly pp 0) (find-poly qq 1))
    (dotimes (k dd)
      (let ((ptmp (+ (aref pp (1+ k)) (aref pp k)))
            (qtmp (- (aref qq (1+ k)) (aref qq k))))
        (setf (aref a32 k) (- (- qtmp) ptmp)
              (aref a32 (- order k 1)) (- qtmp ptmp))))
    (silk-lpc-fit a-q12 a32 12 17 order)
    (loop for i from 0
          while (and (= (silk-lpc-inverse-pred-gain a-q12 order) 0) (< i 16)) do
      (silk-bwexpander-32 a32 order (- 65536 (ash 2 i)))
      (dotimes (k order) (setf (aref a-q12 k) (sat16 (rror (aref a32 k) 5)))))))

;;; ======================================================================
;;;  Gains (silk/gain_quant.c)
;;; ======================================================================
(defconstant +qgain-offset+ (+ (truncate (* 2 128) 6) (* 16 128)))     ; MIN_QGAIN_DB
(defconstant +qgain-inv-scale+ (truncate (* 65536 (truncate (* (- 88 2) 128) 6)) 63))

(defun silk-log2lin-exact (in-log-q7)
  "silk_log2lin (exact, silk/log2lin.c)."
  (cond ((< in-log-q7 0) 0)
        ((>= in-log-q7 3967) 2147483647)
        (t (let* ((out (ash 1 (sh-r in-log-q7 7)))
                  (frac (logand in-log-q7 #x7F))
                  (inner (smlawb frac (smulbb frac (- 128 frac)) -174)))
             (if (< in-log-q7 2048)
                 (w32 (+ out (ash (* out inner) -7)))
                 (w32 (+ out (* (sh-r out 7) inner))))))))

(defun silk-gains-dequant (ch conditional)
  "silk_gains_dequant: gain indices → linear gains_Q16[nb_subfr]."
  (let ((gains (silk-channel-gains-q16 ch))
        (ind (silk-channel-gains-idx ch))
        (prev (silk-channel-lastgainindex ch))
        (nb (silk-channel-nb-subfr ch)))
    (dotimes (k nb)
      (if (and (= k 0) (= conditional 0))
          ;; independent: not allowed to drop more than 16 steps
          (setf prev (max (aref ind k) (- prev 16)))
          ;; delta index, with double step size for large increases
          (let* ((ind-tmp (+ (aref ind k) -4))         ; MIN_DELTA_GAIN_QUANT
                 (thr (+ (- (* 2 36) 64) prev)))       ; 2*MAX - N_LEVELS + prev
            (if (> ind-tmp thr)
                (incf prev (- (ash ind-tmp 1) thr))
                (incf prev ind-tmp))))
      (setf prev (ilimit prev 0 63))
      (setf (aref gains k)
            (silk-log2lin-exact (min (+ (smulwb +qgain-inv-scale+ prev) +qgain-offset+) 3967))))
    (setf (silk-channel-lastgainindex ch) prev)))

;;; ======================================================================
;;;  Pitch lags (silk/decode_pitch.c)
;;; ======================================================================
(defun silk-decode-pitch (ch)
  "silk_decode_pitch → pitchL[nb_subfr]."
  (let* ((fs (silk-channel-fs-khz ch)) (nb (silk-channel-nb-subfr ch))
         (lag-index (silk-channel-lag-index ch))
         (contour (silk-channel-contour-index ch))
         (pitchl (silk-channel-pitchl ch))
         (min-lag (* 2 fs)) (max-lag (* 18 fs)))
    (multiple-value-bind (cb size)
        (if (= fs 8)
            (if (= nb 4) (values +silk-cb-lags-stage2+ 11) (values +silk-cb-lags-stage2-10ms+ 3))
            (if (= nb 4) (values +silk-cb-lags-stage3+ 34) (values +silk-cb-lags-stage3-10ms+ 12)))
      (let ((lag (+ min-lag lag-index)))
        (dotimes (k nb)
          (setf (aref pitchl k)
                (ilimit (+ lag (aref cb (+ (* k size) contour))) min-lag max-lag)))))))

;;; ======================================================================
;;;  Side-info indices (silk/decode_indices.c)
;;; ======================================================================
(defun silk-decode-indices (ch d frame-index decode-lbrr cond-coding)
  "silk_decode_indices."
  (let* ((ix (if (or (plusp decode-lbrr) (plusp (aref (silk-channel-vad-flags ch) frame-index)))
                 (+ (ec-dec-icdf d +silk-type-offset-vad-icdf+ 8) 2)
                 (ec-dec-icdf d +silk-type-offset-no-vad-icdf+ 8))))
    (setf (silk-channel-signal-type ch) (sh-r ix 1)
          (silk-channel-quant-offset-type ch) (logand ix 1))
    (let ((gi (silk-channel-gains-idx ch)) (st (silk-channel-signal-type ch)))
      ;; gains
      (if (= cond-coding +code-conditionally+)
          (setf (aref gi 0) (ec-dec-icdf d +silk-delta-gain-icdf+ 8))
          (setf (aref gi 0) (+ (ash (ec-dec-icdf d (%off +silk-gain-icdf+ (* st 8)) 8) 3)
                               (ec-dec-icdf d +silk-uniform8-icdf+ 8))))
      (loop for i from 1 below (silk-channel-nb-subfr ch) do
        (setf (aref gi i) (ec-dec-icdf d +silk-delta-gain-icdf+ 8)))
      ;; NLSF
      (multiple-value-bind (order nvec qstep cb1 wght cb1-icdf pred ecsel ecicdf dmin) (%nlsf-cb ch)
        (declare (ignore qstep cb1 wght pred ecsel dmin))
        (let ((idxs (silk-channel-nlsf-idx ch)))
          (setf (aref idxs 0) (ec-dec-icdf d (%off cb1-icdf (* (ash st -1) nvec)) 8))
          (multiple-value-bind (ec-ix pred-q8) (silk-nlsf-unpack ch (aref idxs 0))
            (declare (ignore pred-q8))
            (dotimes (i order)
              (let ((v (ec-dec-icdf d (%off ecicdf (aref ec-ix i)) 8)))
                (cond ((= v 0) (decf v (ec-dec-icdf d +silk-nlsf-ext-icdf+ 8)))
                      ((= v (* 2 +silk-nlsf-qmax+)) (incf v (ec-dec-icdf d +silk-nlsf-ext-icdf+ 8))))
                (setf (aref idxs (1+ i)) (- v +silk-nlsf-qmax+)))))))
      ;; interp factor
      (setf (silk-channel-nlsf-interp-q2 ch)
            (if (= (silk-channel-nb-subfr ch) 4)
                (ec-dec-icdf d +silk-nlsf-interp-factor-icdf+ 8) 4))
      ;; pitch + LTP (voiced)
      (when (= st +type-voiced+)
        (let ((decode-abs 1))
          (when (and (= cond-coding +code-conditionally+)
                     (= (silk-channel-ec-prev-signal-type ch) +type-voiced+))
            (let ((delta (ec-dec-icdf d +silk-pitch-delta-icdf+ 8)))
              (when (> delta 0)
                (setf (silk-channel-lag-index ch) (+ (silk-channel-ec-prev-lag-index ch) (- delta 9))
                      decode-abs 0))))
          (when (= decode-abs 1)
            (setf (silk-channel-lag-index ch)
                  (+ (* (ec-dec-icdf d +silk-pitch-lag-icdf+ 8) (sh-r (silk-channel-fs-khz ch) 1))
                     (ec-dec-icdf d (silk-channel-pitch-lag-low-icdf ch) 8)))))
        (setf (silk-channel-ec-prev-lag-index ch) (silk-channel-lag-index ch)
              (silk-channel-contour-index ch) (ec-dec-icdf d (silk-channel-pitch-contour-icdf ch) 8)
              (silk-channel-per-index ch) (ec-dec-icdf d +silk-ltp-per-index-icdf+ 8))
        (let* ((per (silk-channel-per-index ch))
               (gicdf (ecase per (0 +silk-ltp-gain-icdf-0+) (1 +silk-ltp-gain-icdf-1+) (2 +silk-ltp-gain-icdf-2+)))
               (lti (silk-channel-ltp-index ch)))
          (dotimes (k (silk-channel-nb-subfr ch)) (setf (aref lti k) (ec-dec-icdf d gicdf 8))))
        (setf (silk-channel-ltp-scale-index ch)
              (if (= cond-coding +code-independently+) (ec-dec-icdf d +silk-ltpscale-icdf+ 8) 0)))
      (setf (silk-channel-ec-prev-signal-type ch) (silk-channel-signal-type ch)
            (silk-channel-seed ch) (ec-dec-icdf d +silk-uniform4-icdf+ 8)))))

;;; ======================================================================
;;;  Parameters (silk/decode_parameters.c)
;;; ======================================================================
(defun silk-decode-parameters (ch cond-coding)
  "silk_decode_parameters: gains, NLSF→LPC (with interpolation), LTP taps+scale."
  (silk-gains-dequant ch (if (= cond-coding +code-conditionally+) 1 0))
  (let* ((order (silk-channel-lpc-order ch))
         (nlsf (silk-nlsf-decode ch))
         (pc (silk-channel-pred-coef ch))
         (a1 (make-array order)))
    (silk-nlsf2a a1 nlsf order)
    (dotimes (k order) (setf (aref pc 1 k) (aref a1 k)))
    (when (= (silk-channel-first-frame-after-reset ch) 1)
      (setf (silk-channel-nlsf-interp-q2 ch) 4))
    (if (< (silk-channel-nlsf-interp-q2 ch) 4)
        (let ((nlsf0 (make-array order)) (a0 (make-array order))
              (prev (silk-channel-prevnlsf ch)) (fac (silk-channel-nlsf-interp-q2 ch)))
          (dotimes (i order)
            (setf (aref nlsf0 i) (+ (aref prev i) (sh-r (* fac (- (aref nlsf i) (aref prev i))) 2))))
          (silk-nlsf2a a0 nlsf0 order)
          (dotimes (k order) (setf (aref pc 0 k) (aref a0 k))))
        (dotimes (k order) (setf (aref pc 0 k) (aref pc 1 k))))
    (dotimes (i order) (setf (aref (silk-channel-prevnlsf ch) i) (aref nlsf i))))
  ;; LTP
  (if (= (silk-channel-signal-type ch) +type-voiced+)
      (progn
        (silk-decode-pitch ch)
        (let* ((per (silk-channel-per-index ch))
               (cbk (ecase per (0 +silk-ltp-vq-0+) (1 +silk-ltp-vq-1+) (2 +silk-ltp-vq-2+)))
               (lti (silk-channel-ltp-index ch)) (lc (silk-channel-ltp-coef ch)))
          (dotimes (k (silk-channel-nb-subfr ch))
            (let ((ix (aref lti k)))
              (dotimes (i +silk-ltp-order+)
                (setf (aref lc (+ (* k +silk-ltp-order+) i))
                      (ash (aref cbk (+ (* ix +silk-ltp-order+) i)) 7)))))
          (setf (silk-channel-ltp-scale-q14 ch)
                (aref +silk-ltpscales-q14+ (silk-channel-ltp-scale-index ch)))))
      (progn
        (dotimes (k (silk-channel-nb-subfr ch)) (setf (aref (silk-channel-pitchl ch) k) 0))
        (dotimes (i 20) (setf (aref (silk-channel-ltp-coef ch) i) 0))
        (setf (silk-channel-per-index ch) 0 (silk-channel-ltp-scale-q14 ch) 0))))

;;; ======================================================================
;;;  Core inverse NSQ: LTP + LPC synthesis (silk/decode_core.c)
;;; ======================================================================
(defun silk-lpc-analysis-filter (out out-off in in-off b-q12 len d)
  "silk_LPC_analysis_filter (re-whitening)."
  (loop for ix from d below len do
    (let* ((ip (+ in-off ix -1))
           (o (w32 (smulbb (aref in ip) (aref b-q12 0)))))
      (loop for j from 1 below d do
        (setf o (w32 (+ o (* (s16 (aref in (- ip j))) (s16 (aref b-q12 j)))))))
      (setf o (w32 (- (ash (aref in (+ ip 1)) 12) o)))
      (setf (aref out (+ out-off ix)) (sat16 (rror o 12)))))
  (dotimes (ix d) (setf (aref out (+ out-off ix)) 0)))

(defun silk-decode-core (ch xq xq-off pulses)
  "silk_decode_core: reconstruct excitation, then per-subframe LTP + LPC synthesis
scaled by the subframe gains → 16-bit speech in XQ."
  (let* ((nb (silk-channel-nb-subfr ch)) (subfr (silk-channel-subfr-length ch))
         (order (silk-channel-lpc-order ch)) (ltpmem (silk-channel-ltp-mem-length ch))
         (frame-len (silk-channel-frame-length ch))
         (exc (silk-channel-exc-q14 ch))
         (offset-q10 (aref +silk-quant-offsets-q10+
                           (+ (* 2 (ash (silk-channel-signal-type ch) -1))
                              (silk-channel-quant-offset-type ch))))
         (nlsf-interp-flag (if (< (silk-channel-nlsf-interp-q2 ch) 4) 1 0))
         (sltp (make-array ltpmem :element-type 'fixnum :initial-element 0))
         (sltp-q15 (make-array (+ ltpmem frame-len) :element-type 'fixnum :initial-element 0))
         (res-q14 (make-array subfr :element-type 'fixnum :initial-element 0))
         (slpc (make-array (+ subfr +silk-max-lpc-order+) :element-type 'fixnum :initial-element 0))
         (pc (silk-channel-pred-coef ch)) (lc (silk-channel-ltp-coef ch))
         (outbuf (silk-channel-outbuf ch)) (pitchl (silk-channel-pitchl ch))
         (a-tmp (make-array order)) (rand-seed (silk-channel-seed ch))
         (sltp-idx ltpmem) (lag 0))
    ;; excitation
    (dotimes (i frame-len)
      (setf rand-seed (silk-rand rand-seed))
      (let ((e (ash (aref pulses i) 14)))
        (cond ((> e 0) (decf e (ash +silk-quant-level-adjust-q10+ 4)))
              ((< e 0) (incf e (ash +silk-quant-level-adjust-q10+ 4))))
        (incf e (ash offset-q10 4))
        (when (< rand-seed 0) (setf e (- e)))
        (setf (aref exc i) e))
      (setf rand-seed (w32 (+ rand-seed (aref pulses i)))))
    ;; copy LPC state
    (dotimes (i +silk-max-lpc-order+) (setf (aref slpc i) (aref (silk-channel-slpc-buf ch) i)))
    (let ((pexc-off 0) (pxq-off xq-off))
      (dotimes (k nb)
        (let* ((half (ash k -1))
               (gain-q16 (aref (silk-channel-gains-q16 ch) k))
               (gain-q10 (sh-r gain-q16 6))
               (inv-gain-q31 (inverse32-varq gain-q16 47))
               (signal-type (silk-channel-signal-type ch))
               (gain-adj-q16 (ash 1 16))
               (pres-off 0) (pres-array res-q14))
          (dotimes (i order) (setf (aref a-tmp i) (aref pc half i)))
          (when (/= gain-q16 (silk-channel-prev-gain-q16 ch))
            (setf gain-adj-q16 (div32-varq (silk-channel-prev-gain-q16 ch) gain-q16 16))
            (dotimes (i +silk-max-lpc-order+) (setf (aref slpc i) (smulww gain-adj-q16 (aref slpc i)))))
          (setf (silk-channel-prev-gain-q16 ch) gain-q16)
          (when (= signal-type +type-voiced+)
            (setf lag (aref pitchl k))
            (if (or (= k 0) (and (= k 2) (= nlsf-interp-flag 1)))
                (let ((start-idx (- ltpmem lag order (ash +silk-ltp-order+ -1))))
                  (when (= k 2)
                    (dotimes (i (* 2 subfr)) (setf (aref outbuf (+ ltpmem i)) (aref xq (+ xq-off i)))))
                  (silk-lpc-analysis-filter sltp start-idx outbuf (+ start-idx (* k subfr))
                                            a-tmp (- ltpmem start-idx) order)
                  (when (= k 0)
                    (setf inv-gain-q31 (ash (smulwb inv-gain-q31 (silk-channel-ltp-scale-q14 ch)) 2)))
                  (dotimes (i (+ lag (ash +silk-ltp-order+ -1)))
                    (setf (aref sltp-q15 (- sltp-idx i 1))
                          (smulwb inv-gain-q31 (aref sltp (- ltpmem i 1))))))
                (when (/= gain-adj-q16 (ash 1 16))
                  (dotimes (i (+ lag (ash +silk-ltp-order+ -1)))
                    (setf (aref sltp-q15 (- sltp-idx i 1))
                          (smulww gain-adj-q16 (aref sltp-q15 (- sltp-idx i 1))))))))
          ;; LTP
          (if (= signal-type +type-voiced+)
              (let ((pl (+ (- sltp-idx lag) (ash +silk-ltp-order+ -1)))
                    (bo (* k +silk-ltp-order+)))
                (dotimes (i subfr)
                  (let ((ltp 2))
                    (setf ltp (smlawb ltp (aref sltp-q15 (+ pl i)) (aref lc (+ bo 0))))
                    (setf ltp (smlawb ltp (aref sltp-q15 (+ pl i -1)) (aref lc (+ bo 1))))
                    (setf ltp (smlawb ltp (aref sltp-q15 (+ pl i -2)) (aref lc (+ bo 2))))
                    (setf ltp (smlawb ltp (aref sltp-q15 (+ pl i -3)) (aref lc (+ bo 3))))
                    (setf ltp (smlawb ltp (aref sltp-q15 (+ pl i -4)) (aref lc (+ bo 4))))
                    (setf (aref res-q14 i) (w32 (+ (aref exc (+ pexc-off i)) (ash ltp 1))))
                    (setf (aref sltp-q15 sltp-idx) (ash (aref res-q14 i) 1))
                    (incf sltp-idx))))
              (setf pres-array exc pres-off pexc-off))
          ;; LPC synthesis
          (dotimes (i subfr)
            (let ((lpc (ash order -1)) (bi (+ +silk-max-lpc-order+ i)))
              (dotimes (j order)
                (setf lpc (smlawb lpc (aref slpc (- bi 1 j)) (aref a-tmp j))))
              (setf (aref slpc bi) (add-sat32 (aref pres-array (+ pres-off i)) (lshift-sat32 lpc 4)))
              (setf (aref xq (+ pxq-off i)) (sat16 (rror (smulww (aref slpc bi) gain-q10) 8)))))
          (dotimes (i +silk-max-lpc-order+) (setf (aref slpc i) (aref slpc (+ subfr i))))
          (incf pexc-off subfr) (incf pxq-off subfr))))
    (dotimes (i +silk-max-lpc-order+) (setf (aref (silk-channel-slpc-buf ch) i) (aref slpc i)))))

;;; ======================================================================
;;;  Stereo (silk/stereo_decode_pred.c, stereo_MS_to_LR.c)
;;; ======================================================================
(defun silk-stereo-decode-pred (d pred-q13)
  "silk_stereo_decode_pred → pred_Q13[2]."
  (let* ((n (ec-dec-icdf d +silk-stereo-pred-joint-icdf+ 8))
         (ix (make-array '(2 3))))
    (setf (aref ix 0 2) (truncate n 5) (aref ix 1 2) (- n (* 5 (truncate n 5))))
    (dotimes (m 2)
      (setf (aref ix m 0) (ec-dec-icdf d +silk-uniform3-icdf+ 8)
            (aref ix m 1) (ec-dec-icdf d +silk-uniform5-icdf+ 8)))
    (dotimes (m 2)
      (incf (aref ix m 0) (* 3 (aref ix m 2)))
      (let* ((low (aref +silk-stereo-pred-quant-q13+ (aref ix m 0)))
             (step (smulwb (- (aref +silk-stereo-pred-quant-q13+ (1+ (aref ix m 0))) low)
                           6554)))    ; SILK_FIX_CONST(0.5/5,16)=6554
        (setf (aref pred-q13 m) (smlabb low step (+ (* 2 (aref ix m 1)) 1)))))
    (decf (aref pred-q13 0) (aref pred-q13 1))))

(defun silk-stereo-ms-to-lr (stereo x1 x2 pred-q13 fs frame-length)
  "silk_stereo_MS_to_LR: predictive mid/side → left/right with 1-sample delay
and per-sample predictor interpolation over the first 8 ms."
  (let ((smid (silk-stereo-smid stereo)) (sside (silk-stereo-sside stereo))
        (pp (silk-stereo-pred-prev stereo)))
    (setf (aref x1 0) (aref smid 0) (aref x1 1) (aref smid 1)
          (aref x2 0) (aref sside 0) (aref x2 1) (aref sside 1)
          (aref smid 0) (aref x1 frame-length) (aref smid 1) (aref x1 (1+ frame-length))
          (aref sside 0) (aref x2 frame-length) (aref sside 1) (aref x2 (1+ frame-length)))
    (let* ((pred0 (aref pp 0)) (pred1 (aref pp 1))
           (n8 (* +stereo-interp-len-ms+ fs))
           (denom (truncate (ash 1 16) n8))
           (d0 (rror (smulbb (- (aref pred-q13 0) (aref pp 0)) denom) 16))
           (d1 (rror (smulbb (- (aref pred-q13 1) (aref pp 1)) denom) 16)))
      (dotimes (n n8)
        (incf pred0 d0) (incf pred1 d1)
        (let ((sum (ash (+ (+ (aref x1 n) (aref x1 (+ n 2))) (ash (aref x1 (+ n 1)) 1)) 9)))
          (setf sum (smlawb (ash (aref x2 (+ n 1)) 8) sum pred0))
          (setf sum (smlawb sum (ash (aref x1 (+ n 1)) 11) pred1))
          (setf (aref x2 (+ n 1)) (sat16 (rror sum 8)))))
      (setf pred0 (aref pred-q13 0) pred1 (aref pred-q13 1))
      (loop for n from n8 below frame-length do
        (let ((sum (ash (+ (+ (aref x1 n) (aref x1 (+ n 2))) (ash (aref x1 (+ n 1)) 1)) 9)))
          (setf sum (smlawb (ash (aref x2 (+ n 1)) 8) sum pred0))
          (setf sum (smlawb sum (ash (aref x1 (+ n 1)) 11) pred1))
          (setf (aref x2 (+ n 1)) (sat16 (rror sum 8)))))
      (setf (aref pp 0) (aref pred-q13 0) (aref pp 1) (aref pred-q13 1))
      (dotimes (n frame-length)
        (let ((sum (+ (aref x1 (+ n 1)) (aref x2 (+ n 1))))
              (diff (- (aref x1 (+ n 1)) (aref x2 (+ n 1)))))
          (setf (aref x1 (+ n 1)) (sat16 sum) (aref x2 (+ n 1)) (sat16 diff)))))))

;;; ======================================================================
;;;  Per-channel frame decode (silk/decode_frame.c) + reset (decoder_set_fs)
;;; ======================================================================
(defun silk-channel-set-fs (ch fs-khz nb-subfr)
  "silk_decoder_set_fs (subset): configure a channel for FS-KHZ / NB-SUBFR."
  (let ((frame-length (* nb-subfr 5 fs-khz)))
    (setf (silk-channel-subfr-length ch) (* 5 fs-khz)
          (silk-channel-nb-subfr ch) nb-subfr)
    (when (/= (silk-channel-fs-khz ch) fs-khz)
      (silk-resampler-init (silk-channel-resampler ch) (* fs-khz 1000) 48000))
    (setf (silk-channel-pitch-contour-icdf ch)
          (if (= fs-khz 8)
              (if (= nb-subfr 4) +silk-pitch-contour-nb-icdf+ +silk-pitch-contour-10ms-nb-icdf+)
              (if (= nb-subfr 4) +silk-pitch-contour-icdf+ +silk-pitch-contour-10ms-icdf+)))
    (when (/= (silk-channel-fs-khz ch) fs-khz)
      (setf (silk-channel-ltp-mem-length ch) (* 20 fs-khz))
      (if (or (= fs-khz 8) (= fs-khz 12))
          (setf (silk-channel-lpc-order ch) 10 (silk-channel-nlsf-cb ch) :nbmb)
          (setf (silk-channel-lpc-order ch) 16 (silk-channel-nlsf-cb ch) :wb))
      (setf (silk-channel-pitch-lag-low-icdf ch)
            (ecase fs-khz (16 +silk-uniform8-icdf+) (12 +silk-uniform6-icdf+) (8 +silk-uniform4-icdf+))
            (silk-channel-first-frame-after-reset ch) 1
            (silk-channel-lagprev ch) 100
            (silk-channel-lastgainindex ch) 10
            (silk-channel-prev-signal-type ch) +type-no-voice+)
      (fill (silk-channel-outbuf ch) 0)
      (fill (silk-channel-slpc-buf ch) 0))
    (setf (silk-channel-fs-khz ch) fs-khz (silk-channel-frame-length ch) frame-length)))

(defun silk-decode-frame (ch d pout pout-off cond-coding)
  "silk_decode_frame (normal path): indices → pulses → parameters → core NSQ,
then slide the LTP history buffer.  Returns frame_length."
  (let* ((l (silk-channel-frame-length ch))
         (pulses (make-array (logand (+ l (1- +silk-shell-fl+)) (lognot (1- +silk-shell-fl+)))
                             :element-type 'fixnum :initial-element 0)))
    (silk-decode-indices ch d (silk-channel-nframes-decoded ch) 0 cond-coding)
    (silk-decode-pulses d pulses (silk-channel-signal-type ch)
                        (silk-channel-quant-offset-type ch) l)
    (silk-decode-parameters ch cond-coding)
    (silk-decode-core ch pout pout-off pulses)
    (setf (silk-channel-prev-signal-type ch) (silk-channel-signal-type ch)
          (silk-channel-first-frame-after-reset ch) 0)
    ;; update outBuf: memmove left by frame_length, copy pOut into tail
    (let* ((outbuf (silk-channel-outbuf ch)) (mv (- (silk-channel-ltp-mem-length ch) l)))
      (dotimes (i mv) (setf (aref outbuf i) (aref outbuf (+ i l))))
      (dotimes (i l) (setf (aref outbuf (+ mv i)) (aref pout (+ pout-off i)))))
    (setf (silk-channel-lagprev ch) (aref (silk-channel-pitchl ch) (1- (silk-channel-nb-subfr ch))))
    l))

;;; ======================================================================
;;;  Top-level SILK decode driver (silk/dec_API.c silk_Decode)
;;; ======================================================================
(defun silk-decode (dec d n-internal n-api internal-fs-khz payload-ms new-packet out out-off)
  "Decode one SILK frame of an Opus packet-frame into OUT (interleaved int16 at
48 kHz, N-API channels) starting at per-channel sample offset OUT-OFF.  Returns
the number of output samples per channel.  Mirrors one silk_Decode call in the
opus_decoder.c do/while loop (one call per SILK frame)."
  (let ((channels (silk-decoder-channels dec))
        (nframes-per-packet (ecase payload-ms (10 1) (20 1) (40 2) (60 3)))
        (nb-subfr (if (= payload-ms 10) 2 4))
        ;; stereo->mono transition: keep resampling the right channel through
        ;; channel[1] once, for a smooth collapse (silk/dec_API.c)
        (stereo-to-mono (and (= n-internal 1) (= (silk-decoder-n-internal dec) 2)
                             (= internal-fs-khz (silk-channel-fs-khz (aref channels 0))))))
    ;; init on mono->stereo
    (when (> n-internal (silk-decoder-n-internal dec))
      (setf (aref channels 1) (make-silk-channel)))
    (when new-packet
      (dotimes (nn n-internal) (setf (silk-channel-nframes-decoded (aref channels nn)) 0)))
    ;; first-frame-of-packet setup
    (when (= (silk-channel-nframes-decoded (aref channels 0)) 0)
      (dotimes (nn n-internal)
        (let ((ch (aref channels nn)))
          (setf (silk-channel-nframes-per-packet ch) nframes-per-packet)
          (silk-channel-set-fs ch internal-fs-khz nb-subfr))))
    (when (and (= n-api 2) (= n-internal 2)
               (or (= (silk-decoder-n-api dec) 1) (= (silk-decoder-n-internal dec) 1)))
      (fill (silk-stereo-pred-prev (silk-decoder-stereo dec)) 0)
      (fill (silk-stereo-sside (silk-decoder-stereo dec)) 0)
      ;; seed channel[1]'s resampler from channel[0] for a smooth expansion
      (copy-resampler-state (silk-channel-resampler (aref channels 0))
                            (silk-channel-resampler (aref channels 1))))
    (setf (silk-decoder-n-api dec) n-api (silk-decoder-n-internal dec) n-internal)
    (let ((decode-only-middle 0)
          (ms-pred (make-array 2 :element-type 'fixnum :initial-element 0)))
      ;; VAD + LBRR flags on first call of the packet
      (when (= (silk-channel-nframes-decoded (aref channels 0)) 0)
        (dotimes (nn n-internal)
          (let ((ch (aref channels nn)))
            (dotimes (i nframes-per-packet)
              (setf (aref (silk-channel-vad-flags ch) i) (ec-dec-bit-logp d 1)))
            (setf (silk-channel-lbrr-flag ch) (ec-dec-bit-logp d 1))))
        ;; LBRR flags + skip LBRR data (Stage-3 FEC deferred)
        (dotimes (nn n-internal)
          (let ((ch (aref channels nn)))
            (fill (silk-channel-lbrr-flags ch) 0)
            (when (= (silk-channel-lbrr-flag ch) 1)
              (if (= nframes-per-packet 1)
                  (setf (aref (silk-channel-lbrr-flags ch) 0) 1)
                  (let ((sym (1+ (ec-dec-icdf d (if (= nframes-per-packet 2)
                                                    +silk-lbrr-flags-2-icdf+ +silk-lbrr-flags-3-icdf+) 8))))
                    (dotimes (i nframes-per-packet)
                      (setf (aref (silk-channel-lbrr-flags ch) i) (logand (sh-r sym i) 1))))))))
        ;; consume any LBRR frames (they must be parsed to stay bit-aligned)
        (dotimes (i nframes-per-packet)
          (dotimes (nn n-internal)
            (let ((ch (aref channels nn)))
              (when (= (aref (silk-channel-lbrr-flags ch) i) 1)
                (let ((pulses (make-array +silk-max-frame-length+ :element-type 'fixnum :initial-element 0))
                      (cc +code-independently+))
                  (when (and (= n-internal 2) (= nn 0))
                    (silk-stereo-decode-pred d ms-pred)
                    (when (= (aref (silk-channel-lbrr-flags (aref channels 1)) i) 0)
                      (silk-stereo-decode-mid-only d)))
                  (when (and (> i 0) (= (aref (silk-channel-lbrr-flags ch) (1- i)) 1))
                    (setf cc +code-conditionally+))
                  (silk-decode-indices ch d i 1 cc)
                  (silk-decode-pulses d pulses (silk-channel-signal-type ch)
                                      (silk-channel-quant-offset-type ch)
                                      (silk-channel-frame-length ch))))))))
      ;; MS predictor for the current frame
      (when (= n-internal 2)
        (silk-stereo-decode-pred d ms-pred)
        (if (= (aref (silk-channel-vad-flags (aref channels 1))
                     (silk-channel-nframes-decoded (aref channels 0))) 0)
            (setf decode-only-middle (silk-stereo-decode-mid-only d))
            (setf decode-only-middle 0)))
      ;; reset side channel on first side frame
      (when (and (= n-internal 2) (= decode-only-middle 0)
                 (= (silk-decoder-prev-decode-only-middle dec) 1))
        (let ((ch1 (aref channels 1)))
          (fill (silk-channel-outbuf ch1) 0) (fill (silk-channel-slpc-buf ch1) 0)
          (setf (silk-channel-lagprev ch1) 100 (silk-channel-lastgainindex ch1) 10
                (silk-channel-prev-signal-type ch1) +type-no-voice+
                (silk-channel-first-frame-after-reset ch1) 1)))
      (let* ((fl (silk-channel-frame-length (aref channels 0)))
             (t0 (make-array (+ fl 2) :element-type 'fixnum :initial-element 0))
             (t1 (make-array (+ fl 2) :element-type 'fixnum :initial-element 0))
             (tmps (vector t0 t1))
             (has-side (= decode-only-middle 0))
             (nsdec fl))
        (dotimes (nn n-internal)
          (let ((ch (aref channels nn)))
            (if (or (= nn 0) has-side)
                (let ((fi (- (silk-channel-nframes-decoded (aref channels 0)) nn))
                      (cc +code-independently+))
                  (cond ((<= fi 0) (setf cc +code-independently+))
                        ((and (> nn 0) (= (silk-decoder-prev-decode-only-middle dec) 1))
                         (setf cc +code-independently-no-ltp+))
                        (t (setf cc +code-conditionally+)))
                  (setf nsdec (silk-decode-frame ch d (aref tmps nn) 2 cc)))
                (dotimes (i nsdec) (setf (aref (aref tmps nn) (+ 2 i)) 0)))
            (incf (silk-channel-nframes-decoded ch))))
        ;; stereo unmix or mono buffering
        (if (and (= n-api 2) (= n-internal 2))
            (silk-stereo-ms-to-lr (silk-decoder-stereo dec) t0 t1 ms-pred
                                  (silk-channel-fs-khz (aref channels 0)) nsdec)
            (let ((smid (silk-stereo-smid (silk-decoder-stereo dec))))
              (setf (aref t0 0) (aref smid 0) (aref t0 1) (aref smid 1)
                    (aref smid 0) (aref t0 nsdec) (aref smid 1) (aref t0 (1+ nsdec)))))
        ;; resample each channel to 48 kHz, interleave
        (let* ((fs-khz (silk-channel-fs-khz (aref channels 0)))
               (nout (truncate (* nsdec 48) fs-khz))
               (rout (make-array nout :element-type 'fixnum :initial-element 0)))
          (dotimes (nn (min n-api n-internal))
            (silk-resampler (silk-channel-resampler (aref channels nn)) rout 0 (aref tmps nn) 1 nsdec)
            (if (= n-api 2)
                (dotimes (i nout) (setf (aref out (+ nn (* 2 (+ out-off i)))) (aref rout i)))
                (dotimes (i nout) (setf (aref out (+ out-off i)) (aref rout i)))))
          (when (and (= n-api 2) (= n-internal 1))
            (if stereo-to-mono
                ;; flush channel[1]'s resampler on the mono mid for a smooth collapse
                (progn
                  (silk-resampler (silk-channel-resampler (aref channels 1)) rout 0 t0 1 nsdec)
                  (dotimes (i nout) (setf (aref out (+ 1 (* 2 (+ out-off i)))) (aref rout i))))
                (dotimes (i nout)
                  (setf (aref out (+ 1 (* 2 (+ out-off i)))) (aref out (* 2 (+ out-off i)))))))
          (setf (silk-decoder-prev-decode-only-middle dec) decode-only-middle)
          nout)))))

(defun silk-stereo-decode-mid-only (d)
  (ec-dec-icdf d +silk-stereo-only-code-mid-icdf+ 8))
