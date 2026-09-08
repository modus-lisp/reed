;;;; src/ac3/decode.lisp — AC-3 (ATSC A/52), which is why a DVD used to play silently.
;;;;
;;;; AC-3 is the reason this stack could open a `.vob' or a transport stream, show the picture, and
;;;; name the audio as undecodable.  It is the oldest codec here and the least like the others: it
;;;; carries an explicit PSYCHOACOUSTIC MODEL that both encoder and decoder run, in lockstep, on
;;;; integer arithmetic.  Nothing in the bitstream says how many bits a coefficient got — the
;;;; decoder works it out by recomputing the masking curve from the exponents and reading off a
;;;; table.  Get one step of that wrong and the mantissas are read at the wrong widths from that
;;;; point on, so the failure is total rather than gradual.  There is no graceful degradation in
;;;; here anywhere.
;;;;
;;;; The reference is ffmpeg's decoder rather than A/52 itself: the published specification is a PDF
;;;; whose text is not extractable, and this repository's habit with such things is to parse the C.
;;;; The tables in tables.lisp are transcribed by machine from it with invariants checked; the
;;;; algorithm below follows §7 as ffmpeg implements it.
;;;;
;;;; WHAT IS NOT HERE.  E-AC-3 (bitstream id 16) is a different format wearing the same syncword and
;;;; is refused by name.  So is enhanced coupling.  Downmixing is not done: a 5.1 stream decodes to
;;;; six channels in the order ffmpeg uses, and a caller that wants stereo can ask reed's mixer for
;;;; it — the alternative is baking one listening decision into the decoder.

(in-package #:reed)

;;; COVERAGE, not statistics.  Coupling, rematrixing, short blocks, the delta bit allocation and the
;;; LFE channel are paths a fixture either exercises or does not, and a corpus re-encoded some day
;;; could stop covering one without any test failing.  The suite counts them and asserts on them.

(defstruct (ac3-stats (:conc-name ast-))
  (frames 0 :type fixnum) (blocks 0 :type fixnum)
  (coupled 0 :type fixnum) (rematrixed 0 :type fixnum) (short-blocks 0 :type fixnum)
  (dithered 0 :type fixnum) (delta-alloc 0 :type fixnum) (lfe-frames 0 :type fixnum)
  (exp-d15 0 :type fixnum) (exp-d25 0 :type fixnum) (exp-d45 0 :type fixnum)
  (exp-reuse 0 :type fixnum) (phase-flags 0 :type fixnum)
  (acmods (make-array 8 :element-type 'fixnum) :type (simple-array fixnum (8))))

(defvar *ac3-stats* nil "Bind to an AC3-STATS to count which paths a decode takes.")

(defconstant +ac3-cpl-ch+ 0 "Index of the coupling channel; full-bandwidth channels start at one.")
(defconstant +ac3-max-ch+ 7)
(defconstant +ac3-bands+ 50 "Critical bands in the masking model.")
(defconstant +ac3-max-coefs+ 256)

;;; Exponent strategies (§7.1.3): reuse the previous block's, or one exponent per 1, 2 or 4 bins.
(defconstant +exp-reuse+ 0)
(defconstant +exp-d15+ 1)
(defconstant +exp-d25+ 2)
(defconstant +exp-d45+ 3)

(defstruct (ac3-decoder (:conc-name a3-) (:constructor %make-ac3-decoder))
  ;; ---- from the frame header
  (sr-code 0 :type fixnum) (sr-shift 0 :type fixnum)
  (sample-rate 0 :type fixnum) (bit-rate 0 :type fixnum) (frame-size 0 :type fixnum)
  (bsid 0 :type fixnum) (bsmod 0 :type fixnum)
  (acmod 0 :type fixnum) (lfe-on nil)
  (channels 0 :type fixnum) (fbw-channels 0 :type fixnum) (lfe-ch 0 :type fixnum)
  (num-blocks 6 :type fixnum)
  (dialnorm (make-array 2 :element-type 'fixnum :initial-element -31))
  ;; ---- per-block state that persists across blocks (AC-3 reuses aggressively)
  (exp-strategy (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (dexps (let ((v (make-array +ac3-max-ch+)))
           (dotimes (i +ac3-max-ch+ v)
             (setf (aref v i) (make-array 256 :element-type 'fixnum :initial-element 0))))
         :type simple-vector)
  (start-freq (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (end-freq (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (num-exp-groups (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (block-switch (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (dither-flag (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  ;; coupling
  (cpl-in-use nil)
  (channel-in-cpl (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (phase-flags-in-use nil)
  (phase-flags (make-array 18 :element-type 'fixnum) :type (simple-array fixnum (18)))
  (num-cpl-bands 0 :type fixnum)
  (cpl-band-sizes (make-array 18 :element-type 'fixnum) :type (simple-array fixnum (18)))
  (cpl-coords (let ((v (make-array +ac3-max-ch+)))
                (dotimes (i +ac3-max-ch+ v)
                  (setf (aref v i) (make-array 18 :element-type 'double-float
                                                  :initial-element 0d0))))
              :type simple-vector)
  (first-cpl-coords (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  ;; rematrixing
  (num-remat-bands 0 :type fixnum)
  (remat-flags (make-array 4 :element-type 'fixnum) :type (simple-array fixnum (4)))
  ;; bit allocation parameters
  (slow-decay 0 :type fixnum) (fast-decay 0 :type fixnum)
  (slow-gain 0 :type fixnum) (db-per-bit 0 :type fixnum) (floor-val 0 :type fixnum)
  (cpl-fast-leak 0 :type fixnum) (cpl-slow-leak 0 :type fixnum)
  (snr-offset (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (fast-gain (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (dba-mode (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (dba-nsegs (make-array +ac3-max-ch+ :element-type 'fixnum) :type (simple-array fixnum (7)))
  (dba-offsets (make-array (list +ac3-max-ch+ 8) :element-type 'fixnum))
  (dba-lengths (make-array (list +ac3-max-ch+ 8) :element-type 'fixnum))
  (dba-values (make-array (list +ac3-max-ch+ 8) :element-type 'fixnum))
  ;; scratch for the masking model
  (psd (make-array 256 :element-type 'fixnum) :type (simple-array fixnum (256)))
  (band-psd (make-array +ac3-bands+ :element-type 'fixnum) :type (simple-array fixnum (50)))
  (mask (make-array +ac3-bands+ :element-type 'fixnum) :type (simple-array fixnum (50)))
  (bap (let ((v (make-array +ac3-max-ch+)))
         (dotimes (i +ac3-max-ch+ v)
           (setf (aref v i) (make-array 256 :element-type 'fixnum :initial-element 0))))
       :type simple-vector)
  ;; coefficients and the lapping delay
  (coeffs (let ((v (make-array +ac3-max-ch+)))
            (dotimes (i +ac3-max-ch+ v)
              (setf (aref v i) (make-array 256 :element-type 'double-float
                                               :initial-element 0d0))))
          :type simple-vector)
  (delay (let ((v (make-array +ac3-max-ch+)))
           (dotimes (i +ac3-max-ch+ v)
             (setf (aref v i) (make-array 256 :element-type 'double-float
                                              :initial-element 0d0))))
         :type simple-vector)
  (dynamic-range (make-array 2 :element-type 'double-float :initial-element 1d0))
  (window nil)
  ;; output: NUM-BLOCKS * 256 samples per channel
  (out (let ((v (make-array +ac3-max-ch+)))
         (dotimes (i +ac3-max-ch+ v)
           (setf (aref v i) (make-array 1536 :element-type 'double-float
                                             :initial-element 0d0))))
       :type simple-vector))

;;; ---- the window ---------------------------------------------------------------------------------
;;;
;;; A Kaiser-Bessel derived window, alpha 5, half-length 256, mirrored to 512.  The specification
;;; prints it as a table of 256 numbers; generating it from the definition says what it is, and the
;;; fifty-term Bessel series below is the same one the reference implementation uses, so the two
;;; agree to float precision.

(defun %ac3-dynamic-range (bits)
  "§7.7.1.  The gain a DYNRNG field asks for.

   Three bits of exponent, five of mantissa, and the TOP BIT IS EFFECTIVELY A SIGN: values below
   128 amplify and values at or above it attenuate, which is what the -8*(bits>>7) term does.  A
   field of zero is unity gain.

   This is applied because a reference decoder applies it by default, and because it is not
   optional in any meaningful sense: Dolby-encoded material uses it constantly, and a decoder that
   ignores it plays quiet passages far too loud.  An encoder that never emits the field — ffmpeg's
   does not — hides the whole question, which is how this came to be wrong for as long as it was."
  (declare (type (unsigned-byte 8) bits))
  (* (expt 2d0 (- (ash bits -5) (* 8 (ash bits -7)) 5))
     (+ 32 (logand bits 31))))

(defun %ac3-window ()
  (let* ((n 256)
         (half (make-array n :element-type 'double-float))
         (w (make-array 512 :element-type 'double-float))
         (alpha2 (let ((x (/ (* 5d0 pi) n))) (* x x)))
         (sum 0d0))
    (dotimes (i n)
      (let ((tmp (* i (- n i) alpha2))
            (bessel 1d0))
        (loop for j of-type fixnum downfrom 50 above 0
              do (setf bessel (+ 1d0 (/ (* bessel tmp) (* j j)))))
        (incf sum bessel)
        (setf (aref half i) sum)))
    (incf sum 1d0)
    (dotimes (i n)
      (let ((v (sqrt (/ (aref half i) sum))))
        (setf (aref w i) v
              (aref w (- 511 i)) v)))
    w))

;;; ---- the frame header (§5.3) ---------------------------------------------------------------------

(defun %ac3-parse-header (d b)
  "syncinfo and bsi.  Returns the frame size in bytes."
  (declare (type ac3-decoder d) (type abits b))
  (let ((sync (ab b 16)))
    (unless (= sync #x0b77) (ac3-error "frame sync is #x~4,'0x, not #x0b77" sync)))
  (ab-skip b 16)                        ; crc1, checked by nobody: crc2 covers the whole frame
  (let* ((sr-code (ab b 2))
         (frmsizecod (ab b 6))
         (bsid (ab b 5))
         (bsmod (ab b 3)))
    (when (= sr-code 3) (ac3-error "sample rate code 3 is reserved"))
    (when (> frmsizecod 37) (ac3-error "frame size code ~d is reserved" frmsizecod))
    (when (> bsid 10)
      (ac3-error "bitstream id ~d is Enhanced AC-3, which is a different format wearing the same syncword"
                 bsid))
    (when (< bsid 8) nil)               ; 6 and 8 are ordinary AC-3; 6 adds optional fields below
    (let* ((acmod (ab b 3))
           (cmixlev (if (and (logtest acmod 1) (/= acmod 1)) (ab b 2) 0))
           (surmixlev (if (logtest acmod 4) (ab b 2) 0))
           (dsurmod (if (= acmod 2) (ab b 2) 0))
           (lfe-on (plusp (ab1 b))))
      (declare (ignore cmixlev surmixlev dsurmod))
      (let* ((fbw (aref +ac3-channels+ acmod))
             (channels (+ fbw (if lfe-on 1 0))))
        (setf (a3-sr-code d) sr-code
              (a3-sr-shift d) (max 0 (- bsid 8))
              (a3-sample-rate d) (ash (aref +ac3-sample-rate+ sr-code) (- (max 0 (- bsid 8))))
              (a3-bit-rate d) (* 1000 (ash (aref +ac3-bitrate+ (ash frmsizecod -1))
                                           (- (max 0 (- bsid 8)))))
              (a3-frame-size d) (* 2 (aref +ac3-frame-size+ frmsizecod sr-code))
              (a3-bsid d) bsid (a3-bsmod d) bsmod
              (a3-acmod d) acmod (a3-lfe-on d) lfe-on
              (a3-fbw-channels d) fbw
              (a3-channels d) channels
              (a3-lfe-ch d) (1+ fbw)
              (a3-num-blocks d) 6))
      ;; the rest of the bsi, twice over for dual mono
      (dotimes (i (if (zerop acmod) 2 1))
        (setf (aref (a3-dialnorm d) i) (- (ab b 5)))
        (when (plusp (ab1 b)) (ab-skip b 8))    ; compr
        (when (plusp (ab1 b)) (ab-skip b 8))    ; langcod
        (when (plusp (ab1 b)) (ab-skip b 7)))   ; audio production information
      (ab-skip b 2)                             ; copyrightb, origbs
      (if (= (a3-bsid d) 6)
          ;; the Alternate Bit Stream Syntax puts mix levels here instead of timecodes
          (progn
            (when (plusp (ab1 b)) (ab-skip b 14))
            (when (plusp (ab1 b)) (ab-skip b 14)))
          (progn
            (when (plusp (ab1 b)) (ab-skip b 14))
            (when (plusp (ab1 b)) (ab-skip b 14))))
      (when (plusp (ab1 b))                     ; addbsi
        (let ((n (ab b 6))) (ab-skip b (* 8 (1+ n)))))
      ;; the LFE channel is fixed: seven coefficients, two exponent groups, never coupled
      (when (a3-lfe-on d)
        (let ((lfe (a3-lfe-ch d)))
          (setf (aref (a3-start-freq d) lfe) 0
                (aref (a3-end-freq d) lfe) 7
                (aref (a3-num-exp-groups d) lfe) 2
                (aref (a3-channel-in-cpl d) lfe) 0)))
      (a3-frame-size d))))

;;; ---- exponents (§7.1.3) -------------------------------------------------------------------------

(defun %ac3-ungroup-3-in-7 (v)
  "Three base-5 digits packed into seven bits."
  (declare (type fixnum v))
  (values (floor v 25) (mod (floor v 5) 5) (mod v 5)))

(defun %ac3-decode-exponents (b strategy ngrps absexp out start)
  "Differential exponents, unpacked and integrated (§7.1.3).

   The differentials are base-5 digits three to a seven-bit group, biased by two, and each one is
   repeated across as many bins as the strategy covers — one, two or four."
  (declare (type abits b) (type fixnum strategy ngrps absexp start)
           (type (simple-array fixnum (*)) out))
  (let ((group-size (+ strategy (if (= strategy +exp-d45+) 1 0)))
        (prev absexp)
        (j start))
    (declare (type fixnum group-size prev j))
    (dotimes (grp ngrps)
      (let ((expacc (ab b 7)))
        (when (>= expacc 125) (ac3-error "exponent group ~d is out of range" expacc))
        (multiple-value-bind (d0 d1 d2) (%ac3-ungroup-3-in-7 expacc)
          (dolist (dexp (list d0 d1 d2))
            (incf prev (- dexp 2))
            (when (or (minusp prev) (> prev 24))
              (ac3-error "exponent ~d is out of range" prev))
            (dotimes (k group-size)
              (setf (aref out j) prev)
              (incf j))))))
    j))

;;; ---- the psychoacoustic model (§7.2) -------------------------------------------------------------
;;;
;;; This is the part with no bitstream in it.  From the exponents alone, both ends compute a power
;;; spectral density, integrate it into fifty critical bands, spread it with a leaky excitation
;;; function, clamp it against a hearing threshold, and read a bit allocation off a table.  The
;;; encoder did exactly this to decide how many bits to spend; the decoder does it to find out.

(defun %ac3-calc-psd (dexps start end psd band-psd)
  (declare (type (simple-array fixnum (*)) dexps psd) (type (simple-array fixnum (50)) band-psd)
           (type fixnum start end) (optimize (speed 3) (safety 1)))
  (loop for bin of-type fixnum from start below end
        do (setf (aref psd bin) (- 3072 (ash (aref dexps bin) 7))))
  (let ((bin start) (band (aref +ac3-bin-to-band+ start)))
    (declare (type fixnum bin band))
    (loop
      (let ((v (aref psd bin)))
        (declare (type fixnum v))
        (incf bin)
        (let ((band-end (min (aref +ac3-band-start+ (1+ band)) end)))
          (declare (type fixnum band-end))
          (loop while (< bin band-end)
                do (let* ((mx (max v (aref psd bin)))
                          (adr (min (- mx (ash (+ v (aref psd bin) 1) -1)) 255)))
                     (declare (type fixnum mx adr))
                     (setf v (+ mx (aref +ac3-log-add+ adr)))
                     (incf bin)))
          (setf (aref band-psd band) v)
          (incf band)))
      (when (<= end (aref +ac3-band-start+ band)) (return)))))

(declaim (inline %lowcomp1))
(defun %lowcomp1 (a b0 b1 c)
  (declare (type fixnum a b0 b1 c))
  (cond ((= (+ b0 256) b1) c)
        ((> b0 b1) (max (- a 64) 0))
        (t a)))

(defun %lowcomp (a b0 b1 bin)
  (declare (type fixnum a b0 b1 bin))
  (cond ((< bin 7) (%lowcomp1 a b0 b1 384))
        ((< bin 20) (%lowcomp1 a b0 b1 320))
        (t (max (- a 128) 0))))

(defun %ac3-calc-mask (d band-psd start end fast-gain lfe-p ch mask)
  "The excitation function and the masking curve (§7.2.2)."
  (declare (type ac3-decoder d) (type (simple-array fixnum (50)) band-psd mask)
           (type fixnum start end fast-gain ch) (optimize (speed 3) (safety 1)))
  (when (<= end 0) (ac3-error "a channel with no coefficients"))
  (let* ((excite (make-array +ac3-bands+ :element-type 'fixnum :initial-element 0))
         (band-start (aref +ac3-bin-to-band+ start))
         (band-end (1+ (aref +ac3-bin-to-band+ (1- end))))
         (begin 0) (lowcomp 0) (fastleak 0) (slowleak 0))
    (declare (type fixnum band-start band-end begin lowcomp fastleak slowleak)
             (dynamic-extent excite))
    (if (zerop band-start)
        (progn
          (setf lowcomp (%lowcomp1 lowcomp (aref band-psd 0) (aref band-psd 1) 384))
          (setf (aref excite 0) (- (aref band-psd 0) fast-gain lowcomp))
          (setf lowcomp (%lowcomp1 lowcomp (aref band-psd 1) (aref band-psd 2) 384))
          (setf (aref excite 1) (- (aref band-psd 1) fast-gain lowcomp))
          (setf begin 7)
          (loop for band of-type fixnum from 2 below 7
                do (unless (and lfe-p (= band 6))
                     (setf lowcomp (%lowcomp1 lowcomp (aref band-psd band)
                                              (aref band-psd (1+ band)) 384)))
                   (setf fastleak (- (aref band-psd band) fast-gain))
                   (setf slowleak (- (aref band-psd band) (a3-slow-gain d)))
                   (setf (aref excite band) (- fastleak lowcomp))
                   (unless (and lfe-p (= band 6))
                     (when (<= (aref band-psd band) (aref band-psd (1+ band)))
                       (setf begin (1+ band))
                       (return))))
          (let ((end1 (min band-end 22)))
            (declare (type fixnum end1))
            (loop for band of-type fixnum from begin below end1
                  do (unless (and lfe-p (= band 6))
                       (setf lowcomp (%lowcomp lowcomp (aref band-psd band)
                                               (aref band-psd (1+ band)) band)))
                     (setf fastleak (max (- fastleak (a3-fast-decay d))
                                         (- (aref band-psd band) fast-gain)))
                     (setf slowleak (max (- slowleak (a3-slow-decay d))
                                         (- (aref band-psd band) (a3-slow-gain d))))
                     (setf (aref excite band) (max (- fastleak lowcomp) slowleak))))
          (setf begin 22))
        ;; the coupling channel starts from the leak values the stream stated
        (progn
          (setf begin band-start)
          (setf fastleak (+ (ash (a3-cpl-fast-leak d) 8) 768))
          (setf slowleak (+ (ash (a3-cpl-slow-leak d) 8) 768))))
    (loop for band of-type fixnum from begin below band-end
          do (setf fastleak (max (- fastleak (a3-fast-decay d))
                                 (- (aref band-psd band) fast-gain)))
             (setf slowleak (max (- slowleak (a3-slow-decay d))
                                 (- (aref band-psd band) (a3-slow-gain d))))
             (setf (aref excite band) (max fastleak slowleak)))
    (loop for band of-type fixnum from band-start below band-end
          do (let ((tmp (- (a3-db-per-bit d) (aref band-psd band))))
               (declare (type fixnum tmp))
               (when (plusp tmp) (incf (aref excite band) (ash tmp -2)))
               (setf (aref mask band)
                     (max (aref +ac3-hearing-threshold+ (ash band (- (a3-sr-shift d)))
                                (a3-sr-code d))
                          (aref excite band)))))
    ;; ---- delta bit allocation: the encoder's override of its own model
    ;; §7.2.2.7.  The strategy codes are REUSE 0, NEW 1, NONE 2, reserved 3 — the deltas apply
    ;; for the first two and the third means there are none.  Reading NEW as 2 skips the segment
    ;; fields, and the parse drifts from the first block that uses them: everything before is
    ;; perfect and everything after is noise.
    (let ((mode (aref (a3-dba-mode d) ch)))
      (declare (type fixnum mode))
      (when (or (= mode 0) (= mode 1))
        (let ((band band-start))
          (declare (type fixnum band))
          (dotimes (seg (aref (a3-dba-nsegs d) ch))
            (incf band (aref (a3-dba-offsets d) ch seg))
            (when (>= band +ac3-bands+) (ac3-error "delta bit allocation runs past band 49"))
            (let ((delta (if (>= (aref (a3-dba-values d) ch seg) 4)
                             (* 128 (- (aref (a3-dba-values d) ch seg) 3))
                             (* 128 (- (aref (a3-dba-values d) ch seg) 4)))))
              (declare (type fixnum delta))
              (dotimes (i (aref (a3-dba-lengths d) ch seg))
                (when (>= band +ac3-bands+) (return))
                (incf (aref mask band) delta)
                (incf band)))))))
    (values band-start band-end)))

(defun %ac3-calc-bap (d ch start end)
  "Read the bit allocation off the table (§7.2.3): how many bits each mantissa was given."
  (declare (type ac3-decoder d) (type fixnum ch start end) (optimize (speed 3) (safety 1)))
  (let ((mask (a3-mask d)) (psd (a3-psd d)) (bap (aref (a3-bap d) ch))
        (snr (aref (a3-snr-offset d) ch)) (flr (a3-floor-val d)))
    (declare (type (simple-array fixnum (50)) mask) (type (simple-array fixnum (256)) psd)
             (type (simple-array fixnum (*)) bap) (type fixnum snr flr))
    (if (= snr -960)
        (fill bap 0)
        (let ((bin start) (band (aref +ac3-bin-to-band+ start)))
          (declare (type fixnum bin band))
          (loop
            (let* ((m (+ (logand (max (- (aref mask band) snr flr) 0) #x1fe0) flr))
                   (band-end (min (aref +ac3-band-start+ (1+ band)) end)))
              (declare (type fixnum m band-end))
              (incf band)
              (loop while (< bin band-end)
                    do (let ((address (max 0 (min 63 (ash (- (aref psd bin) m) -5)))))
                         (declare (type fixnum address))
                         (setf (aref bap bin) (aref +ac3-bap+ address))
                         (incf bin)))
              (when (<= end band-end) (return)))))))
  (values))

(defun %ac3-bit-alloc (d ch)
  "The whole model for one channel: PSD, mask, bap."
  (declare (type ac3-decoder d) (type fixnum ch))
  (let ((start (aref (a3-start-freq d) ch))
        (end (aref (a3-end-freq d) ch)))
    (declare (type fixnum start end))
    (%ac3-calc-psd (aref (a3-dexps d) ch) start end (a3-psd d) (a3-band-psd d))
    (%ac3-calc-mask d (a3-band-psd d) start end (aref (a3-fast-gain d) ch)
                    (and (a3-lfe-on d) (= ch (a3-lfe-ch d))) ch (a3-mask d))
    (%ac3-calc-bap d ch start end)))

;;; ---- mantissas (§7.3) ----------------------------------------------------------------------------
;;;
;;; THREE OF THE QUANTISERS PACK SEVERAL VALUES INTO ONE FIELD, because their level counts are not
;;; powers of two: three levels go three-to-five-bits, five levels three-to-seven, eleven levels
;;; two-to-seven.  So reading a mantissa sometimes reads nothing at all and returns a digit cached
;;; from an earlier field — and that cache is shared across CHANNELS within a block, in the order
;;; the channels are decoded, with the coupling channel wedged in after the first channel that uses
;;; it.  Decode the channels in another order and the groups line up against the wrong coefficients.

(defstruct (mant-groups (:conc-name mg-))
  (b1-count 0 :type fixnum) (b1 (make-array 2 :element-type 'fixnum))
  (b2-count 0 :type fixnum) (b2 (make-array 2 :element-type 'fixnum))
  (b4-count 0 :type fixnum) (b4 0 :type fixnum))

(declaim (inline %symmetric-dequant))
(defun %symmetric-dequant (code levels)
  "An L-level symmetric quantiser reconstructs code m as (2m - (L-1))/L, in units of 2^-24."
  (declare (type fixnum code levels))
  (truncate (* (- (* 2 code) (1- levels)) (ash 1 23)) levels))

;;; DITHER FOR ZERO-BIT MANTISSAS (§7.3.4).  A coefficient that the allocator gave no bits is not
;;; set to silence: it is filled with noise at roughly a third of full scale, so that a band the
;;; encoder decided not to code sounds like the noise floor rather than like a hole.  The sequence
;;; is explicitly pseudo-random and NOT normative — no two AC-3 decoders produce the same samples
;;; here, which is why a differential test against another decoder cannot reach one and why the
;;; agreement improves with bit rate: at 384 kbit almost nothing is dithered and the match is
;;; 0.99999, at 96 kbit a great deal is.
;;;
;;; The amplitude is matched to the reference decoder's; the sequence is a plain linear congruential
;;; generator, because any generator is as correct as any other.

(defvar *ac3-dither-state* 1)
(declaim (type (unsigned-byte 32) *ac3-dither-state*))

(declaim (inline %ac3-dither))
(defun %ac3-dither ()
  (setf *ac3-dither-state*
        (logand (+ (* 1103515245 *ac3-dither-state*) 12345) #xffffffff))
  (- (ash (* (ash *ac3-dither-state* -8) 181) -8) 5931008))

(defun %ac3-read-mantissa (b bap m dither)
  "One mantissa in units of 2^-24 (§7.3.5).  BAP zero means the coefficient got no bits at all."
  (declare (type abits b) (type fixnum bap) (type mant-groups m))
  (case bap
    (0 (if dither (%ac3-dither) 0))
    (1 (if (plusp (mg-b1-count m))
           (progn (decf (mg-b1-count m)) (aref (mg-b1 m) (mg-b1-count m)))
           (let ((bits (ab b 5)))
             (when (>= bits 27) (ac3-error "a three-level mantissa group of ~d" bits))
             (setf (aref (mg-b1 m) 1) (%symmetric-dequant (mod (floor bits 3) 3) 3)
                   (aref (mg-b1 m) 0) (%symmetric-dequant (mod bits 3) 3)
                   (mg-b1-count m) 2)
             (%symmetric-dequant (floor bits 9) 3))))
    (2 (if (plusp (mg-b2-count m))
           (progn (decf (mg-b2-count m)) (aref (mg-b2 m) (mg-b2-count m)))
           (let ((bits (ab b 7)))
             (when (>= bits 125) (ac3-error "a five-level mantissa group of ~d" bits))
             (setf (aref (mg-b2 m) 1) (%symmetric-dequant (mod (floor bits 5) 5) 5)
                   (aref (mg-b2 m) 0) (%symmetric-dequant (mod bits 5) 5)
                   (mg-b2-count m) 2)
             (%symmetric-dequant (floor bits 25) 5))))
    (3 (%symmetric-dequant (ab b 3) 7))
    (4 (if (plusp (mg-b4-count m))
           (progn (setf (mg-b4-count m) 0) (mg-b4 m))
           (let ((bits (ab b 7)))
             (when (>= bits 121) (ac3-error "an eleven-level mantissa group of ~d" bits))
             (setf (mg-b4 m) (%symmetric-dequant (mod bits 11) 11)
                   (mg-b4-count m) 1)
             (%symmetric-dequant (floor bits 11) 11))))
    (5 (%symmetric-dequant (ab b 4) 15))
    (t ;; bap 6..15: the mantissa is stored directly, left-justified in 24 bits
     (let ((width (aref +ac3-quantization+ bap)))
       (declare (type fixnum width))
       (ash (ab-signed b width) (- 24 width))))))

(defun %ac3-read-channel-coeffs (d b ch m &optional (dither t))
  "The mantissas of one channel, dequantised by its exponents into COEFFS."
  (declare (type ac3-decoder d) (type abits b) (type fixnum ch) (type mant-groups m))
  (let ((bap (aref (a3-bap d) ch))
        (dexps (aref (a3-dexps d) ch))
        (coeffs (aref (a3-coeffs d) ch))
        (start (aref (a3-start-freq d) ch))
        (end (aref (a3-end-freq d) ch)))
    (declare (type (simple-array fixnum (*)) bap dexps)
             (type (simple-array double-float (*)) coeffs) (type fixnum start end))
    (loop for i of-type fixnum from start below end
          do (let ((mant (%ac3-read-mantissa b (aref bap i) m dither)))
               (declare (type fixnum mant))
               ;; the mantissa is in units of 2^-24 and the exponent divides by a further 2^e
               (setf (aref coeffs i)
                     (* mant (expt 2d0 (- -24 (aref dexps i)))))))
    ;; THE TAIL IS NOT ZEROED HERE.  A coupled channel's own mantissas stop where the coupling
    ;; range begins, and the coupling fills the rest — but coupling is applied while the FIRST
    ;; coupled channel is being read, so zeroing from END-FREQ here would wipe what coupling had
    ;; already written into every later channel.  The zeroing happens once, after all of them, and
    ;; starts above the coupling range rather than below it.
    ))

;;; ---- coupling and rematrixing (§7.4) -------------------------------------------------------------

(defun %ac3-apply-coupling (d)
  "Rebuild the coupled channels from the coupling channel and their per-band coordinates."
  (declare (type ac3-decoder d))
  (let ((bin (aref (a3-start-freq d) +ac3-cpl-ch+))
        (cpl (aref (a3-coeffs d) +ac3-cpl-ch+)))
    (declare (type fixnum bin) (type (simple-array double-float (*)) cpl))
    (dotimes (band (a3-num-cpl-bands d))
      (let ((band-start bin)
            (band-end (+ bin (aref (a3-cpl-band-sizes d) band))))
        (declare (type fixnum band-start band-end))
        (loop for ch of-type fixnum from 1 to (a3-fbw-channels d)
              do (when (plusp (aref (a3-channel-in-cpl d) ch))
                   (let ((coord (aref (the (simple-array double-float (*))
                                           (aref (a3-cpl-coords d) ch))
                                      band))
                         (dst (aref (a3-coeffs d) ch)))
                     (declare (type double-float coord)
                              (type (simple-array double-float (*)) dst))
                     (loop for i of-type fixnum from band-start below band-end
                           do (setf (aref dst i) (* (aref cpl i) coord)))
                     ;; channel two's phase can be inverted per band, which is what lets a
                     ;; single coupling channel serve two that are out of phase
                     (when (and (= ch 2) (plusp (aref (a3-phase-flags d) band)))
                       (loop for i of-type fixnum from band-start below band-end
                             do (setf (aref dst i) (- (aref dst i))))))))
        (setf bin band-end)))))

(defun %ac3-remove-dithering (d)
  "§7.3.4: a coupled channel whose dither flag is clear does not want the coupling channel's noise.

   Without this a low-rate stereo encode carries dither into both channels regardless, which is
   audible as a raised noise floor and measurable as the one fixture that would not converge."
  (declare (type ac3-decoder d))
  (loop for ch of-type fixnum from 1 to (a3-fbw-channels d)
        do (when (and (zerop (aref (a3-dither-flag d) ch))
                      (plusp (aref (a3-channel-in-cpl d) ch)))
             (let ((coeffs (aref (a3-coeffs d) ch))
                   (cplbap (aref (a3-bap d) +ac3-cpl-ch+)))
               (declare (type (simple-array double-float (*)) coeffs)
                        (type (simple-array fixnum (*)) cplbap))
               (loop for i of-type fixnum from (aref (a3-start-freq d) +ac3-cpl-ch+)
                       below (aref (a3-end-freq d) +ac3-cpl-ch+)
                     do (when (zerop (aref cplbap i)) (setf (aref coeffs i) 0d0)))))))

(defun %ac3-rematrix (d)
  "Undo the 2/0 sum-difference coding (§7.5.2), band by band as the block asked for."
  (declare (type ac3-decoder d))
  (let ((end (min (aref (a3-end-freq d) 1) (aref (a3-end-freq d) 2)))
        (l (aref (a3-coeffs d) 1))
        (r (aref (a3-coeffs d) 2)))
    (declare (type fixnum end) (type (simple-array double-float (*)) l r))
    (dotimes (bnd (a3-num-remat-bands d))
      (when (plusp (aref (a3-remat-flags d) bnd))
        (let ((bstart (aref +ac3-rematrix-band+ bnd))
              (bend (min end (aref +ac3-rematrix-band+ (1+ bnd)))))
          (declare (type fixnum bstart bend))
          (loop for i of-type fixnum from bstart below bend
                do (let ((tmp (aref l i)))
                     (declare (type double-float tmp))
                     (setf (aref l i) (+ tmp (aref r i))
                           (aref r i) (- tmp (aref r i))))))))))

;;; ---- the transform and the lap (§7.9) ------------------------------------------------------------
;;;
;;; A long block is one 512-point inverse MDCT; a short block is two 256-point ones over the even
;;; and odd coefficients.  Either way the result is 512 windowed samples, of which the first 256
;;; overlap what the previous block left behind and the second 256 are kept for the next.  The
;;; window is symmetric, so one 512-entry table serves both halves.
;;;
;;; The scale is -2048/N: the -2/N of the specification's transform equations, times the 2^10 that
;;; reconciles the twenty-four bit mantissa normalisation used here with the level a reference
;;; decoder produces.  That factor was measured rather than derived — the signature of getting it
;;; wrong is unmistakable and worth recording, because it is the same one Vorbis produced in this
;;; repository: correlation near one against the reference with a relative error near one, which is
;;; a pure gain error and nothing else.  Here it was 1024 exactly.
;;;
;;; The minus sign is in the specification and is not a convention that can be absorbed elsewhere:
;;; dropping it inverts every sample, which sounds identical on its own and cancels against
;;; anything else playing.

(defun %ac3-transform-block (d ch out-offset)
  (declare (type ac3-decoder d) (type fixnum ch out-offset))
  (let* ((coeffs (aref (a3-coeffs d) ch))
         (delay (aref (a3-delay d) ch))
         (out (aref (a3-out d) ch))
         (w (the (simple-array double-float (512)) (a3-window d)))
         (y (make-array 512 :element-type 'double-float :initial-element 0d0)))
    (declare (type (simple-array double-float (*)) coeffs delay out y)
             (optimize (speed 3) (safety 1)) (dynamic-extent y))
    (if (plusp (aref (a3-block-switch d) ch))
        ;; two short transforms, interleaved in frequency
        (let ((half (make-array 128 :element-type 'double-float))
              (t1 (make-array 256 :element-type 'double-float))
              (t2 (make-array 256 :element-type 'double-float)))
          (declare (dynamic-extent half t1 t2))
          (dotimes (i 128) (setf (aref half i) (aref coeffs (* 2 i))))
          (%vorbis-imdct-fast half 256 t1)
          (dotimes (i 128) (setf (aref half i) (aref coeffs (1+ (* 2 i)))))
          (%vorbis-imdct-fast half 256 t2)
          (dotimes (i 256) (setf (aref y i) (* -2048d0 (/ 1d0 256) (aref t1 i))))
          (dotimes (i 256) (setf (aref y (+ 256 i)) (* -2048d0 (/ 1d0 256) (aref t2 i)))))
        (let ((tmp (make-array 512 :element-type 'double-float)))
          (declare (dynamic-extent tmp))
          (%vorbis-imdct-fast coeffs 512 tmp)
          (dotimes (i 512) (setf (aref y i) (* -2048d0 (/ 1d0 512) (aref tmp i))))))
    ;; window, overlap with what the last block left, and keep the tail
    (dotimes (i 256)
      (setf (aref out (+ out-offset i)) (+ (* (aref y i) (aref w i)) (aref delay i))))
    (dotimes (i 256)
      (setf (aref delay i) (* (aref y (+ 256 i)) (aref w (+ 256 i)))))))

;;; ---- one audio block (§7.1) -----------------------------------------------------------------------

(defun %ac3-decode-block (d b blk)
  (declare (type ac3-decoder d) (type abits b) (type fixnum blk))
  (let ((fbw (a3-fbw-channels d))
        (acmod (a3-acmod d)))
    (declare (type fixnum fbw acmod))
    ;; block switch and dither flags
    (loop for ch of-type fixnum from 1 to fbw do (setf (aref (a3-block-switch d) ch) (ab1 b)))
    (loop for ch of-type fixnum from 1 to fbw do (setf (aref (a3-dither-flag d) ch) (ab1 b)))
    ;; dynamic range, twice over for dual mono
    (let ((i (if (zerop acmod) 1 0)))
      (declare (type fixnum i))
      (loop
        (if (plusp (ab1 b))
            (let ((bits (ab b 8)))
              (setf (aref (a3-dynamic-range d) i) (%ac3-dynamic-range bits)))
            (when (zerop blk) (setf (aref (a3-dynamic-range d) i) 1d0)))
        (when (minusp (decf i)) (return))))
    ;; ---- coupling strategy
    (when (plusp (ab1 b))
      (setf (a3-cpl-in-use d) (plusp (ab1 b)))
      (when (a3-cpl-in-use d)
        (when (< acmod 2) (ac3-error "coupling in a stream with fewer than two channels"))
        ;; EVERY full-bandwidth channel states whether it is coupled, including both halves of a
        ;; stereo pair.  The reference implementation makes them implicit for stereo, but only for
        ;; E-AC-3 — reading that shortcut as unconditional costs two bits per block and the parse
        ;; drifts from the exponent strategies onward, which is exactly where it looks broken.
        (loop for ch of-type fixnum from 1 to fbw
              do (setf (aref (a3-channel-in-cpl d) ch) (ab1 b)))
        (setf (a3-phase-flags-in-use d) (and (= acmod 2) (plusp (ab1 b))))
        (let* ((start-sub (ab b 4))
               (end-sub (+ 3 (ab b 4))))
          (declare (type fixnum start-sub end-sub))
          (when (>= start-sub end-sub)
            (ac3-error "coupling subband range ~d..~d is empty" start-sub end-sub))
          (setf (aref (a3-start-freq d) +ac3-cpl-ch+) (+ 37 (* 12 start-sub))
                (aref (a3-end-freq d) +ac3-cpl-ch+) (+ 37 (* 12 end-sub)))
          ;; the band structure: each subband either starts a new band or joins the last
          (let ((nbands 1) (size 12))
            (declare (type fixnum nbands size))
            (loop for sub of-type fixnum from (1+ start-sub) below end-sub
                  do (if (plusp (ab1 b))
                         (incf size 12)
                         (progn (setf (aref (a3-cpl-band-sizes d) (1- nbands)) size)
                                (setf size 12)
                                (incf nbands))))
            (setf (aref (a3-cpl-band-sizes d) (1- nbands)) size)
            (setf (a3-num-cpl-bands d) nbands))))
      )
    ;; ---- coupling coordinates
    (when (a3-cpl-in-use d)
      (loop for ch of-type fixnum from 1 to fbw
            do (when (and (plusp (aref (a3-channel-in-cpl d) ch)) (plusp (ab1 b)))
                 (let ((mstrcplco (ab b 2))
                       (coords (aref (a3-cpl-coords d) ch)))
                   (declare (type fixnum mstrcplco)
                            (type (simple-array double-float (*)) coords))
                   (dotimes (band (a3-num-cpl-bands d))
                     (let* ((cplcoexp (ab b 4))
                            (cplcomant (ab b 4))
                            ;; §7.4.3.  The mantissa has an implied leading one — hence the
                            ;; + 16 — except at exponent 15, which is the escape that lets a
                            ;; coordinate go all the way to zero.  Both branches are divided by
                            ;; four rather than by sixteen or thirty-two: getting that wrong
                            ;; scales every coupled band by a constant, so the error is invisible
                            ;; at a high bit rate where little is coupled and enormous at a low
                            ;; one where nearly everything is.
                            (mant (if (= cplcoexp 15)
                                      (/ cplcomant 2d0)
                                      (/ (+ cplcomant 16) 4d0))))
                       (declare (type fixnum cplcoexp cplcomant))
                       (setf (aref coords band)
                             (* mant (expt 2d0 (- (- cplcoexp) (* 3 mstrcplco))))))))))
      (dotimes (band (a3-num-cpl-bands d))
        (setf (aref (a3-phase-flags d) band)
              (if (a3-phase-flags-in-use d) (ab1 b) 0))))
    ;; ---- rematrixing
    (when (= acmod 2)
      (when (plusp (ab1 b))
        (setf (a3-num-remat-bands d) 4)
        ;; A coupling channel that starts low overlaps the rematrixing bands and takes them out
        ;; of play: one band if it starts at or below bin 61, two if it starts at 37.
        (when (and (a3-cpl-in-use d) (<= (aref (a3-start-freq d) +ac3-cpl-ch+) 61))
          (decf (a3-num-remat-bands d)
                (+ 1 (if (= (aref (a3-start-freq d) +ac3-cpl-ch+) 37) 1 0))))
        (dotimes (bnd (a3-num-remat-bands d))
          (setf (aref (a3-remat-flags d) bnd) (ab1 b)))))
    ;; ---- exponent strategies
    (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to (a3-channels d)
          do (setf (aref (a3-exp-strategy d) ch)
                   (ab b (if (= ch (a3-lfe-ch d)) 1 2))))
    ;; ---- channel bandwidth
    (loop for ch of-type fixnum from 1 to fbw
          do (setf (aref (a3-start-freq d) ch) 0)
             (when (/= (aref (a3-exp-strategy d) ch) +exp-reuse+)
               (if (plusp (aref (a3-channel-in-cpl d) ch))
                   (setf (aref (a3-end-freq d) ch) (aref (a3-start-freq d) +ac3-cpl-ch+))
                   (let ((code (ab b 6)))
                     (when (> code 60) (ac3-error "channel bandwidth code ~d is out of range" code))
                     (setf (aref (a3-end-freq d) ch) (+ 73 (* 3 code)))))
               (let ((group-size (ash 3 (1- (aref (a3-exp-strategy d) ch)))))
                 (declare (type fixnum group-size))
                 (setf (aref (a3-num-exp-groups d) ch)
                       (floor (+ (aref (a3-end-freq d) ch) group-size -4) group-size)))))
    (when (and (a3-cpl-in-use d) (/= (aref (a3-exp-strategy d) +ac3-cpl-ch+) +exp-reuse+))
      (setf (aref (a3-num-exp-groups d) +ac3-cpl-ch+)
            (floor (- (aref (a3-end-freq d) +ac3-cpl-ch+)
                      (aref (a3-start-freq d) +ac3-cpl-ch+))
                   (ash 3 (1- (aref (a3-exp-strategy d) +ac3-cpl-ch+))))))
    ;; ---- exponents
    (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to (a3-channels d)
          do (when (/= (aref (a3-exp-strategy d) ch) +exp-reuse+)
               (let* ((dexps (aref (a3-dexps d) ch))
                      (absexp (ash (ab b 4) (if (zerop ch) 1 0))))
                 (declare (type (simple-array fixnum (*)) dexps) (type fixnum absexp))
                 (setf (aref dexps 0) absexp)
                 (%ac3-decode-exponents b (aref (a3-exp-strategy d) ch)
                                        (aref (a3-num-exp-groups d) ch) absexp dexps
                                        (+ (aref (a3-start-freq d) ch) (if (zerop ch) 0 1)))
                 (when (and (/= ch +ac3-cpl-ch+) (/= ch (a3-lfe-ch d)))
                   (ab-skip b 2)))))    ; gainrng, which nothing reads
    ;; ---- bit allocation parameters
    (when (plusp (ab1 b))
      (setf (a3-slow-decay d) (ash (aref +ac3-slow-decay+ (ab b 2)) (- (a3-sr-shift d)))
            (a3-fast-decay d) (ash (aref +ac3-fast-decay+ (ab b 2)) (- (a3-sr-shift d)))
            (a3-slow-gain d) (aref +ac3-slow-gain+ (ab b 2))
            (a3-db-per-bit d) (aref +ac3-db-per-bit+ (ab b 2))
            (a3-floor-val d) (aref +ac3-floor+ (ab b 3))))
    ;; ---- SNR offsets and fast gains
    (when (plusp (ab1 b))
      (let ((csnr (ash (- (ab b 6) 15) 4)))
        (declare (type fixnum csnr))
        (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to (a3-channels d)
              do (setf (aref (a3-snr-offset d) ch) (ash (+ csnr (ab b 4)) 2))
                 (setf (aref (a3-fast-gain d) ch) (aref +ac3-fast-gain+ (ab b 3))))))
    ;; ---- coupling leak
    (when (a3-cpl-in-use d)
      (when (plusp (ab1 b))
        (setf (a3-cpl-fast-leak d) (ab b 3)
              (a3-cpl-slow-leak d) (ab b 3))))
    ;; ---- delta bit allocation
    (if (plusp (ab1 b))
        (progn
          (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to fbw
                do (setf (aref (a3-dba-mode d) ch) (ab b 2))
                   (when (= (aref (a3-dba-mode d) ch) 3)
                     (ac3-error "delta bit allocation mode 3 is reserved")))
          (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to fbw
                do (when (= (aref (a3-dba-mode d) ch) 1)   ; DBA_NEW
                     (setf (aref (a3-dba-nsegs d) ch) (1+ (ab b 3)))
                     (dotimes (seg (aref (a3-dba-nsegs d) ch))
                       (setf (aref (a3-dba-offsets d) ch seg) (ab b 5)
                             (aref (a3-dba-lengths d) ch seg) (ab b 4)
                             (aref (a3-dba-values d) ch seg) (ab b 3))))))
        (when (zerop blk)
          (dotimes (ch +ac3-max-ch+) (setf (aref (a3-dba-mode d) ch) 2))))   ; DBA_NONE
    ;; ---- skipped bytes
    (when (plusp (ab1 b))
      (let ((n (ab b 9))) (ab-skip b (* 8 n))))
    ;; ---- the bit allocation, then the mantissas
    (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to (a3-channels d)
          do (%ac3-bit-alloc d ch))
    (let ((m (make-mant-groups)) (got-cpl nil))
      (loop for ch of-type fixnum from 1 to (a3-channels d)
            do (%ac3-read-channel-coeffs d b ch m
                                         (plusp (aref (a3-dither-flag d) ch)))
               (when (plusp (aref (a3-channel-in-cpl d) ch))
                 (unless got-cpl
                   ;; the coupling channel is ALWAYS dithered, whatever the coupled channels
                   ;; asked for, and the ones that did not ask have it removed again below
                   (%ac3-read-channel-coeffs d b +ac3-cpl-ch+ m t)
                   (%ac3-apply-coupling d)
                   (%ac3-remove-dithering d)
                   (setf got-cpl t)))))
    ;; everything above a channel's own range — or above the coupling range, for a coupled one —
    ;; is silence, and is cleared only now that coupling has had its say
    (loop for ch of-type fixnum from 1 to (a3-channels d)
          do (let ((coeffs (aref (a3-coeffs d) ch))
                   (from (if (plusp (aref (a3-channel-in-cpl d) ch))
                             (aref (a3-end-freq d) +ac3-cpl-ch+)
                             (aref (a3-end-freq d) ch))))
               (declare (type (simple-array double-float (*)) coeffs) (type fixnum from))
               (loop for i of-type fixnum from from below 256 do (setf (aref coeffs i) 0d0))))
    (when *ac3-stats*
      (incf (ast-blocks *ac3-stats*))
      (when (a3-cpl-in-use d) (incf (ast-coupled *ac3-stats*)))
      (when (and (= acmod 2) (plusp (a3-num-remat-bands d)))
        (incf (ast-rematrixed *ac3-stats*)))
      (when (a3-phase-flags-in-use d) (incf (ast-phase-flags *ac3-stats*)))
      (loop for ch of-type fixnum from 1 to fbw
            do (when (plusp (aref (a3-block-switch d) ch))
                 (incf (ast-short-blocks *ac3-stats*)))
               (when (plusp (aref (a3-dither-flag d) ch))
                 (incf (ast-dithered *ac3-stats*))))
      (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to (a3-channels d)
            do (case (aref (a3-exp-strategy d) ch)
                 (0 (incf (ast-exp-reuse *ac3-stats*)))
                 (1 (incf (ast-exp-d15 *ac3-stats*)))
                 (2 (incf (ast-exp-d25 *ac3-stats*)))
                 (3 (incf (ast-exp-d45 *ac3-stats*)))))
      (loop for ch of-type fixnum from (if (a3-cpl-in-use d) 0 1) to fbw
            do (when (= (aref (a3-dba-mode d) ch) 1)
                 (incf (ast-delta-alloc *ac3-stats*)))))
    (when (= acmod 2) (%ac3-rematrix d))
    ;; ---- dynamic range, then out through the transform
    (loop for ch of-type fixnum from 1 to (a3-channels d)
          do (let ((gain (aref (a3-dynamic-range d) (if (and (zerop acmod) (= ch 2)) 1 0)))
                   (coeffs (aref (a3-coeffs d) ch)))
               (declare (type double-float gain)
                        (type (simple-array double-float (*)) coeffs))
               (unless (= gain 1d0)
                 (dotimes (i 256) (setf (aref coeffs i) (* gain (aref coeffs i)))))
               (%ac3-transform-block d ch (* blk 256))))))

;;; ---- channel order (§5.4.2 versus what a player expects) ------------------------------------------
;;;
;;; AC-3 codes 3/2 as L, C, R, Ls, Rs with the LFE last; every player wants L, R, C, LFE, Ls, Rs.
;;; The map below is output position -> coded channel, one-based.  The 2/2 and 3/2 rows are ffmpeg's
;;; verbatim; the rest follow the same rule — the centre channel moves after the pair, and the LFE
;;; lands directly after the centre if there is one and after the front pair if there is not.

(defparameter +ac3-channel-order+
  (vector '(1 2) '(1) '(1 2) '(1 3 2) '(1 2 3) '(1 3 2 4) '(1 2 3 4) '(1 3 2 4 5))
  "By acmod, with no LFE.")
(defparameter +ac3-channel-order-lfe+
  (vector '(1 2 3) '(1 2) '(1 2 3) '(1 3 2 4) '(1 2 4 3) '(1 3 2 5 4) '(1 2 5 3 4) '(1 3 2 6 4 5))
  "By acmod, with the LFE, whose coded index is one past the full-bandwidth channels.")

;;; ---- whole frames ----------------------------------------------------------------------------------

(defun make-ac3-decoder ()
  (let ((d (%make-ac3-decoder)))
    (setf (a3-window d) (%ac3-window))
    d))

(defun %ac3-decode-frame (d octets start)
  "One AC-3 frame beginning at byte START.  Returns its size in bytes."
  (declare (type ac3-decoder d) (type octets octets) (type fixnum start))
  (let ((b (make-abits octets :start start)))
    (let ((size (%ac3-parse-header d b)))
      (declare (type fixnum size))
      (when (> (+ start size) (length octets))
        (ac3-error "a frame of ~d bytes runs past the end of the stream" size))
      ;; a fresh frame starts from silence in the lapping buffers only at the very first one;
      ;; AC-3 has no key frames, so a decoder joining mid-stream is one block behind and no more
      (when *ac3-stats*
        (incf (ast-frames *ac3-stats*))
        (incf (aref (ast-acmods *ac3-stats*) (a3-acmod d)))
        (when (a3-lfe-on d) (incf (ast-lfe-frames *ac3-stats*))))
      (dotimes (blk (a3-num-blocks d))
        (%ac3-decode-block d b blk))
      size)))

(defun decode-ac3 (octets)
  "Decode an AC-3 elementary stream to a REED PCM struct.

   Channels come out in the order a player expects rather than the order AC-3 codes them; no
   downmix is applied, so a 5.1 stream decodes to six channels and what to do with them is the
   caller's decision."
  (declare (type octets octets))
  (let ((d (make-ac3-decoder))
        (pos 0)
        (chunks '())
        (total 0)
        (nch 0)
        (rate 0)
        (order nil))
    ;; find the first sync word: a transport stream hands over whole frames, a file may not
    (loop while (and (< (+ pos 1) (length octets))
                     (not (and (= (aref octets pos) #x0b) (= (aref octets (1+ pos)) #x77))))
          do (incf pos))
    (loop while (< (+ pos 6) (length octets))
          do (let ((size (handler-case (%ac3-decode-frame d octets pos)
                           (ac3-error (e) (if chunks (return) (error e))))))
               (setf rate (a3-sample-rate d)
                     order (if (a3-lfe-on d)
                               (aref +ac3-channel-order-lfe+ (a3-acmod d))
                               (aref +ac3-channel-order+ (a3-acmod d)))
                     nch (length order))
               (let ((n (* 256 (a3-num-blocks d)))
                     (frame (make-array nch)))
                 (loop for out-ch of-type fixnum from 0
                       for coded in order
                       do (setf (aref frame out-ch)
                                (subseq (the (simple-array double-float (*))
                                             (aref (a3-out d) coded))
                                        0 n)))
                 (push frame chunks)
                 (incf total n))
               (incf pos size)))
    (when (null chunks) (ac3-error "no AC-3 frames found"))
    (setf chunks (nreverse chunks))
    (let ((samples (make-pcm16 (* total nch))) (o 0))
      (dolist (frame chunks)
        (let ((n (length (the (simple-array double-float (*)) (aref frame 0)))))
          (dotimes (i n)
            (dotimes (c nch)
              (setf (aref samples o)
                    (clamp16 (round (* 32768d0
                                       (aref (the (simple-array double-float (*)) (aref frame c))
                                             i)))))
              (incf o)))))
      (make-pcm :samples samples :channels nch :sample-rate rate
                :format :pcm16 :frame-count total))))

(defun decode-ac3-file (path)
  "Decode an AC-3 file (.ac3) to PCM."
  (decode-ac3
   (with-open-file (s path :element-type '(unsigned-byte 8))
     (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
       (read-sequence buf s)
       buf))))

;;; ---- one frame at a time, for a container that hands them over --------------------------------

(defun decode-ac3-packet (d packet)
  "One AC-3 frame as a REED PCM struct, in player channel order.

   AC-3 frames are self-describing — the sample rate, the channel configuration and the frame length
   are all in the header — so a container carries no configuration for them and every frame is
   independently decodable.  What is not independent is the lapping: the first frame after a seek is
   missing the previous frame's tail, which is 256 samples of ramp at the front."
  (declare (type ac3-decoder d) (type octets packet))
  (when (< (length packet) 7) (return-from decode-ac3-packet nil))
  (let ((size (%ac3-decode-frame d packet 0)))
    (declare (ignore size))
    (let* ((order (if (a3-lfe-on d)
                      (aref +ac3-channel-order-lfe+ (a3-acmod d))
                      (aref +ac3-channel-order+ (a3-acmod d))))
           (nch (length order))
           (n (* 256 (a3-num-blocks d)))
           (samples (make-pcm16 (* n nch)))
           (o 0))
      (dotimes (i n)
        (loop for coded in order
              do (setf (aref samples o)
                       (clamp16 (round (* 32768d0
                                          (aref (the (simple-array double-float (*))
                                                     (aref (a3-out d) coded))
                                                i)))))
                 (incf o)))
      (make-pcm :samples samples :channels nch :sample-rate (a3-sample-rate d)
                :format :pcm16 :frame-count n))))

(defun ac3-frame-length (buf i)
  "The length in bytes of the AC-3 frame beginning at BUF[I], or NIL if there is not one there.

   A container that hands over a byte stream rather than frames — an MPEG program stream does —
   has to cut it somewhere, and AC-3 has no start code longer than its sixteen-bit sync word.  The
   length comes from two fields right after it, which is enough to walk the stream and confirm each
   candidate by checking that the next frame syncs too."
  (declare (type octets buf) (type fixnum i))
  (when (or (< (+ i 5) 0) (>= (+ i 5) (length buf))) (return-from ac3-frame-length nil))
  (unless (and (= (aref buf i) #x0b) (= (aref buf (1+ i)) #x77))
    (return-from ac3-frame-length nil))
  (let* ((b4 (aref buf (+ i 4)))
         (sr-code (ash b4 -6))
         (frmsizecod (logand b4 #x3f))
         (bsid (ash (aref buf (+ i 5)) -3)))
    (when (or (= sr-code 3) (> frmsizecod 37) (> bsid 10)) (return-from ac3-frame-length nil))
    (* 2 (aref +ac3-frame-size+ frmsizecod sr-code))))
