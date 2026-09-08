;;;; src/flac/decode.lisp — FLAC, which is the only codec here that can be checked without an oracle.
;;;;
;;;; FLAC is LOSSLESS, and that changes what a test can prove.  Every other decoder in reed is
;;;; asserted against ffmpeg at a correlation or an RMS bound, because a lossy float codec has no
;;;; single right answer to the last bit.  This one has exactly one right answer, and the format
;;;; carries it: STREAMINFO holds the MD5 of the unencoded audio, so a decode can be checked against
;;;; the encoder's own record of what went in, with no reference decoder involved at all.
;;;;
;;;; THE MODEL IS PREDICTION PLUS RESIDUAL, and nothing else.  There is no transform, no
;;;; psychoacoustics and no quantisation: each subframe predicts its samples from the ones before —
;;;; either with one of five fixed polynomial predictors or with up to 32 learned coefficients — and
;;;; codes what the prediction got wrong.  Everything expensive about a lossy codec is absent, which
;;;; is why the whole decoder is two files.

(in-package #:reed)

;;; COVERAGE, not statistics.  Subframe types, wasted bits, escaped partitions and the three stereo
;;; modes are paths a fixture either exercises or does not, and a corpus re-encoded some day could
;;; stop covering one without any test failing.  The suite counts them and asserts on the counts.
;;; A special variable rather than a threaded argument because it is diagnostic scaffolding and
;;; should cost nothing and clutter nothing when nobody is looking.

(defstruct (flac-stats (:conc-name fst-))
  (frames 0 :type fixnum)
  (constant 0 :type fixnum) (verbatim 0 :type fixnum)
  (fixed 0 :type fixnum) (lpc 0 :type fixnum)
  (wasted 0 :type fixnum) (escaped 0 :type fixnum) (rice2 0 :type fixnum)
  (independent 0 :type fixnum) (left-side 0 :type fixnum)
  (side-right 0 :type fixnum) (mid-side 0 :type fixnum)
  (max-lpc-order 0 :type fixnum) (max-partition-order 0 :type fixnum))

(defvar *flac-stats* nil "Bind to a FLAC-STATS to count which paths a decode takes.")

(defstruct (flac-info (:conc-name fi-))
  (min-block 0 :type fixnum) (max-block 0 :type fixnum)
  (min-frame 0 :type fixnum) (max-frame 0 :type fixnum)
  (sample-rate 0 :type fixnum)
  (channels 0 :type fixnum)
  (bits 0 :type fixnum)
  (total-samples 0 :type integer)
  (md5 nil))                            ; 16 octets, or NIL when the encoder left it zero

;;; ---- metadata ------------------------------------------------------------------------------------

(defun parse-flac-metadata (b)
  "Read `fLaC' and the metadata blocks, leaving B at the first frame.  Returns the STREAMINFO."
  (declare (type fbits b))
  (unless (= (fb b 32) #x664c6143)      ; "fLaC"
    (flac-error "not a FLAC stream: it does not begin with `fLaC'"))
  (let ((info nil))
    (loop
      (let* ((last (plusp (fb1 b)))
             (type (fb b 7))
             (len (fb b 24)))
        (cond
          ((= type 0)
           (unless (= len 34) (flac-error "STREAMINFO is ~d bytes, not 34" len))
           (let* ((min-block (fb b 16)) (max-block (fb b 16))
                  (min-frame (fb b 24)) (max-frame (fb b 24))
                  (rate (fb b 20)) (channels (1+ (fb b 3))) (bits (1+ (fb b 5)))
                  (total (logior (ash (fb b 4) 32) (fb b 32)))
                  (md5 (make-array 16 :element-type '(unsigned-byte 8))))
             (dotimes (i 16) (setf (aref md5 i) (fb b 8)))
             (setf info (make-flac-info :min-block min-block :max-block max-block
                                        :min-frame min-frame :max-frame max-frame
                                        :sample-rate rate :channels channels :bits bits
                                        :total-samples total
                                        :md5 (unless (every #'zerop md5) md5)))))
          ((= type 127) (flac-error "a metadata block of the forbidden type 127"))
          (t (incf (fb-pos b) (* 8 len))))
        (when last (return))))
    (unless info (flac-error "the stream has no STREAMINFO block"))
    (when (zerop (fi-channels info)) (flac-error "a stream with no channels"))
    (unless (<= 4 (fi-bits info) 32)
      (flac-error "~d bits per sample is outside what FLAC allows" (fi-bits info)))
    info))

;;; ---- the frame header ----------------------------------------------------------------------------

(defparameter +flac-rates+
  #(0 88200 176400 192000 8000 16000 22050 24000 32000 44100 48000 96000 0 0 0 0))
(defparameter +flac-depths+ #(0 8 12 0 16 20 24 32))

(defstruct (flac-frame (:conc-name ff-))
  (block-size 0 :type fixnum)
  (sample-rate 0 :type fixnum)
  (channels 0 :type fixnum)             ; how many subframes there are
  (assignment 0 :type fixnum)           ; 0-7 independent, 8 left/side, 9 side/right, 10 mid/side
  (bits 0 :type fixnum)
  (number 0 :type integer))

(defun %read-frame-header (b info)
  "One frame header, with its CRC-8 checked (§9.1)."
  (declare (type fbits b) (type flac-info info))
  (let ((start (fb-byte-pos b)))
    (let ((sync (fb b 15)))
      (unless (= sync #x7ffc) (flac-error "frame sync is #x~4,'0x, not #x7ffc" sync)))
    (let* ((variable (plusp (fb1 b)))
           (bs-code (fb b 4))
           (sr-code (fb b 4))
           (ch-code (fb b 4))
           (bd-code (fb b 3))
           (reserved (fb1 b)))
      (declare (ignore variable))
      (unless (zerop reserved) (flac-error "the frame header's reserved bit is set"))
      (when (zerop bs-code) (flac-error "block size code 0 is reserved"))
      (when (= sr-code 15) (flac-error "sample rate code 15 is forbidden"))
      (when (> ch-code 10) (flac-error "channel assignment ~d is reserved" ch-code))
      (when (= bd-code 3) (flac-error "bit depth code 3 is reserved"))
      (let* ((number (fb-utf8 b))
             (block-size (cond ((= bs-code 1) 192)
                               ((<= 2 bs-code 5) (* 144 (ash 1 bs-code)))
                               ((= bs-code 6) (1+ (fb b 8)))
                               ((= bs-code 7) (1+ (fb b 16)))
                               (t (ash 1 bs-code))))
             (rate (cond ((zerop sr-code) (fi-sample-rate info))
                         ((<= sr-code 11) (aref +flac-rates+ sr-code))
                         ((= sr-code 12) (* 1000 (fb b 8)))
                         ((= sr-code 13) (fb b 16))
                         (t (* 10 (fb b 16)))))
             (bits (if (zerop bd-code) (fi-bits info) (aref +flac-depths+ bd-code)))
             (channels (if (>= ch-code 8) 2 (1+ ch-code)))
             (crc-at (fb-byte-pos b))
             (crc (fb b 8)))
        (unless (fb-aligned-p b) (flac-error "the frame header did not end byte aligned"))
        (let ((want (%crc8 (fb-bytes b) start crc-at)))
          (unless (= crc want)
            (flac-error "frame header CRC is #x~2,'0x, computed #x~2,'0x" crc want)))
        (values (make-flac-frame :block-size block-size :sample-rate rate :channels channels
                                 :assignment ch-code :bits bits :number number)
                start)))))

;;; ---- the residual ---------------------------------------------------------------------------------

(defun %read-residual (b out block-size order)
  "The coded residual for one subframe, written into OUT from index ORDER (§9.2.7)."
  (declare (type fbits b) (type (simple-array fixnum (*)) out) (type fixnum block-size order)
           (optimize (speed 3) (safety 1)))
  (let* ((method (fb b 2))
         (pbits (case method (0 4) (1 5) (t (flac-error "residual coding method ~d is reserved" method))))
         (escape (1- (ash 1 pbits)))
         (porder (fb b 4))
         (nparts (ash 1 porder))
         (i order))
    (declare (type fixnum method pbits escape porder nparts i))
    (when *flac-stats*
      (when (= method 1) (incf (fst-rice2 *flac-stats*)))
      (setf (fst-max-partition-order *flac-stats*)
            (max (fst-max-partition-order *flac-stats*) porder)))
    (unless (zerop (mod block-size nparts))
      (flac-error "block size ~d does not divide into ~d partitions" block-size nparts))
    (when (<= (ash block-size (- porder)) order)
      (flac-error "partition order ~d leaves no room for a predictor of order ~d" porder order))
    (dotimes (p nparts out)
      (let* ((count (- (ash block-size (- porder)) (if (zerop p) order 0)))
             (param (fb b pbits)))
        (declare (type fixnum count param))
        (if (= param escape)
            ;; an escaped partition stores its samples raw, and may store them in no bits at all
            (let ((width (fb b 5)))
              (declare (type fixnum width))
              (when *flac-stats* (incf (fst-escaped *flac-stats*)))
              (dotimes (k count) (setf (aref out i) (fb-signed b width)) (incf i)))
            (dotimes (k count)
              (let* ((q (fb-unary b))
                     (folded (logior (ash q param) (fb b param))))
                (declare (type fixnum q folded))
                (let ((v (if (oddp folded) (- (- (ash folded -1)) 1) (ash folded -1))))
                  (declare (type fixnum v))
                  ;; §9.2.7.3: every residual sample must fit a 32-bit signed integer.  On a
                  ;; corrupt stream the unary run is the first thing to run away, and checking here
                  ;; turns "a number no predictor could produce" into a stated error rather than
                  ;; letting it propagate into the prediction and overflow something further on.
                  (unless (typep v '(signed-byte 32))
                    (flac-error "a residual sample of ~d does not fit the 32 bits the format allows"
                                v))
                  (setf (aref out i) v))
                (incf i))))))))

;;; ---- subframes ------------------------------------------------------------------------------------

(defun %read-subframe (b out block-size bits)
  "One subframe into OUT, which must hold BLOCK-SIZE samples (§9.2)."
  (declare (type fbits b) (type (simple-array fixnum (*)) out) (type fixnum block-size bits)
           (optimize (speed 3) (safety 1)))
  (unless (zerop (fb1 b)) (flac-error "a subframe header does not begin with a zero bit"))
  (let* ((type (fb b 6))
         (wasted (if (plusp (fb1 b)) (1+ (fb-unary b)) 0))
         (bps (- bits wasted)))
    (declare (type fixnum type wasted bps))
    (unless (plusp bps) (flac-error "a subframe with ~d wasted bits has no bits left" wasted))
    (when (and *flac-stats* (plusp wasted)) (incf (fst-wasted *flac-stats*)))
    (cond
      ((zerop type)                     ; constant
       (when *flac-stats* (incf (fst-constant *flac-stats*)))
       (let ((v (fb-signed b bps))) (dotimes (i block-size) (setf (aref out i) v))))
      ((= type 1)                       ; verbatim
       (when *flac-stats* (incf (fst-verbatim *flac-stats*)))
       (dotimes (i block-size) (setf (aref out i) (fb-signed b bps))))
      ((<= 8 type 12)                   ; fixed predictor, order type-8
       (when *flac-stats* (incf (fst-fixed *flac-stats*)))
       (let ((order (- type 8)))
         (declare (type fixnum order))
         (dotimes (i order) (setf (aref out i) (fb-signed b bps)))
         (%read-residual b out block-size order)
         (%fixed-predict out block-size order)))
      ((>= type 32)                     ; linear predictor, order type-31
       (let* ((order (- type 31))
              (coeffs (make-array order :element-type 'fixnum)))
         (declare (type fixnum order))
         (when *flac-stats*
           (incf (fst-lpc *flac-stats*))
           (setf (fst-max-lpc-order *flac-stats*)
                 (max (fst-max-lpc-order *flac-stats*) order)))
         (dotimes (i order) (setf (aref out i) (fb-signed b bps)))
         (let ((precision (1+ (fb b 4)))
               (shift 0))
           (declare (type fixnum precision))
           (when (= precision 16) (flac-error "predictor coefficient precision 0b1111 is forbidden"))
           (setf shift (fb-signed b 5))
           (when (minusp shift) (flac-error "a negative predictor right shift (~d)" shift))
           (dotimes (i order) (setf (aref coeffs i) (fb-signed b precision)))
           (%read-residual b out block-size order)
           (%lpc-predict out block-size order coeffs shift))))
      (t (flac-error "subframe type ~d is reserved" type)))
    (when (plusp wasted)
      (dotimes (i block-size) (setf (aref out i) (ash (aref out i) wasted))))
    out))

(defun %fixed-predict (out n order)
  "Add back one of the five fixed polynomial predictions (§9.2.5)."
  (declare (type (simple-array fixnum (*)) out) (type fixnum n order)
           (optimize (speed 3) (safety 1)))
  (ecase order
    (0 nil)
    (1 (loop for i of-type fixnum from 1 below n
             do (incf (aref out i) (aref out (- i 1)))))
    (2 (loop for i of-type fixnum from 2 below n
             do (incf (aref out i) (- (* 2 (aref out (- i 1))) (aref out (- i 2))))))
    (3 (loop for i of-type fixnum from 3 below n
             do (incf (aref out i) (+ (* 3 (aref out (- i 1)))
                                      (* -3 (aref out (- i 2)))
                                      (aref out (- i 3))))))
    (4 (loop for i of-type fixnum from 4 below n
             do (incf (aref out i) (+ (* 4 (aref out (- i 1)))
                                      (* -6 (aref out (- i 2)))
                                      (* 4 (aref out (- i 3)))
                                      (- (aref out (- i 4))))))))
  out)

(defun %lpc-predict (out n order coeffs shift)
  "Add back the learned prediction (§9.2.6).

   THE COEFFICIENTS RUN BACKWARDS IN TIME: the first multiplies the sample immediately before the
   one being predicted, the second the one before that.  The sum is shifted right ARITHMETICALLY,
   which for a negative sum is not the same as dividing."
  (declare (type (simple-array fixnum (*)) out coeffs) (type fixnum n order shift)
           (optimize (speed 3) (safety 1)))
  (loop for i of-type fixnum from order below n
        do (let ((sum 0))
             (declare (type fixnum sum))
             (dotimes (j order)
               (incf sum (* (aref coeffs j) (aref out (- i j 1)))))
             (incf (aref out i) (ash sum (- shift)))))
  out)

;;; ---- a whole frame ---------------------------------------------------------------------------------

(defun %decode-flac-frame (b info channels)
  "One frame into CHANNELS (a vector of fixnum arrays), returning its block size.

   The frame's CRC-16 is checked before anything is believed."
  (declare (type fbits b) (type flac-info info) (type simple-vector channels))
  (multiple-value-bind (f start) (%read-frame-header b info)
    (let* ((n (ff-block-size f))
           (nch (ff-channels f))
           (assign (ff-assignment f))
           (bits (ff-bits f)))
      (when (> nch (length channels))
        (flac-error "a frame with ~d channels in a stream that declared ~d"
                    nch (length channels)))
      (dotimes (c nch)
        (let ((buf (aref channels c)))
          (when (< (length (the (simple-array fixnum (*)) buf)) n)
            (setf buf (make-array n :element-type 'fixnum) (aref channels c) buf))
          ;; the side channel of a decorrelated pair carries one extra bit, because a difference
          ;; can be twice as large as either of the things differenced
          (%read-subframe b buf n
                          (+ bits (if (or (and (= assign 8) (= c 1))
                                          (and (= assign 9) (= c 0))
                                          (and (= assign 10) (= c 1)))
                                      1 0)))))
      (fb-align b)
      (let* ((end (fb-byte-pos b))
             (crc (fb b 16))
             (want (%crc16 (fb-bytes b) start end)))
        (unless (= crc want)
          (flac-error "frame ~d CRC is #x~4,'0x, computed #x~4,'0x — the stream or this decoder is wrong"
                      (ff-number f) crc want)))
      (when *flac-stats*
        (incf (fst-frames *flac-stats*))
        (case assign
          (8 (incf (fst-left-side *flac-stats*)))
          (9 (incf (fst-side-right *flac-stats*)))
          (10 (incf (fst-mid-side *flac-stats*)))
          (t (incf (fst-independent *flac-stats*)))))
      ;; ---- undo the stereo decorrelation (§4.2)
      (let ((a (and (>= nch 1) (aref channels 0)))
            (s (and (>= nch 2) (aref channels 1))))
        (declare (type (or null (simple-array fixnum (*))) a s))
        (case assign
          (8 (dotimes (i n) (setf (aref s i) (- (aref a i) (aref s i)))))          ; left/side
          (9 (dotimes (i n) (setf (aref a i) (+ (aref a i) (aref s i)))))          ; side/right
          (10 (dotimes (i n)                                                        ; mid/side
                (let* ((side (aref s i))
                       (mid (logior (ash (aref a i) 1) (logand side 1))))
                  (setf (aref a i) (ash (+ mid side) -1)
                        (aref s i) (ash (- mid side) -1)))))))
      (values n nch (ff-sample-rate f)))))

;;; ---- whole streams -----------------------------------------------------------------------------------

(defun decode-flac-raw (octets)
  "Decode every frame.  Returns (values channel-arrays total-samples info) with the samples at
   their native bit depth — which is what the STREAMINFO MD5 is computed over."
  (declare (type octets octets))
  (let* ((b (make-fbits octets))
         (info (parse-flac-metadata b))
         (nch (fi-channels info))
         (scratch (let ((v (make-array nch)))
                    (dotimes (i nch v)
                      (setf (aref v i) (make-array (max 1 (fi-max-block info))
                                                   :element-type 'fixnum)))))
         (out (let ((v (make-array nch)))
                (dotimes (i nch v)
                  (setf (aref v i) (make-array (max 1 (fi-total-samples info))
                                               :element-type 'fixnum
                                               :adjustable t :fill-pointer 0)))))
         (total 0)
         (nframes 0))
    (loop while (>= (fb-left b) 40)     ; a frame header cannot be shorter than five bytes
          do (multiple-value-bind (n frame-ch)
                 ;; A DECODER EATS UNTRUSTED INPUT.  Corrupt bits can produce a predictor order, a
                 ;; coefficient or a residual that is individually legal and jointly absurd, and the
                 ;; arithmetic downstream then fails in whatever way it fails.  The frame CRC would
                 ;; have caught it, but only after the subframes were decoded — so anything that
                 ;; goes wrong before that point is reported as what it is: a broken frame.
                 (handler-case (%decode-flac-frame b info scratch)
                   (flac-error (e) (error e))
                   (error (e)
                     (flac-error "frame ~d did not decode: ~a — the stream is corrupt"
                                 nframes e)))
               (declare (ignore frame-ch))
               (dotimes (c nch)
                 (let ((src (aref scratch c)) (dst (aref out c)))
                   (dotimes (i n) (vector-push-extend (aref src i) dst))))
               (incf total n)
               (incf nframes)))
    (values (map 'simple-vector
                 (lambda (v) (make-array (length v) :element-type 'fixnum :initial-contents v))
                 out)
            total info)))

(defun decode-flac (octets)
  "Decode a FLAC stream to a REED PCM struct.

   Sixteen-bit audio comes out exactly as it went in.  Deeper audio is shifted down to sixteen bits,
   which is the only lossy step anywhere in this decoder and is the caller's PCM type asking for it —
   DECODE-FLAC-RAW returns the native-depth samples."
  (declare (type octets octets))
  (multiple-value-bind (channels total info) (decode-flac-raw octets)
    (let* ((nch (fi-channels info))
           (bits (fi-bits info))
           (shift (- bits 16))
           (samples (make-pcm16 (* total nch)))
           (o 0))
      (dotimes (i total)
        (dotimes (c nch)
          (let ((v (aref (the (simple-array fixnum (*)) (aref channels c)) i)))
            (setf (aref samples o)
                  (clamp16 (cond ((plusp shift) (ash v (- shift)))
                                 ((minusp shift) (ash v (- shift)))
                                 (t v))))
            (incf o))))
      (make-pcm :samples samples :channels nch :sample-rate (fi-sample-rate info)
                :format :pcm16 :frame-count total))))

(defun decode-flac-file (path)
  "Decode a .flac file to PCM."
  (decode-flac
   (with-open-file (s path :element-type '(unsigned-byte 8))
     (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
       (read-sequence buf s)
       buf))))

;;; ---- one frame at a time, for a container that hands them over -------------------------------
;;;
;;; Matroska carries the native FLAC header — `fLaC' and its metadata blocks — in CodecPrivate, and
;;; then one FLAC frame per block.  So the same parser reads the configuration, and the frame loop
;;; is driven by the container instead of by the byte after the last frame.

(defstruct (flac-decoder (:conc-name fd-) (:constructor %make-flac-decoder))
  (info nil :type (or null flac-info))
  (scratch #() :type simple-vector))

(defun make-flac-decoder (info)
  (let ((nch (fi-channels info)))
    (%make-flac-decoder
     :info info
     :scratch (let ((v (make-array nch)))
                (dotimes (i nch v)
                  (setf (aref v i) (make-array (max 1 (fi-max-block info))
                                               :element-type 'fixnum)))))))

(defun make-flac-decoder-for-header (private)
  "A decoder from a container's copy of the native FLAC header (Matroska CodecPrivate)."
  (declare (type octets private))
  (make-flac-decoder (parse-flac-metadata (make-fbits private))))

(defun decode-flac-packet (d packet)
  "One FLAC frame as a REED PCM struct.  Unlike a lapped transform, every frame stands alone: FLAC
   predicts only within a subframe, so a packet is decodable on its own and seeking is exact."
  (declare (type flac-decoder d) (type octets packet))
  (when (zerop (length packet)) (return-from decode-flac-packet nil))
  (let* ((info (fd-info d))
         (b (make-fbits packet))
         (scratch (fd-scratch d)))
    (multiple-value-bind (n nch rate) (%decode-flac-frame b info scratch)
      (let* ((bits (fi-bits info))
             (shift (- bits 16))
             (samples (make-pcm16 (* n nch)))
             (o 0))
        (dotimes (i n)
          (dotimes (c nch)
            (let ((v (aref (the (simple-array fixnum (*)) (aref scratch c)) i)))
              (setf (aref samples o) (clamp16 (if (zerop shift) v (ash v (- shift)))))
              (incf o))))
        (make-pcm :samples samples :channels nch :sample-rate rate
                  :format :pcm16 :frame-count n)))))
