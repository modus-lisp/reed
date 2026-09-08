;;;; src/vorbis/bits.lisp — Vorbis reads its bits the other way round.
;;;;
;;;; EVERY OTHER CODEC IN THIS LIBRARY IS MSB-FIRST.  Vorbis is not: the first bit of a packet is
;;;; the LEAST significant bit of the first byte, and a multi-bit field is assembled with the first
;;;; bit read as its least significant (Vorbis I §2.1.2).  That is not a detail that can be papered
;;;; over by reversing bytes somewhere — it changes what "the next four bits" means — so this reader
;;;; exists rather than reusing REED's.
;;;;
;;;; RUNNING OFF THE END OF A PACKET IS NOT ALWAYS AN ERROR.  In the headers it is: a truncated
;;;; setup packet is a broken stream.  In an audio packet it is the ordinary way a residue partition
;;;; ends — the encoder simply stops writing once the rest would be zero, and §1.3.2 says the
;;;; decoder returns what it has.  So the reader does not signal; it sets EOP, returns zeros, and
;;;; the caller decides which of the two situations it is in.

(in-package #:reed)

(define-condition vorbis-error (error)
  ((message :initarg :message :reader vorbis-error-message))
  (:report (lambda (c s) (format s "~a" (vorbis-error-message c)))))

(defun vorbis-error (fmt &rest args)
  (error 'vorbis-error :message (apply #'format nil fmt args)))

(defstruct (vbits (:conc-name vb-) (:constructor %make-vbits))
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)                  ; bit position from the start of the packet
  (nbits 0 :type fixnum)
  (eop nil))                            ; has a read run past the end?

(defun make-vbits (bytes &key (start 0) (end (length bytes)))
  (declare (type octets bytes) (type fixnum start end))
  (%make-vbits :bytes (if (and (zerop start) (= end (length bytes)))
                          bytes
                          (subseq bytes start end))
               :nbits (* 8 (- end start))))

(declaim (inline vb-tell vb-left))
(defun vb-tell (v) (vb-pos v))
(defun vb-left (v) (- (vb-nbits v) (vb-pos v)))

(defun vb (v n)
  "Read N bits, first bit read is the LEAST significant of the result.

   Past the end of the packet this returns zero and latches EOP, which is what §1.3.2 asks for."
  (declare (type vbits v) (type (integer 0 32) n) (optimize (speed 3) (safety 1)))
  (let ((pos (vb-pos v)) (nbits (vb-nbits v)) (bytes (vb-bytes v)))
    (declare (type fixnum pos nbits))
    (when (> (+ pos n) nbits)
      (setf (vb-eop v) t (vb-pos v) nbits)
      (return-from vb 0))
    (let ((acc 0) (got 0))
      (declare (type (unsigned-byte 40) acc) (type fixnum got))
      (loop while (< got n)
            do (let* ((byte (ash pos -3))
                      (bit (logand pos 7))
                      (take (min (- 8 bit) (- n got))))
                 (declare (type fixnum byte bit take))
                 (setf acc (logior acc (ash (logand (ash (aref bytes byte) (- bit))
                                                    (1- (ash 1 take)))
                                            got)))
                 (incf got take)
                 (incf pos take)))
      (setf (vb-pos v) pos)
      acc)))

(declaim (inline vb1))
(defun vb1 (v) (vb v 1))

(defun vb-signed (v n)
  "N bits as a two's-complement signed integer (§2.1.5)."
  (declare (type vbits v) (type (integer 1 32) n))
  (let ((x (vb v n)))
    (if (logbitp (1- n) x) (- x (ash 1 n)) x)))

(defun ilog (x)
  "§9.2.1: the position of the highest set bit, counting from one; zero for anything not positive."
  (declare (type integer x))
  (if (plusp x) (integer-length x) 0))

(defun float32-unpack (x)
  "§9.2.2.  NOT an IEEE 754 float: Vorbis packs a 21-bit mantissa, a sign, and a 10-bit exponent
   biased by 788, and the value is mantissa * 2^exponent — no implicit leading one, no infinities,
   no NaN.  Reading it as an IEEE single gives a number that is wrong by an enormous factor rather
   than slightly wrong, which is at least easy to notice."
  (declare (type (unsigned-byte 32) x))
  (let* ((mantissa (logand x #x1fffff))
         (sign (logbitp 31 x))
         (exponent (ash (logand x #x7fe00000) -21)))
    (when sign (setf mantissa (- mantissa)))
    (* mantissa (expt 2d0 (- exponent 788)))))

(defun lookup1-values (entries dimensions)
  "§9.2.3: the largest integer whose DIMENSIONS-th power is not greater than ENTRIES.

   Computed by search rather than by ROUND of a root, because the spec means the exact integer and
   a floating-point root lands on the wrong side of it for entry counts that are exact powers."
  (declare (type (integer 0 #.(ash 1 24)) entries) (type (integer 1 65535) dimensions))
  (let ((r 0))
    (loop while (<= (expt (1+ r) dimensions) entries) do (incf r))
    r))
