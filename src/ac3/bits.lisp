;;;; src/ac3/bits.lisp — AC-3's bit reader, and the frame it lives in.
;;;;
;;;; MSB-first, like everything here except Vorbis.  AC-3 never reads more than sixteen bits at once
;;;; and never needs a unary code, so this is the plainest reader in the library — but it is its own
;;;; reader rather than a shared one because the condition it signals is what a caller catches to
;;;; find out that a stream is not decodable, and that has to name AC-3.

(in-package #:reed)

(define-condition ac3-error (error)
  ((message :initarg :message :reader ac3-error-message))
  (:report (lambda (c s) (format s "~a" (ac3-error-message c)))))

(defun ac3-error (fmt &rest args)
  (error 'ac3-error :message (apply #'format nil fmt args)))

(defstruct (abits (:conc-name ab-) (:constructor %make-abits))
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (nbits 0 :type fixnum))

(defun make-abits (bytes &key (start 0) (end (length bytes)))
  (declare (type octets bytes))
  (%make-abits :bytes bytes :pos (* 8 start) :nbits (* 8 end)))

(declaim (inline ab-left))
(defun ab-left (b) (- (ab-nbits b) (ab-pos b)))

(defun ab (b n)
  "N bits, MSB first."
  (declare (type abits b) (type (integer 0 32) n) (optimize (speed 3) (safety 1)))
  (when (zerop n) (return-from ab 0))
  (let ((pos (ab-pos b)) (bytes (ab-bytes b)))
    (declare (type fixnum pos))
    (when (> (+ pos n) (ab-nbits b))
      (ac3-error "the frame ends inside a value"))
    (let ((acc 0) (got 0))
      (declare (type (unsigned-byte 40) acc) (type fixnum got))
      (loop while (< got n)
            do (let* ((byte (ash pos -3))
                      (bit (logand pos 7))
                      (take (min (- 8 bit) (- n got))))
                 (declare (type fixnum byte bit take))
                 (setf acc (logior (ash acc take)
                                   (logand (ash (aref bytes byte) (- (- 8 bit take)))
                                           (1- (ash 1 take)))))
                 (incf got take)
                 (incf pos take)))
      (setf (ab-pos b) pos)
      acc)))

(declaim (inline ab1))
(defun ab1 (b) (ab b 1))

(defun ab-signed (b n)
  (declare (type abits b) (type (integer 1 32) n))
  (let ((x (ab b n)))
    (if (logbitp (1- n) x) (- x (ash 1 n)) x)))

(defun ab-skip (b n)
  (declare (type abits b) (type fixnum n))
  (when (> (+ (ab-pos b) n) (ab-nbits b)) (ac3-error "the frame ends inside a skipped field"))
  (incf (ab-pos b) n))
