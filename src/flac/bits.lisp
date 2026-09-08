;;;; src/flac/bits.lisp — a bit reader over the whole stream, and the two CRCs that police it.
;;;;
;;;; FLAC reads MSB-first, like everything here except Vorbis.  What it needs that reed's existing
;;;; reader does not offer is width — a sample can be 32 bits — and UNARY, which is how the Rice
;;;; code stores the high part of every residual sample and therefore the single most executed
;;;; operation in the decoder.
;;;;
;;;; The reader addresses the WHOLE FILE rather than a window, because both CRCs are computed over
;;;; byte ranges the parse discovers as it goes: the header CRC covers from the sync code to just
;;;; before itself, and the frame CRC covers everything from the sync code to the end of the
;;;; subframes.  Keeping an absolute bit position makes those ranges a subtraction instead of
;;;; bookkeeping.

(in-package #:reed)

(define-condition flac-error (error)
  ((message :initarg :message :reader flac-error-message))
  (:report (lambda (c s) (format s "~a" (flac-error-message c)))))

(defun flac-error (fmt &rest args)
  (error 'flac-error :message (apply #'format nil fmt args)))

(defstruct (fbits (:conc-name fb-) (:constructor %make-fbits))
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)                  ; absolute bit position
  (nbits 0 :type fixnum))

(defun make-fbits (bytes &key (start 0) (end (length bytes)))
  (declare (type octets bytes))
  (%make-fbits :bytes bytes :pos (* 8 start) :nbits (* 8 end)))

(declaim (inline fb-left fb-byte-pos fb-aligned-p))
(defun fb-left (b) (- (fb-nbits b) (fb-pos b)))
(defun fb-byte-pos (b) (ash (fb-pos b) -3))
(defun fb-aligned-p (b) (zerop (logand (fb-pos b) 7)))

(defun fb-align (b)
  "Skip to the next byte boundary, which is where a frame's CRC-16 begins."
  (setf (fb-pos b) (ash (ash (+ (fb-pos b) 7) -3) 3)))

(defun fb (b n)
  "N bits, MSB first.  Running past the end is always an error in FLAC: unlike Vorbis, a frame
   states its own length in samples and a truncated one is a truncated file."
  (declare (type fbits b) (type (integer 0 32) n) (optimize (speed 3) (safety 1)))
  (when (zerop n) (return-from fb 0))
  (let ((pos (fb-pos b)) (bytes (fb-bytes b)))
    (declare (type fixnum pos))
    (when (> (+ pos n) (fb-nbits b))
      (flac-error "the stream ends inside a value"))
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
      (setf (fb-pos b) pos)
      acc)))

(declaim (inline fb1))
(defun fb1 (b) (fb b 1))

(defun fb-signed (b n)
  "N bits as a two's-complement signed integer."
  (declare (type fbits b) (type (integer 0 32) n))
  (if (zerop n)
      0
      (let ((x (fb b n)))
        (if (logbitp (1- n) x) (- x (ash 1 n)) x))))

(defun fb-unary (b)
  "Zero bits counted up to and including the terminating one — the high half of every Rice code
   word, and the busiest operation in the decoder."
  (declare (type fbits b) (optimize (speed 3) (safety 1)))
  (let ((pos (fb-pos b)) (nbits (fb-nbits b)) (bytes (fb-bytes b)) (n 0))
    (declare (type fixnum pos nbits n))
    (loop
      (when (>= pos nbits) (flac-error "the stream ends inside a Rice code word"))
      (let* ((byte (ash pos -3))
             (bit (logand pos 7))
             (rest (logand (aref bytes byte) (1- (ash 1 (- 8 bit))))))
        (declare (type fixnum byte bit rest))
        (if (zerop rest)
            (progn (incf n (- 8 bit)) (setf pos (ash (1+ byte) 3)))
            ;; the first one bit in this byte ends the run
            (let ((lead (- (- 8 bit) (integer-length rest))))
              (declare (type fixnum lead))
              (incf n lead)
              (setf pos (+ pos lead 1))
              (setf (fb-pos b) pos)
              (return-from fb-unary n)))))))

(defun fb-utf8 (b)
  "FLAC's frame or sample number: UTF-8's byte structure used to code a number rather than a
   character, and extended to seven octets (§9.1.5).  A general purpose UTF-8 decoder will not do."
  (declare (type fbits b))
  (let ((first (fb b 8)))
    (declare (type (unsigned-byte 8) first))
    (when (< first #x80) (return-from fb-utf8 first))
    (when (< first #xc0) (flac-error "coded number starts with a continuation octet"))
    (let ((extra (cond ((< first #xe0) 1) ((< first #xf0) 2) ((< first #xf8) 3)
                       ((< first #xfc) 4) ((< first #xfe) 5) ((< first #xff) 6)
                       (t (flac-error "coded number starts with #xff"))))
          (mask (cond ((< first #xe0) #x1f) ((< first #xf0) #x0f) ((< first #xf8) #x07)
                      ((< first #xfc) #x03) ((< first #xfe) #x01) (t #x00))))
      (let ((v (logand first mask)))
        (dotimes (i extra v)
          (let ((c (fb b 8)))
            (unless (= (logand c #xc0) #x80)
              (flac-error "coded number has a malformed continuation octet"))
            (setf v (logior (ash v 6) (logand c #x3f)))))))))

;;; ---- the two CRCs ---------------------------------------------------------------------------
;;;
;;; FLAC checks itself twice per frame: an 8-bit CRC over the header and a 16-bit CRC over the whole
;;; frame including that header.  They are not decoration.  A decoder that desynchronises by one bit
;;; anywhere in a subframe still produces plausible-looking samples, and the CRC is what turns that
;;; into a stated error rather than quiet noise — the same role the "consumes its partition exactly"
;;; check plays for VP9 in the sibling repository.

(defun %crc-table (poly width)
  (let ((tab (make-array 256 :element-type '(unsigned-byte 16)))
        (top (ash 1 (1- width)))
        (mask (1- (ash 1 width))))
    (dotimes (i 256 tab)
      (let ((c (logand (ash i (- width 8)) mask)))
        (dotimes (j 8)
          (setf c (logand (if (logtest c top) (logxor (ash c 1) poly) (ash c 1)) mask)))
        (setf (aref tab i) c)))))

(defparameter +crc8-table+ (%crc-table #x07 8)
  "x^8 + x^2 + x + 1, over the frame header (§9.1.8).")
(defparameter +crc16-table+ (%crc-table #x8005 16)
  "x^16 + x^15 + x^2 + 1, over the whole frame (§9.3).")
(declaim (type (simple-array (unsigned-byte 16) (256)) +crc8-table+ +crc16-table+))

(defun %crc8 (bytes start end)
  (declare (type octets bytes) (type fixnum start end) (optimize (speed 3) (safety 1)))
  (let ((c 0))
    (declare (type (unsigned-byte 8) c))
    (loop for i of-type fixnum from start below end
          do (setf c (aref +crc8-table+ (logxor c (aref bytes i)))))
    c))

(defun %crc16 (bytes start end)
  (declare (type octets bytes) (type fixnum start end) (optimize (speed 3) (safety 1)))
  (let ((c 0))
    (declare (type (unsigned-byte 16) c))
    (loop for i of-type fixnum from start below end
          do (setf c (logand (logxor (ash c 8) (aref +crc16-table+ (logxor (ash c -8) (aref bytes i))))
                             #xffff)))
    c))
