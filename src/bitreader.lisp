;;;; src/bitreader.lisp — an MSB-first bit reader over a byte window.
;;;;
;;;; Used both for the frame side-information and for the assembled main-data
;;;; bit reservoir.  Bit position 0 is the MSB of the first byte of the window.
(in-package #:reed)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))
(deftype octets-u16 () '(simple-array (unsigned-byte 16) (*)))

(defstruct (bitreader (:constructor %make-bitreader))
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (base  0 :type fixnum)   ; byte index in BYTES where the window begins
  (pos   0 :type fixnum)   ; current bit position, relative to the window start
  (nbits 0 :type fixnum))  ; number of bits in the window (for bounds checks)

(declaim (inline make-bitreader br-byte))
(defun make-bitreader (bytes &key (start 0) (end (length bytes)))
  "Create a bit reader over BYTES[START,END)."
  (declare (type octets bytes) (type fixnum start end))
  (%make-bitreader :bytes bytes :base start :pos 0 :nbits (* 8 (- end start))))

(defun br-byte (br i)
  "Byte I of the window (0 past the end — the reservoir/side-info is padded)."
  (declare (type bitreader br) (type fixnum i))
  (let ((idx (+ (bitreader-base br) i))
        (bytes (bitreader-bytes br)))
    (declare (type fixnum idx))
    (if (< -1 idx (length bytes)) (aref bytes idx) 0)))

(declaim (inline br-pos br-set-pos br-remaining))
(defun br-pos (br) (bitreader-pos br))
(defun br-set-pos (br p) (setf (bitreader-pos br) p))
(defun br-remaining (br) (- (bitreader-nbits br) (bitreader-pos br)))

(defun read-bits (br n)
  "Read N (0..25) bits MSB-first; result right-justified in the return value."
  (declare (type bitreader br) (type (integer 0 25) n))
  (if (zerop n)
      0
      (let* ((pos (bitreader-pos br))
             (byte0 (ash pos -3))
             (bit (logand pos 7))
             ;; assemble a 32-bit window of up to four bytes
             (w (logior (ash (br-byte br byte0) 24)
                        (ash (br-byte br (+ byte0 1)) 16)
                        (ash (br-byte br (+ byte0 2)) 8)
                        (br-byte br (+ byte0 3)))))
        (declare (type fixnum pos byte0 bit)
                 (type (unsigned-byte 32) w))
        (setf (bitreader-pos br) (+ pos n))
        ;; the N desired bits sit at MSB offset BIT; right-justify them
        (logand (ash w (- (+ bit n) 32))
                (1- (ash 1 n))))))

(declaim (inline read-bit))
(defun read-bit (br)
  (declare (type bitreader br))
  (let* ((pos (bitreader-pos br))
         (b (br-byte br (ash pos -3))))
    (setf (bitreader-pos br) (1+ pos))
    (logand (ash b (- (logand pos 7) 7)) 1)))
