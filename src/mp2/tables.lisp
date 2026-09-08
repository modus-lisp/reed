;;;; mp2/tables.lisp — GENERATED.  The constant tables of ISO/IEC 11172-3 Layer II.
;;;;
;;;; Layer II is a subband coder and its tables say, for every subband, how many bits the bit
;;;; ALLOCATION field takes and what each allocation value means.  There are three distinct tables
;;;; and five ways of choosing between them, by bit rate and sampling rate — because a low-rate
;;;; encode cannot afford to describe the top subbands at all.
;;;;
;;;; Extracted mechanically and checked against the one property that ties them together: walking
;;;; each table by its own field widths must produce exactly as many subbands as the limit table
;;;; says it has.  A transcription error almost always breaks that walk.

(in-package #:reed)

(defparameter +mp2-sblimit+
  (make-array 5 :element-type 'fixnum :initial-contents '(27 30 8 12 30))
  "How many of the 32 subbands each table describes.  The rest are silent.")

(defparameter +mp2-quant-steps+
  (make-array 17 :element-type 'fixnum :initial-contents
   '(
        3      5      7      9     15     31
       63    127    255    511   1023   2047
     4095   8191  16383  32767  65535))
  "The number of levels each quantiser class has.  Never a power of two, which is the point: three
   levels in five bits beats two bits each.")

(defparameter +mp2-quant-bits+
  (make-array 17 :element-type 'fixnum :initial-contents
   '(
     -5   -7    3  -10    4    5
      6    7    8    9   10   11
     12   13   14   15   16))
  "How many bits a sample of that class takes, NEGATED when three samples share one code.")

;;; The allocation tables themselves.  Each is a flat walk: a field width, then that many entries
;;; naming a quantiser class, then the next subband.
(defparameter +mp2-alloc-a+
  (make-array 300 :element-type 'fixnum :initial-contents
   '(
     4   0   2   4   5   6   7   8   9  10  11  12  13  14  15  16
     4   0   2   4   5   6   7   8   9  10  11  12  13  14  15  16
     4   0   2   4   5   6   7   8   9  10  11  12  13  14  15  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     3   0   1   2   3   4   5  16   3   0   1   2   3   4   5  16
     2   0   1  16   2   0   1  16   2   0   1  16   2   0   1  16
     2   0   1  16   2   0   1  16   2   0   1  16)))

(defparameter +mp2-alloc-b+
  (make-array 112 :element-type 'fixnum :initial-contents
   '(
     4   0   1   3   4   5   6   7   8   9  10  11  12  13  14  15
     4   0   1   3   4   5   6   7   8   9  10  11  12  13  14  15
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7)))

(defparameter +mp2-alloc-c+
  (make-array 196 :element-type 'fixnum :initial-contents
   '(
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     4   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   3   0   1   3   4   5   6   7
     3   0   1   3   4   5   6   7   2   0   1   3   2   0   1   3
     2   0   1   3   2   0   1   3   2   0   1   3   2   0   1   3
     2   0   1   3   2   0   1   3   2   0   1   3   2   0   1   3
     2   0   1   3   2   0   1   3   2   0   1   3   2   0   1   3
     2   0   1   3   2   0   1   3   2   0   1   3   2   0   1   3
     2   0   1   3)))

(defparameter +mp2-alloc-tables+
  (vector +mp2-alloc-a+ +mp2-alloc-a+ +mp2-alloc-b+ +mp2-alloc-b+ +mp2-alloc-c+)
  "Which table each of the five selections uses.  Two pairs share a table and differ only in how
   many subbands of it they read.")

(defparameter +mp2-bitrate-mpeg1+
  (make-array 15 :element-type 'fixnum :initial-contents
   '(0 32000 48000 56000 64000 80000 96000 112000 128000 160000 192000 224000 256000 320000
     384000))
  "Layer II's bit rates, in BITS per second because that is what the frame length is computed from.
   They are NOT Layer III's — 384 kbit/s exists here and 40 does not.")

(defparameter +mp2-scalefactor+
  (let ((a (make-array 64 :element-type 'double-float)))
    (dotimes (i 64 a) (setf (aref a i) (* 2d0 (expt 2d0 (/ i -3d0))))))
  "Table 3-B.1: two, halving every three steps.  Computed rather than transcribed because that is
   exactly what the specification says it is.")

(declaim (type (simple-array fixnum (5)) +mp2-sblimit+))
(declaim (type (simple-array fixnum (17)) +mp2-quant-steps+ +mp2-quant-bits+))
(declaim (type (simple-array double-float (64)) +mp2-scalefactor+))
