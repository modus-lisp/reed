;;;; src/requantize.lisp — scalefactor-band tables and requantization.
(in-package #:reed)

(deftype samples () '(simple-array double-float (576)))

;;; ---- scalefactor band index tables ----------------------------------------
;;; Index by FH-SFBAND-INDEX: 0-2 MPEG-1 (44100/48000/32000), 3-5 MPEG-2
;;; (22050/24000/16000), 6-8 MPEG-2.5 (11025/12000/8000).  LONG has 23 boundary
;;; entries (21 bands), SHORT has 14 (12 bands); short entries are per-window and
;;; are multiplied by 3 for the interleaved index.
(defun %iv (&rest xs) (make-array (length xs) :element-type 'fixnum :initial-contents xs))

(defparameter +sfband-long+
  (vector
   (%iv 0 4 8 12 16 20 24 30 36 44 52 62 74 90 110 134 162 196 238 288 342 418 576)
   (%iv 0 4 8 12 16 20 24 30 36 42 50 60 72 88 106 128 156 190 230 276 330 384 576)
   (%iv 0 4 8 12 16 20 24 30 36 44 54 66 82 102 126 156 194 240 296 364 448 550 576)
   ;; MPEG-2 (LSF)
   (%iv 0 6 12 18 24 30 36 44 54 66 80 96 116 140 168 200 238 284 336 396 464 522 576)
   (%iv 0 6 12 18 24 30 36 44 54 66 80 96 114 136 162 194 232 278 332 394 464 540 576)
   (%iv 0 6 12 18 24 30 36 44 54 66 80 96 116 140 168 200 238 284 336 396 464 522 576)
   ;; MPEG-2.5
   (%iv 0 6 12 18 24 30 36 44 54 66 80 96 116 140 168 200 238 284 336 396 464 522 576)
   (%iv 0 6 12 18 24 30 36 44 54 66 80 96 116 140 168 200 238 284 336 396 464 522 576)
   (%iv 0 12 24 36 48 60 72 88 108 132 160 192 232 280 336 400 476 566 568 570 572 574 576)))

(defparameter +sfband-short+
  (vector
   (%iv 0 4 8 12 16 22 30 40 52 66 84 106 136 192)
   (%iv 0 4 8 12 16 22 28 38 50 64 80 100 126 192)
   (%iv 0 4 8 12 16 22 30 42 58 78 104 138 180 192)
   ;; MPEG-2
   (%iv 0 4 8 12 18 24 32 42 56 74 100 132 174 192)
   (%iv 0 4 8 12 18 26 36 48 62 80 104 136 180 192)
   (%iv 0 4 8 12 18 26 36 48 62 80 104 134 174 192)
   ;; MPEG-2.5
   (%iv 0 4 8 12 18 26 36 48 62 80 104 134 174 192)
   (%iv 0 4 8 12 18 26 36 48 62 80 104 134 174 192)
   (%iv 0 8 16 24 36 52 72 96 124 160 162 164 166 192)))

(declaim (inline sfb-long sfb-short))
(defun sfb-long (idx) (aref +sfband-long+ idx))
(defun sfb-short (idx) (aref +sfband-short+ idx))

;;; ---- alias reduction / intensity-stereo coefficients ----------------------
(defparameter +cs+ (make-array 8 :element-type 'double-float :initial-contents
  '(0.857492926d0 0.881741997d0 0.949628649d0 0.983314592d0
    0.995517816d0 0.999160558d0 0.999899195d0 0.999993155d0)))
(defparameter +ca+ (make-array 8 :element-type 'double-float :initial-contents
  '(-0.514495755d0 -0.471731969d0 -0.313377454d0 -0.181913200d0
    -0.094574193d0 -0.040965583d0 -0.014198569d0 -0.003699975d0)))
(defparameter +is-ratios+ (make-array 6 :element-type 'double-float :initial-contents
  '(0.0d0 0.267949192d0 0.577350269d0 1.0d0 1.732050808d0 3.732050808d0)))
(defparameter +pretab+ (make-array 22 :element-type 'fixnum :initial-contents
  '(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 2 2 3 3 3 2 0)))

;;; ---- x^(4/3) power table --------------------------------------------------
(declaim (type (simple-array double-float (8207)) +pow43+))
(defparameter +pow43+
  (let ((a (make-array 8207 :element-type 'double-float)))
    (dotimes (i 8207 a)
      (setf (aref a i) (expt (coerce i 'double-float) 4/3)))))

(declaim (inline pow43))
(defun pow43 (is-pos)
  (declare (type fixnum is-pos))
  (aref +pow43+ (min is-pos 8206)))

;;; ---- requantization -------------------------------------------------------
(defun requantize (is scl scs si header gr ch)
  "Requantize the 576 quantized lines in IS (double-float, holding signed ints)
in place, using scalefactors SCL/SCS and side info SI for granule GR channel CH."
  (declare (type samples is) (type fixnum gr ch)
           (optimize (speed 3) (safety 1)))
  (let* ((sfbi (fh-sfband-index header))
         (long-tab (sfb-long sfbi))
         (short-tab (sfb-short sfbi))
         (count1 (aref (si-count1 si) gr ch))
         (gg (aref (si-global-gain si) gr ch))
         (sf-mult (if (zerop (aref (si-scalefac-scale si) gr ch)) 0.5d0 1.0d0))
         (preflag (aref (si-preflag si) gr ch))
         (gain2 (expt 2.0d0 (* 0.25d0 (- (coerce gg 'double-float) 210.0d0))))
         (short-p (and (= 1 (aref (si-win-switch si) gr ch))
                       (= 2 (aref (si-block-type si) gr ch))))
         (mixed-p (= 1 (aref (si-mixed si) gr ch))))
    (declare (type fixnum count1 gg preflag)
             (type double-float sf-mult gain2)
             (type (simple-array fixnum (*)) long-tab short-tab))
    (labels ((deq (i tmp1 sbgain)
               (declare (type fixnum i) (type double-float tmp1 sbgain))
               (let* ((v (aref is i))
                      (iv (the fixnum (round v)))
                      (mag (pow43 (abs iv)))
                      (tmp3 (if (minusp iv) (- mag) mag)))
                 (setf (aref is i) (* tmp1 gain2 sbgain tmp3))))
             (long-band (i sfb)
               (declare (type fixnum i sfb))
               (let ((tmp1 (expt 2.0d0 (- (* sf-mult
                                             (+ (aref scl gr ch sfb)
                                                (* preflag (aref +pretab+ sfb))))))))
                 (deq i tmp1 1.0d0)))
             (short-band (i sfb win)
               (declare (type fixnum i sfb win))
               (let ((tmp1 (expt 2.0d0 (- (* sf-mult (aref scs gr ch sfb win))))))
                 (deq i tmp1 (expt 2.0d0 (* -2.0d0
                                            (coerce (aref (si-subblock-gain si) gr ch win)
                                                    'double-float)))))))
      (cond
        (short-p
         (when mixed-p
           ;; first two subbands (36 lines) are long
           (let ((sfb 0) (next (aref long-tab 1)))
             (declare (type fixnum sfb next))
             (dotimes (i 36)
               (when (= i next) (incf sfb) (setf next (aref long-tab (1+ sfb))))
               (long-band i sfb))))
         ;; short bands
         (let* ((start (if mixed-p 36 0))
                (sfb (if mixed-p 3 0))
                (win-len (- (aref short-tab (1+ sfb)) (aref short-tab sfb)))
                (next (* 3 (aref short-tab (1+ sfb))))
                (i start))
           (declare (type fixnum start sfb win-len next i))
           (loop while (< i count1) do
             (when (= i next)
               (incf sfb)
               (setf win-len (- (aref short-tab (1+ sfb)) (aref short-tab sfb))
                     next (* 3 (aref short-tab (1+ sfb)))))
             (dotimes (win 3)
               (dotimes (j win-len)
                 (short-band i sfb win)
                 (incf i))))))
        (t
         (let ((sfb 0) (next (aref long-tab 1)))
           (declare (type fixnum sfb next))
           (dotimes (i count1)
             (when (= i next) (incf sfb) (setf next (aref long-tab (1+ sfb))))
             (long-band i sfb))))))))
