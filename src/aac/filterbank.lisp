;;;; src/aac/filterbank.lisp --- AAC inverse filterbank: IMDCT (2048 long /
;;;; 256 short), window application (sine/KBD with start/stop transitions),
;;;; and overlap-add.  ISO/IEC 14496-3 clause 4.6.11.
(in-package #:reed)

;;; Window sequence codes (ISO Table 4.53)
(defconstant +wseq-only-long+  0)
(defconstant +wseq-long-start+ 1)
(defconstant +wseq-eight-short+ 2)
(defconstant +wseq-long-stop+  3)

;;; ------------------------------------------------------------------ ;;;
;;; IMDCT via precomputed cosine matrices                               ;;;
;;; ------------------------------------------------------------------ ;;;
;;;   x[n] = (2/N) * sum_{k=0}^{N/2-1} X[k] cos( (2pi/N)(n+n0)(k+1/2) )
;;;   n = 0..N-1,  n0 = N/4 + 1/2.

(defvar *imdct-long*  nil)   ; 2048 x 1024
(defvar *imdct-short* nil)   ; 256  x 128

(defun %make-imdct-matrix (n)
  "Cosine matrix M[n][k] = (2/N) cos((2pi/N)(n+n0)(k+1/2))."
  (let* ((half (ash n -1))
         (m (make-array (list n half) :element-type 'double-float))
         (n0 (+ (/ n 4d0) 0.5d0))
         (w (/ (* 2d0 pi) n))
         (scale (/ 2d0 n)))
    (dotimes (nn n m)
      (dotimes (k half)
        (setf (aref m nn k)
              (* scale (cos (* w (+ nn n0) (+ k 0.5d0)))))))))

(defun aac-imdct-long-matrix ()
  (or *imdct-long* (setf *imdct-long* (%make-imdct-matrix 2048))))
(defun aac-imdct-short-matrix ()
  (or *imdct-short* (setf *imdct-short* (%make-imdct-matrix 256))))

(defun %imdct (spec base half out matrix n)
  "IMDCT of HALF spectral coeffs at SPEC[BASE..] -> OUT[0..N-1]."
  (declare (type (simple-array double-float (*)) spec out)
           (type (simple-array double-float (* *)) matrix)
           (type fixnum base half n))
  (dotimes (nn n)
    (let ((acc 0d0))
      (declare (type double-float acc))
      (dotimes (k half)
        (incf acc (* (aref spec (+ base k)) (aref matrix nn k))))
      (setf (aref out nn) acc))))

;;; ------------------------------------------------------------------ ;;;
;;; Full window assembly for a long-type sequence                       ;;;
;;; ------------------------------------------------------------------ ;;;

(defun %assemble-long-window (wseq kbd-cur kbd-prev)
  "Build the 2048-length window for a non-eight-short WSEQ, given current and
previous window shapes."
  (let ((w (make-array 2048 :element-type 'double-float :initial-element 0d0))
        (lcur (aac-long-window kbd-cur))
        (lprev (aac-long-window kbd-prev))
        (scur (aac-short-window kbd-cur))
        (sprev (aac-short-window kbd-prev)))
    ;; left half (0..1023)
    (cond ((= wseq +wseq-long-stop+)
           ;; 448 zeros, 128 short-prev rising, 448 ones
           (dotimes (i 128) (setf (aref w (+ 448 i)) (aref sprev i)))
           (loop for i from 576 below 1024 do (setf (aref w i) 1d0)))
          (t                              ; only-long / long-start
           (dotimes (i 1024) (setf (aref w i) (aref lprev i)))))
    ;; right half (1024..2047)
    (cond ((= wseq +wseq-long-start+)
           (loop for i from 1024 below 1472 do (setf (aref w i) 1d0))
           (dotimes (i 128) (setf (aref w (+ 1472 i)) (aref scur (+ 128 i))))
           ;; 1600..2047 already zero
           )
          (t                              ; only-long / long-stop
           (dotimes (i 1024) (setf (aref w (+ 1024 i)) (aref lcur (+ 1024 i))))))
    w))

;;; ------------------------------------------------------------------ ;;;
;;; imdct + windowing + overlap-add for one channel                     ;;;
;;; ------------------------------------------------------------------ ;;;
;;;
;;; SPEC holds the 1024 dequantized spectral coefficients (window-major for
;;; eight-short).  SAVED is the 1024-sample overlap buffer from the previous
;;; frame (updated in place).  OUT receives 1024 time-domain samples.

(defun aac-filterbank (spec saved out wseq kbd-cur kbd-prev)
  (declare (type (simple-array double-float (*)) spec saved out))
  (let ((z (make-array 2048 :element-type 'double-float :initial-element 0d0)))
    (if (= wseq +wseq-eight-short+)
        ;; ---- eight short windows, hop 128, base offset 448 ----
        (let ((tmp (make-array 256 :element-type 'double-float))
              (scur (aac-short-window kbd-cur))
              (sprev (aac-short-window kbd-prev))
              (mat (aac-imdct-short-matrix)))
          (dotimes (j 8)
            (%imdct spec (* j 128) 128 tmp mat 256)
            (let ((o (+ 448 (* j 128))))
              (dotimes (n 256)
                ;; subwindow 0's rising half follows the previous shape
                (let ((wv (if (and (= j 0) (< n 128))
                              (aref sprev n)
                              (aref scur n))))
                  (incf (aref z (+ o n)) (* wv (aref tmp n))))))))
        ;; ---- long / start / stop ----
        (let ((x (make-array 2048 :element-type 'double-float))
              (w (%assemble-long-window wseq kbd-cur kbd-prev))
              (mat (aac-imdct-long-matrix)))
          (%imdct spec 0 1024 x mat 2048)
          (dotimes (n 2048) (setf (aref z n) (* (aref w n) (aref x n))))))
    ;; overlap-add: first half + saved; new saved = second half
    (dotimes (n 1024)
      (setf (aref out n) (+ (aref z n) (aref saved n))
            (aref saved n) (aref z (+ 1024 n))))))
