;;;; src/opus/mdct.lisp — CELT inverse MDCT (clt_mdct_backward) in pure CL.
;;;;
;;;; Ported from libopus 1.4 celt/mdct.c (clt_mdct_backward_c).  The kiss_fft
;;;; bitrev / in-place post-rotation tricks are algebraically eliminated: the
;;;; pre-rotation writes the complex buffer F in natural order, a forward FFT is
;;;; run (exp(-2j*pi*kn/N), no scaling), and the post-rotation de-interleaves
;;;; both ends of the output.  All DSP is double-float.
(in-package #:reed)

;;; ------------------------------------------------------------------
;;; Mixed-radix (recursive Cooley-Tukey) forward complex FFT.
;;;
;;; Handles any composite N; for CELT we only ever see N4 in
;;; {60,120,240,480} = 2^a*3*5, but the algorithm is general.  Forward
;;; transform: X[k] = sum_n x[n] exp(-2j*pi*k*n/N), no 1/N scaling.
;;; ------------------------------------------------------------------

(declaim (type hash-table *mdct-fft-exp-cache*))
(defparameter *mdct-fft-exp-cache* (make-hash-table)
  "Caches, per transform size N, the vector exp(-2j*pi*j/N) for j in 0..N-1.")

(defun %mdct-fft-exp-table (n)
  "Return (simple-array (complex double-float) (n)) of exp(-2j*pi*j/N)."
  (declare (type fixnum n))
  (or (gethash n *mdct-fft-exp-cache*)
      (setf (gethash n *mdct-fft-exp-cache*)
            (let ((tbl (make-array n :element-type '(complex double-float))))
              (dotimes (j n tbl)
                (let ((ang (/ (* -2d0 pi (float j 1d0)) (float n 1d0))))
                  (setf (aref tbl j)
                        (complex (cos ang) (sin ang)))))))))

(defun %mdct-smallest-factor (n)
  "Smallest prime factor of N (>1); returns N itself if prime."
  (declare (type fixnum n))
  (when (evenp n) (return-from %mdct-smallest-factor 2))
  (let ((d 3))
    (declare (type fixnum d))
    (loop while (<= (* d d) n) do
      (when (zerop (mod n d)) (return-from %mdct-smallest-factor d))
      (incf d 2))
    n))

(defun %mdct-fft (x n)
  "Recursive mixed-radix forward FFT of the length-N complex vector X.
Returns a fresh (simple-array (complex double-float) (n))."
  (declare (type fixnum n)
           (type (simple-array (complex double-float) (*)) x))
  (when (= n 1)
    (let ((r (make-array 1 :element-type '(complex double-float))))
      (setf (aref r 0) (aref x 0))
      (return-from %mdct-fft r)))
  (let* ((p (%mdct-smallest-factor n))
         (m (truncate n p))
         (subs (make-array p))
         (table (%mdct-fft-exp-table n))
         (result (make-array n :element-type '(complex double-float))))
    (declare (type fixnum p m)
             (type (simple-array (complex double-float) (*)) table result))
    ;; Decimation in time: gather the p subsequences x[b], x[b+p], ... and
    ;; recursively transform each (size m).
    (dotimes (b p)
      (let ((xb (make-array m :element-type '(complex double-float))))
        (dotimes (a m)
          (setf (aref xb a) (aref x (+ (* p a) b))))
        (setf (aref subs b) (%mdct-fft xb m))))
    ;; Combine: X[k] = sum_b W_N^{b*k} * Y_b[k mod m].
    (dotimes (k n result)
      (let ((kk (mod k m))
            (acc #C(0d0 0d0)))
        (declare (type (complex double-float) acc))
        (dotimes (b p)
          (let ((yb (aref subs b)))
            (declare (type (simple-array (complex double-float) (*)) yb))
            (incf acc (* (aref table (mod (* b k) n))
                         (aref yb kk)))))
        (setf (aref result k) acc)))))

;;; ------------------------------------------------------------------
;;; Trig table cache: trig[i] = cos(2*pi*(i+0.125)/Nshift) for i in 0..N2-1.
;;; ------------------------------------------------------------------

(declaim (type hash-table *mdct-trig-cache*))
(defparameter *mdct-trig-cache* (make-hash-table)
  "Caches the CELT MDCT trig table keyed by Nshift.")

(defun %mdct-trig-table (nshift)
  "Return (simple-array double-float (nshift/2)) with trig[i]=cos(2pi(i+.125)/Nshift)."
  (declare (type fixnum nshift))
  (or (gethash nshift *mdct-trig-cache*)
      (setf (gethash nshift *mdct-trig-cache*)
            (let* ((n2 (ash nshift -1))
                   (tbl (make-array n2 :element-type 'double-float)))
              (dotimes (i n2 tbl)
                (setf (aref tbl i)
                      (cos (/ (* 2d0 pi (+ (float i 1d0) 0.125d0))
                              (float nshift 1d0)))))))))

;;; ------------------------------------------------------------------
;;; Overlap window (CELT).  window[i]=sin(.5*pi*(sin(.5*pi*(i+.5)/overlap))^2).
;;; ------------------------------------------------------------------

(defconstant +celt-overlap+ 120)

(defun celt-mdct-window ()
  "Return the CELT overlap window as (simple-array double-float (120))."
  (let* ((overlap +celt-overlap+)
         (w (make-array overlap :element-type 'double-float)))
    (dotimes (i overlap w)
      (let ((s (sin (* 0.5d0 pi (/ (+ (float i 1d0) 0.5d0)
                                   (float overlap 1d0))))))
        (setf (aref w i) (sin (* 0.5d0 pi s s)))))))

;;; ------------------------------------------------------------------
;;; clt_mdct_backward
;;; ------------------------------------------------------------------

(defun clt-mdct-backward (in in-off out out-off nfull overlap shift stride window)
  "Inverse CELT MDCT.  IN is a spectral (simple-array double-float) read with
STRIDE starting at IN-OFF; OUT is the time-domain (simple-array double-float)
buffer written at OUT-OFF.  Nshift = NFULL >> SHIFT.  WINDOW is the length-120
overlap window.  OUT[out-off .. out-off+overlap/2-1] must hold the previous
block's tail on entry (it is folded by the TDAC step, not zeroed)."
  (declare (type (simple-array double-float (*)) in out window)
           (type fixnum in-off out-off nfull overlap shift stride)
           (optimize (speed 2) (safety 1)))
  (let* ((nshift (ash nfull (- shift)))
         (n2 (ash nshift -1))
         (n4 (ash nshift -2))
         (trig (%mdct-trig-table nshift))
         (base (+ out-off (ash overlap -1)))
         (f (make-array n4 :element-type '(complex double-float))))
    (declare (type fixnum nshift n2 n4 base)
             (type (simple-array double-float (*)) trig)
             (type (simple-array (complex double-float) (*)) f))
    ;; --- pre-rotate into F (natural order) ---
    (dotimes (i n4)
      (let* ((x1 (aref in (+ in-off (* stride (* 2 i)))))
             (x2 (aref in (+ in-off (* stride (- n2 1 (* 2 i))))))
             (ti (aref trig i))
             (tn (aref trig (+ n4 i)))
             (fr (+ (* x2 ti) (* x1 tn)))    ; yr
             (fi (- (* x1 ti) (* x2 tn))))   ; yi
        (setf (aref f i) (complex fi fr))))
    ;; --- forward FFT (no scaling) ---
    (let ((g (%mdct-fft f n4)))
      (declare (type (simple-array (complex double-float) (*)) g))
      ;; --- post-rotate / de-shuffle ---
      (dotimes (i n4)
        (let* ((gi (aref g i))
               (re (imagpart gi))
               (im (realpart gi))
               (ti (aref trig i))
               (tn (aref trig (+ n4 i)))
               (yr (+ (* re ti) (* im tn)))
               (yi (- (* re tn) (* im ti))))
          (setf (aref out (+ base (* 2 i))) yr)
          (setf (aref out (+ base (- n2 1 (* 2 i)))) yi))))
    ;; --- TDAC mirror windowing over out[out-off .. out-off+overlap-1] ---
    (let ((a out-off))
      (dotimes (i (ash overlap -1))
        (let* ((x1 (aref out (+ a (- overlap 1 i))))    ; post-rotate tail
               (x2 (aref out (+ a i)))                  ; previous-block content
               (wi (aref window i))
               (wo (aref window (- overlap 1 i))))
          (setf (aref out (+ a i)) (- (* wo x2) (* wi x1)))
          (setf (aref out (+ a (- overlap 1 i))) (+ (* wi x2) (* wo x1))))))
    out))
