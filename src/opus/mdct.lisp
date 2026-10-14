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

;;; ------------------------------------------------------------------
;;; Split-array FFT (unscaled forward DFT, mixed radix by decimation in
;;; time) on separate real/imaginary double-float vectors, with Cooley-Tukey
;;; butterflies: the twiddled terms t_b = W_N^{bk} Y_b[k] are formed ONCE per k
;;; and combined into all p outputs X[k + m q] by a small p-point DFT (a plain
;;; butterfly for p = 2).  It replaced a (complex double-float) version that
;;; allocated per recursion level and recomputed every product for every
;;; output; on a Lisp without unboxed complex arrays each of its elements was a
;;; boxed object, and modus spent most of an Opus decode building and
;;; dispatching them.  Output is byte-identical on the conformance vectors.
;;; ------------------------------------------------------------------

(declaim (type hash-table *mdct-twiddle-cache*))
(defparameter *mdct-twiddle-cache* (make-hash-table)
  "Per transform size N, (cos . sin) vectors of exp(-2 pi i j / N) for j < N.")

(defun %mdct-twiddles (n)
  (declare (type fixnum n))
  (or (gethash n *mdct-twiddle-cache*)
      (setf (gethash n *mdct-twiddle-cache*)
            (let ((c (make-array n :element-type 'double-float))
                  (sn (make-array n :element-type 'double-float)))
              (dotimes (j n (cons c sn))
                (let ((ang (/ (* -2d0 pi (float j 1d0)) (float n 1d0))))
                  (setf (aref c j) (cos ang) (aref sn j) (sin ang))))))))

(defun %fft-split (xr xi x0 xs n yr yi y0 tw-c tw-s tw-n)
  "Forward DFT of the N points X[x0 + xs*j] (j < N) of XR/XI into YR/YI at
Y0..Y0+N-1.  TW-C/TW-S are the twiddles of size TW-N (a multiple of N)."
  (declare (type (simple-array double-float (*)) xr xi yr yi tw-c tw-s)
           (type fixnum x0 xs n y0 tw-n)
           (optimize (speed 2) (safety 1)))
  (if (= n 1)
      (setf (aref yr y0) (aref xr x0) (aref yi y0) (aref xi x0))
      (let* ((p (%mdct-smallest-factor n))
             (m (truncate n p))
             (step (truncate tw-n n)))
        (declare (type fixnum p m step))
        ;; Y_b = DFT of the b-th decimated subsequence, into Y[y0 + b*m ...].
        (dotimes (b p)
          (%fft-split xr xi (+ x0 (* xs b)) (* xs p) m yr yi (+ y0 (* b m))
                      tw-c tw-s tw-n))
        (if (= p 2)
            ;; radix 2: X[k] = Y0 + W^k Y1, X[k+m] = Y0 - W^k Y1
            (dotimes (k m)
              (let* ((i0 (+ y0 k)) (i1 (+ i0 m))
                     (wi (* step k))
                     (wr (aref tw-c wi)) (ws (aref tw-s wi))
                     (br (aref yr i1)) (bi (aref yi i1))
                     (tr (- (* wr br) (* ws bi)))
                     (ti (+ (* wr bi) (* ws br)))
                     (ar (aref yr i0)) (ai (aref yi i0)))
                (declare (type fixnum i0 i1 wi)
                         (type double-float wr ws br bi tr ti ar ai))
                (setf (aref yr i0) (+ ar tr) (aref yi i0) (+ ai ti)
                      (aref yr i1) (- ar tr) (aref yi i1) (- ai ti))))
            ;; radix p: t_b = W_N^{bk} Y_b[k], then X[k + m q] = sum_b t_b W_p^{bq}
            (let ((tr (make-array p :element-type 'double-float))
                  (ti (make-array p :element-type 'double-float)))
              (dotimes (k m)
                (dotimes (b p)
                  (let* ((ib (+ y0 (* b m) k))
                         (wi (mod (* step b k) tw-n))
                         (wr (aref tw-c wi)) (ws (aref tw-s wi))
                         (vr (aref yr ib)) (vi (aref yi ib)))
                    (declare (type fixnum ib wi) (type double-float wr ws vr vi))
                    (setf (aref tr b) (- (* wr vr) (* ws vi))
                          (aref ti b) (+ (* wr vi) (* ws vr)))))
                (dotimes (q p)
                  (let ((sr 0d0) (si 0d0))
                    (declare (type double-float sr si))
                    (dotimes (b p)
                      (let* ((wi (mod (* step m b q) tw-n))
                             (wr (aref tw-c wi)) (ws (aref tw-s wi))
                             (vr (aref tr b)) (vi (aref ti b)))
                        (declare (type fixnum wi) (type double-float wr ws vr vi))
                        (setf sr (+ sr (- (* wr vr) (* ws vi)))
                              si (+ si (+ (* wr vi) (* ws vr))))))
                    (setf (aref yr (+ y0 k (* m q))) sr
                          (aref yi (+ y0 k (* m q))) si)))))))))

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
         (fr (make-array n4 :element-type 'double-float))
         (fi (make-array n4 :element-type 'double-float))
         (gr (make-array n4 :element-type 'double-float))
         (gi (make-array n4 :element-type 'double-float))
         (tw (%mdct-twiddles n4)))
    (declare (type fixnum nshift n2 n4 base)
             (type (simple-array double-float (*)) trig fr fi gr gi))
    ;; --- pre-rotate into F (natural order) ---
    (dotimes (i n4)
      (let* ((x1 (aref in (+ in-off (* stride (* 2 i)))))
             (x2 (aref in (+ in-off (* stride (- n2 1 (* 2 i))))))
             (ti (aref trig i))
             (tn (aref trig (+ n4 i)))
             (yr (+ (* x2 ti) (* x1 tn)))
             (yi (- (* x1 ti) (* x2 tn))))
        ;; the complex point is (yi + i yr): real part yi, imaginary part yr
        (setf (aref fr i) yi (aref fi i) yr)))
    ;; --- forward FFT (no scaling) ---
    (%fft-split fr fi 0 1 n4 gr gi 0 (car tw) (cdr tw) n4)
    (progn
      ;; --- post-rotate / de-shuffle ---
      (dotimes (i n4)
        (let* ((re (aref gi i))
               (im (aref gr i))
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
