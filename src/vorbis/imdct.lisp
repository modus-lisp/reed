;;;; src/vorbis/imdct.lisp — the inverse MDCT, in n log n instead of n squared.
;;;;
;;;; The direct sum in decode.lisp is the definition and stays there, because this file is only
;;;; trustworthy against something that is obviously right.  The test asserts the two agree to
;;;; 1e-9 on random spectra at every block size Vorbis allows.
;;;;
;;;; THE DERIVATION, because the constants are otherwise unguessable.  Write the transform as
;;;;
;;;;     y[i] = SUM_k X[k] cos( pi (2i+1+n/2)(2k+1) / 2n )
;;;;
;;;; and substitute j = i + n/4.  Then 2i+1+n/2 becomes 2j+1 and the whole thing is
;;;;
;;;;     Z[j] = SUM_k X[k] cos( pi (2j+1)(2k+1) / 2n )
;;;;
;;;; which is a DCT-IV of size M = n/2 exactly — its denominator is 4M — and y[i] = Z[i + n/4].
;;;; The indices run past the DCT's own range, and the kernel's two symmetries cover it:
;;;; Z[2M-1-j] = -Z[j] and Z[j+n] = -Z[j], both by cos(pi(2k+1)) = -1.  So one DCT-IV of size n/2
;;;; and a rearrangement with sign flips is the whole transform.
;;;;
;;;; The DCT-IV itself becomes a COMPLEX FFT OF SIZE M/2.  Pair the input as u[p] = x[2p] +
;;;; i*x[M-1-2p], which covers the evens ascending and the odds descending, and note that
;;;;
;;;;     cos(theta (2j+1)(2M-4p-1)) = (-1)^j sin(theta (2j+1)(4p+1)),   theta = pi/4M
;;;;
;;;; so the cosine and sine halves are the real and imaginary parts of one complex product.  For
;;;; even j the phase is e^-i(phi) and for odd j it is e^+i(phi); expanding (4q+1)(4p+1) splits the
;;;; phase into a pre-twiddle in p, the FFT kernel 2*pi*pq/(M/2), and a post-twiddle in q, with the
;;;; leftover constant halved between the two ends.  What falls out is
;;;;
;;;;     Z[2q] = Re(S[q]),   Z[M-1-2q] = -Im(S[q])
;;;;
;;;; and the odd case is the even one turned by i, because e^{i(pi/2)(4p+1)} is i for every p.

(in-package #:reed)

;;; ---- the definition, kept ------------------------------------------------------------------------
;;;
;;; A DIRECT SUM, not a fast one.  It is no longer what decodes a file, and it is not dead code: it
;;; is what the fast transform below is tested against, at every block size the format allows, and
;;; it is what any future rewrite will be tested against too.  Deleting it would leave the fast one
;;; checkable only against ffmpeg, end to end, where a transform bug and a residue bug look alike.  N/2 coefficients times N outputs is a million multiply-adds for a
;;; 2048-sample block, which is far more than an FFT-based MDCT would cost — but it is the
;;; definition, written the way the definition is written, and a fast MDCT is a thing to check
;;; against something known to be right rather than a thing to start with.
;;;
;;; THE NAME IS QUALIFIED BECAUSE REED IS ONE FLAT PACKAGE across every codec in it.  Calling this
;;; %IMDCT redefined AAC's inverse transform, which takes six arguments rather than three, and the
;;; AAC suite failed with "invalid number of arguments: 6" from inside a file this work never
;;; touched.  A flat package means every internal name is a global name.

(defvar *vorbis-imdct-tables* (make-hash-table)
  "N -> the cosine matrix for a block of that size, built once and shared.")

(defun %vorbis-imdct-table (n)
  (or (gethash n *vorbis-imdct-tables*)
      (setf (gethash n *vorbis-imdct-tables*)
            (let ((tab (make-array (* n (ash n -1)) :element-type 'double-float)))
              (dotimes (i n tab)
                (dotimes (k (ash n -1))
                  (setf (aref tab (+ (* i (ash n -1)) k))
                        (cos (/ (* pi (+ (* 2 i) 1 (ash n -1)) (+ (* 2 k) 1))
                                (* 2d0 n))))))))))

(defun %vorbis-imdct (spectrum n out)
  "y[i] = sum_k X[k] cos(pi (2i+1+n/2)(2k+1) / 2n), the inverse of the MDCT Vorbis uses.

   NO 4/n.  The textbook inverse MDCT carries a 4/N normalisation, and the reference encoder folds
   exactly that factor into its FORWARD transform instead — so the coefficients in the bitstream are
   already scaled and applying it again here makes the output five hundred times too quiet.  The
   giveaway is a correlation of 0.999 against the reference with a relative RMS error of 0.998,
   which is what a pure gain error looks like and nothing else does."
  (declare (type (simple-array double-float (*)) spectrum out) (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (let ((tab (the (simple-array double-float (*)) (%vorbis-imdct-table n)))
        (half (ash n -1)))
    (declare (type fixnum half))
    (dotimes (i n out)
      (let ((acc 0d0) (base (* i half)))
        (declare (type double-float acc) (type fixnum base))
        (dotimes (k half)
          (incf acc (* (aref spectrum k) (aref tab (+ base k)))))
        (setf (aref out i) acc)))))


;;; ---- a plain iterative radix-2 FFT --------------------------------------------------------------

(defstruct (fft-plan (:conc-name fp-) (:constructor %make-fft-plan))
  (size 0 :type fixnum)
  (rev #() :type (simple-array fixnum (*)))
  (cs #() :type (simple-array double-float (*)))
  (sn #() :type (simple-array double-float (*))))

(defvar *fft-plans* (make-hash-table))

(defun fft-plan (p)
  "A plan for a complex FFT of size P, a power of two."
  (declare (type fixnum p))
  (or (gethash p *fft-plans*)
      (setf (gethash p *fft-plans*)
            (let ((rev (make-array p :element-type 'fixnum :initial-element 0))
                  (cs (make-array (max 1 (ash p -1)) :element-type 'double-float))
                  (sn (make-array (max 1 (ash p -1)) :element-type 'double-float))
                  (bits (1- (integer-length p))))
              (dotimes (i p)
                (let ((r 0))
                  (dotimes (b bits) (when (logbitp b i) (setf r (logior r (ash 1 (- bits 1 b))))))
                  (setf (aref rev i) r)))
              (dotimes (k (ash p -1))
                (let ((a (/ (* -2d0 pi k) p)))
                  (setf (aref cs k) (cos a) (aref sn k) (sin a))))
              (%make-fft-plan :size p :rev rev :cs cs :sn sn)))))

(defun %fft (re im plan)
  "In-place forward complex FFT, e^-2*pi*i*k*n/N."
  (declare (type (simple-array double-float (*)) re im) (type fft-plan plan)
           (optimize (speed 3) (safety 1)))
  (let* ((p (fp-size plan)) (rev (fp-rev plan)) (cs (fp-cs plan)) (sn (fp-sn plan)))
    (declare (type fixnum p) (type (simple-array fixnum (*)) rev)
             (type (simple-array double-float (*)) cs sn))
    (dotimes (i p)
      (let ((j (aref rev i)))
        (when (< i j)
          (rotatef (aref re i) (aref re j))
          (rotatef (aref im i) (aref im j)))))
    (let ((len 2))
      (declare (type fixnum len))
      (loop while (<= len p)
            do (let ((half (ash len -1)) (step (floor p len)))
                 (declare (type fixnum half step))
                 (loop for i of-type fixnum from 0 below p by len
                       do (dotimes (k half)
                            (let* ((w (* k step))
                                   (wr (aref cs w)) (wi (aref sn w))
                                   (a (+ i k)) (b (+ a half))
                                   (xr (aref re b)) (xi (aref im b))
                                   (tr (- (* xr wr) (* xi wi)))
                                   (ti (+ (* xr wi) (* xi wr))))
                              (declare (type fixnum a b) (type double-float wr wi xr xi tr ti))
                              (setf (aref re b) (- (aref re a) tr)
                                    (aref im b) (- (aref im a) ti)
                                    (aref re a) (+ (aref re a) tr)
                                    (aref im a) (+ (aref im a) ti)))))
                 (setf len (ash len 1)))))
    (values re im)))

;;; ---- the DCT-IV, and the transform on top of it -------------------------------------------------

(defstruct (imdct-plan (:conc-name ip-) (:constructor %make-imdct-plan))
  (n 0 :type fixnum)
  (fft nil :type (or null fft-plan))
  (pre-cs #() :type (simple-array double-float (*)))   ; e^-i(pi (8p+1) / 8M)
  (pre-sn #() :type (simple-array double-float (*)))
  (post-cs #() :type (simple-array double-float (*)))  ; e^-i(pi (8q+1) / 8M)
  (post-sn #() :type (simple-array double-float (*)))
  (re #() :type (simple-array double-float (*)))
  (im #() :type (simple-array double-float (*)))
  (z #() :type (simple-array double-float (*))))

(defvar *imdct-plans* (make-hash-table))

(defun imdct-plan (n)
  "A plan for a block of N samples: an FFT of size N/4 and the twiddles either side of it."
  (declare (type fixnum n))
  (or (gethash n *imdct-plans*)
      (setf (gethash n *imdct-plans*)
            (let* ((m (ash n -1))                 ; the DCT-IV's size
                   (h (ash m -1))                 ; and the FFT's
                   (pre-cs (make-array (max 1 h) :element-type 'double-float))
                   (pre-sn (make-array (max 1 h) :element-type 'double-float))
                   (post-cs (make-array (max 1 h) :element-type 'double-float))
                   (post-sn (make-array (max 1 h) :element-type 'double-float)))
              (dotimes (k h)
                (let ((a (/ (* (- pi) (+ (* 8 k) 1)) (* 8 m))))
                  (setf (aref pre-cs k) (cos a) (aref pre-sn k) (sin a)
                        (aref post-cs k) (cos a) (aref post-sn k) (sin a))))
              (%make-imdct-plan
               :n n :fft (and (plusp h) (fft-plan h))
               :pre-cs pre-cs :pre-sn pre-sn :post-cs post-cs :post-sn post-sn
               :re (make-array (max 1 h) :element-type 'double-float)
               :im (make-array (max 1 h) :element-type 'double-float)
               :z (make-array m :element-type 'double-float))))))

(defun %vorbis-imdct-fast (spectrum n out)
  "The same transform as %VORBIS-IMDCT, by way of one complex FFT of size N/4."
  (declare (type (simple-array double-float (*)) spectrum out) (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (let* ((plan (imdct-plan n))
         (m (ash n -1)) (h (ash m -1)) (n4 (ash n -2))
         (re (ip-re plan)) (im (ip-im plan)) (z (ip-z plan))
         (pre-cs (ip-pre-cs plan)) (pre-sn (ip-pre-sn plan))
         (post-cs (ip-post-cs plan)) (post-sn (ip-post-sn plan)))
    (declare (type fixnum m h n4)
             (type (simple-array double-float (*)) re im z pre-cs pre-sn post-cs post-sn))
    ;; u[p] = x[2p] + i x[M-1-2p], turned by the pre-twiddle
    (dotimes (p h)
      (let ((ur (aref spectrum (* 2 p)))
            (ui (aref spectrum (- m 1 (* 2 p))))
            (wr (aref pre-cs p)) (wi (aref pre-sn p)))
        (declare (type double-float ur ui wr wi))
        (setf (aref re p) (- (* ur wr) (* ui wi))
              (aref im p) (+ (* ur wi) (* ui wr)))))
    (%fft re im (ip-fft plan))
    ;; the post-twiddle, and the two halves of the DCT-IV it produces
    (dotimes (q h)
      (let* ((vr (aref re q)) (vi (aref im q))
             (wr (aref post-cs q)) (wi (aref post-sn q))
             (sr (- (* vr wr) (* vi wi)))
             (si (+ (* vr wi) (* vi wr))))
        (declare (type double-float vr vi wr wi sr si))
        (setf (aref z (* 2 q)) sr
              (aref z (- m 1 (* 2 q))) (- si))))
    ;; y[i] = Z[i + n/4], with the kernel's two symmetries covering the parts that run off the end
    (dotimes (i n out)
      (let ((j (+ i n4)))
        (declare (type fixnum j))
        (setf (aref out i)
              (cond ((< j m) (aref z j))
                    ((< j n) (- (aref z (- n 1 j))))
                    (t (- (aref z (- j n))))))))))
