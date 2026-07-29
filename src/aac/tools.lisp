;;;; src/aac/tools.lisp --- AAC spectral-domain tools applied before the
;;;; filterbank: Temporal Noise Shaping, Mid/Side and Intensity stereo.
;;;; ISO/IEC 14496-3 clauses 4.6.8, 4.6.9.
(in-package #:reed)

;;; Band-type codes (ISO Table 4.45 semantics)
(defconstant +bt-zero+       0)
(defconstant +bt-esc+        11)
(defconstant +bt-noise+      13)   ; PNS
(defconstant +bt-intensity2+ 14)
(defconstant +bt-intensity+  15)

(declaim (inline aac-pow2sf))
(defun aac-pow2sf (x)
  "2^(x/4) for integer scalefactor exponent X."
  (declare (type fixnum x))
  (expt 2d0 (/ x 4d0)))

;;; ------------------------------------------------------------------ ;;;
;;; Temporal Noise Shaping                                              ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-parcor->lpc (parcor order)
  "Reflection (parcor) coefficients -> LPC coefficients, in place recursion
matching the AAC/ffmpeg step-up (lpc_stride 0, normalize 0)."
  (declare (type (simple-array double-float (*)) parcor) (type fixnum order))
  (let ((lpc (make-array order :element-type 'double-float :initial-element 0d0)))
    (dotimes (i order lpc)
      (let ((r (- (aref parcor i))))
        (declare (type double-float r))
        (setf (aref lpc i) r)
        (dotimes (j (ash (1+ i) -1))
          (let ((f (aref lpc j))
                (b (aref lpc (- i 1 j))))
            (setf (aref lpc j)         (+ f (* r b))
                  (aref lpc (- i 1 j)) (+ b (* r f)))))))))

(defun aac-apply-tns (coeffs ics)
  "Apply the all-pole TNS filters to the 1024 spectral COEFFS in place."
  (declare (type (simple-array double-float (*)) coeffs))
  (let* ((tns (aac-chan-tns ics))
         (mmm (min (aac-chan-tns-max-bands ics) (aac-chan-max-sfb ics)))
         (swb (aac-chan-swb ics))
         (num-swb (aac-chan-num-swb ics)))
    (when (or (null tns) (zerop mmm)) (return-from aac-apply-tns))
    (dotimes (w (aac-chan-num-windows ics))
      (let ((nfilt (aref (tns-n-filt tns) w))
            (bottom num-swb))
        (dotimes (filt nfilt)
          (let* ((top bottom)
                 (len (aref (tns-length tns) w filt))
                 (order (aref (tns-order tns) w filt)))
            (setf bottom (max 0 (- top len)))
            (when (plusp order)
              (let* ((lpc (aac-parcor->lpc
                           (let ((v (make-array order :element-type 'double-float)))
                             (dotimes (i order v)
                               (setf (aref v i) (aref (tns-coef tns) w filt i))))
                           order))
                     (start (aref swb (min bottom mmm)))
                     (end   (aref swb (min top mmm)))
                     (size (- end start))
                     (inc 1))
                (declare (type (simple-array double-float (*)) lpc))
                (when (plusp size)
                  (when (= 1 (aref (tns-direction tns) w filt))
                    (setf inc -1 start (1- end)))
                  (incf start (* w 128))
                  (let ((p start))
                    (dotimes (m size)
                      (let ((acc (aref coeffs p)))
                        (declare (type double-float acc))
                        (loop for i of-type fixnum from 1 to (min m order)
                              do (decf acc (* (aref coeffs (- p (* i inc)))
                                              (aref lpc (1- i)))))
                        (setf (aref coeffs p) acc))
                      (incf p inc))))))))))))

;;; ------------------------------------------------------------------ ;;;
;;; Mid/Side stereo                                                     ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-apply-ms (ch0 ch1 ms-mask)
  "In-place M/S butterfly on paired channels' coefficients."
  (let* ((ics ch0)
         (c0 (aac-chan-coeffs ch0))
         (c1 (aac-chan-coeffs ch1))
         (swb (aac-chan-swb ics))
         (max-sfb (aac-chan-max-sfb ics))
         (bt0 (aac-chan-band-type ch0))
         (bt1 (aac-chan-band-type ch1))
         (base 0))
    (declare (type (simple-array double-float (*)) c0 c1))
    (dotimes (g (aac-chan-num-groups ics))
      (let ((glen (aref (aac-chan-group-len ics) g)))
        (dotimes (sfb max-sfb)
          (let ((idx (+ (* g max-sfb) sfb)))
            (when (and (aref ms-mask idx)
                       (< (aref bt0 idx) +bt-noise+)
                       (< (aref bt1 idx) +bt-noise+))
              (dotimes (grp glen)
                (loop for k from (aref swb sfb) below (aref swb (1+ sfb))
                      for p = (+ base (* grp 128) k)
                      do (let ((l (aref c0 p)) (r (aref c1 p)))
                           (setf (aref c0 p) (+ l r)
                                 (aref c1 p) (- l r))))))))
        (incf base (* glen 128))))))

;;; ------------------------------------------------------------------ ;;;
;;; Intensity stereo                                                    ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-apply-intensity (ch0 ch1 ms-present ms-mask)
  "Reconstruct ch1 intensity bands from ch0 (ISO 4.6.8.2.3)."
  (let* ((ics ch1)
         (c0 (aac-chan-coeffs ch0))
         (c1 (aac-chan-coeffs ch1))
         (swb (aac-chan-swb ics))
         (max-sfb (aac-chan-max-sfb ics))
         (bt1 (aac-chan-band-type ch1))
         (sf1 (aac-chan-sfmult ch1))
         (base 0))
    (declare (type (simple-array double-float (*)) c0 c1))
    (dotimes (g (aac-chan-num-groups ics))
      (let ((glen (aref (aac-chan-group-len ics) g)))
        (dotimes (sfb max-sfb)
          (let ((idx (+ (* g max-sfb) sfb)))
            (when (or (= (aref bt1 idx) +bt-intensity+)
                      (= (aref bt1 idx) +bt-intensity2+))
              (let ((c (+ -1 (* 2 (- (aref bt1 idx) 14)))))
                (when ms-present
                  (setf c (* c (- 1 (* 2 (if (aref ms-mask idx) 1 0))))))
                (let ((scale (* c (aref sf1 idx))))
                  (dotimes (grp glen)
                    (loop for k from (aref swb sfb) below (aref swb (1+ sfb))
                          for p = (+ base (* grp 128) k)
                          do (setf (aref c1 p) (* (aref c0 p) scale)))))))))
        (incf base (* glen 128))))))
