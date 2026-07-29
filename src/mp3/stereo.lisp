;;;; src/stereo.lisp — MS and intensity stereo decoding.
(in-package #:reed)

(defconstant +inv-sqrt-2+ 0.70710678118654752d0)

(defun stereo (is0 is1 scl scs si header gr)
  "Apply joint-stereo (MS and/or intensity) processing for granule GR, in place
on the two channels' line buffers IS0 and IS1."
  (declare (type samples is0 is1) (type fixnum gr)
           (optimize (speed 3) (safety 1)))
  (when (or (/= (fh-mode header) 1) (= (fh-mode-extension header) 0))
    (return-from stereo))
  (let ((ext (fh-mode-extension header))
        (c0 (aref (si-count1 si) gr 0))
        (c1 (aref (si-count1 si) gr 1)))
    (declare (type fixnum ext c0 c1) (ignorable c0))
    ;; ---- MS (middle/side) stereo ----
    ;; The M/S rotation covers the whole spectrum (every coded line), including
    ;; regions where the Side channel is all zero (e.g. mono content coded MS) —
    ;; a per-channel count1 bound would wrongly skip those.
    (when (logtest ext #x2)
      (dotimes (i 576)
        (let ((l (* (+ (aref is0 i) (aref is1 i)) +inv-sqrt-2+))
              (r (* (- (aref is0 i) (aref is1 i)) +inv-sqrt-2+)))
          (setf (aref is0 i) l (aref is1 i) r))))
    ;; ---- intensity stereo (MPEG-1; LSF intensity handled separately) ----
    (when (and (logtest ext #x1) (eq (fh-version header) :mpeg1))
      (let* ((sfbi (fh-sfband-index header))
             (long-tab (sfb-long sfbi))
             (short-tab (sfb-short sfbi))
             (short-p (and (= 1 (aref (si-win-switch si) gr 0))
                           (= 2 (aref (si-block-type si) gr 0))))
             (mixed-p (= 1 (aref (si-mixed si) gr 0))))
        (declare (type (simple-array fixnum (*)) long-tab short-tab))
        (labels ((ratios (is-pos)
                   (declare (type fixnum is-pos))
                   (if (= is-pos 6)
                       (values 1.0d0 0.0d0)
                       (let ((rr (aref +is-ratios+ is-pos)))
                         (values (/ rr (1+ rr)) (/ 1.0d0 (1+ rr))))))
                 (intensity-long (sfb)
                   (declare (type fixnum sfb))
                   (let ((is-pos (aref scl gr 0 sfb)))
                     (declare (type fixnum is-pos))
                     (when (/= is-pos 7)
                       (multiple-value-bind (rl rr) (ratios is-pos)
                         (loop for i of-type fixnum from (aref long-tab sfb)
                                 below (aref long-tab (1+ sfb))
                               for v = (aref is0 i)
                               do (setf (aref is0 i) (* rl v)
                                        (aref is1 i) (* rr v)))))))
                 (intensity-short (sfb)
                   (declare (type fixnum sfb))
                   (let ((win-len (- (aref short-tab (1+ sfb)) (aref short-tab sfb))))
                     (declare (type fixnum win-len))
                     (dotimes (win 3)
                       (let ((is-pos (aref scs gr 0 sfb win)))
                         (declare (type fixnum is-pos))
                         (when (/= is-pos 7)
                           (multiple-value-bind (rl rr) (ratios is-pos)
                             (let* ((start (+ (* 3 (aref short-tab sfb)) (* win-len win)))
                                    (stop (+ start win-len)))
                               (declare (type fixnum start stop))
                               (loop for i of-type fixnum from start below stop
                                     for v = (aref is0 i)
                                     do (setf (aref is0 i) (* rl v)
                                              (aref is1 i) (* rr v)))))))))))
          (cond
            (short-p
             (cond
               (mixed-p
                (loop for sfb of-type fixnum from 0 below 8
                      when (>= (aref long-tab sfb) c1) do (intensity-long sfb))
                (loop for sfb of-type fixnum from 3 below 12
                      when (>= (* 3 (aref short-tab sfb)) c1) do (intensity-short sfb)))
               (t
                (loop for sfb of-type fixnum from 0 below 12
                      when (>= (* 3 (aref short-tab sfb)) c1) do (intensity-short sfb)))))
            (t
             (loop for sfb of-type fixnum from 0 below 21
                   when (>= (aref long-tab sfb) c1) do (intensity-long sfb)))))))))
