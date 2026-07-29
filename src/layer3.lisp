;;;; src/layer3.lisp — main-data parsing (scalefactors + Huffman) and the
;;;; per-granule decode pipeline that ties the DSP stages together.
(in-package #:reed)

;; slen1/slen2 (scalefactor field widths) per MPEG-1 scalefac_compress.
(defparameter +scalefac-sizes+
  (let ((a (make-array '(16 2) :element-type 'fixnum)))
    (loop for (s1 s2) in '((0 0)(0 1)(0 2)(0 3)(3 0)(1 1)(1 2)(1 3)
                           (2 1)(2 2)(2 3)(3 1)(3 2)(3 3)(4 2)(4 3))
          for i from 0 do (setf (aref a i 0) s1 (aref a i 1) s2))
    a))

(defun read-scalefactors-mpeg1 (br si scl scs gr ch)
  "Read MPEG-1 scalefactors for granule GR channel CH from main-data reader BR."
  (declare (type bitreader br) (type fixnum gr ch))
  (let* ((sc (aref (si-scalefac-compress si) gr ch))
         (slen1 (aref +scalefac-sizes+ sc 0))
         (slen2 (aref +scalefac-sizes+ sc 1))
         (short-p (and (/= 0 (aref (si-win-switch si) gr ch))
                       (= 2 (aref (si-block-type si) gr ch))))
         (mixed-p (/= 0 (aref (si-mixed si) gr ch))))
    (cond
      (short-p
       (cond
         (mixed-p
          (dotimes (sfb 8) (setf (aref scl gr ch sfb) (read-bits br slen1)))
          (loop for sfb from 3 below 12
                for nb = (if (< sfb 6) slen1 slen2)
                do (dotimes (win 3) (setf (aref scs gr ch sfb win) (read-bits br nb)))))
         (t
          (loop for sfb from 0 below 12
                for nb = (if (< sfb 6) slen1 slen2)
                do (dotimes (win 3) (setf (aref scs gr ch sfb win) (read-bits br nb)))))))
      (t
       (flet ((band (lo hi nb sb)
                (declare (type fixnum lo hi nb sb))
                (if (or (= 0 (aref (si-scfsi si) ch sb)) (= gr 0))
                    (loop for sfb from lo below hi
                          do (setf (aref scl gr ch sfb) (read-bits br nb)))
                    (loop for sfb from lo below hi   ; copy from granule 0
                          do (setf (aref scl 1 ch sfb) (aref scl 0 ch sfb))))))
         (band 0 6 slen1 0) (band 6 11 slen1 1)
         (band 11 16 slen2 2) (band 16 21 slen2 3))))))

;; nr_of_sfb[blocknumber 0..5][blocktypenumber 0..2][group 0..3] — number of
;; scalefactors per group for MPEG-2/2.5 LSF (ISO/IEC 13818-3).
(defparameter +nr-of-sfb+
  (let ((a (make-array '(6 3 4) :element-type 'fixnum))
        (data '(((6 5 5 5)(9 9 9 9)(6 9 9 9))
                ((6 5 7 3)(9 9 12 6)(6 9 12 6))
                ((11 10 0 0)(18 18 0 0)(15 18 0 0))
                ((7 7 7 0)(12 12 12 0)(6 15 12 0))
                ((6 6 6 3)(12 9 9 6)(6 12 9 6))
                ((8 8 5 0)(15 12 9 0)(6 18 9 0)))))
    (loop for bn from 0 for rows in data do
      (loop for bt from 0 for row in rows do
        (loop for g from 0 for v in row do (setf (aref a bn bt g) v))))
    a))

(defun read-scalefactors-mpeg2 (br si scl scs header gr ch)
  "Read MPEG-2/2.5 (LSF) scalefactors for granule GR channel CH."
  (declare (type bitreader br) (type fixnum gr ch))
  (let* ((sfc (aref (si-scalefac-compress si) gr ch))
         (short-p (and (/= 0 (aref (si-win-switch si) gr ch))
                       (= 2 (aref (si-block-type si) gr ch))))
         (mixed-p (/= 0 (aref (si-mixed si) gr ch)))
         (i-stereo (and (= 1 (fh-mode header)) (logtest (fh-mode-extension header) 1)))
         (right-is (and i-stereo (= ch 1)))
         (btn (if short-p (if mixed-p 2 1) 0))
         (slen (make-array 4 :element-type 'fixnum :initial-element 0))
         (bn 0))
    (declare (dynamic-extent slen) (type fixnum sfc btn bn))
    (if (not right-is)
        (cond
          ((< sfc 400)
           (setf (aref slen 0) (floor (ash sfc -4) 5) (aref slen 1) (mod (ash sfc -4) 5)
                 (aref slen 2) (ash (mod sfc 16) -2) (aref slen 3) (mod sfc 4) bn 0))
          ((< sfc 500)
           (let ((x (- sfc 400)))
             (setf (aref slen 0) (floor (ash x -2) 5) (aref slen 1) (mod (ash x -2) 5)
                   (aref slen 2) (mod x 4) (aref slen 3) 0 bn 1)))
          (t
           (let ((x (- sfc 500)))
             (setf (aref slen 0) (floor x 3) (aref slen 1) (mod x 3)
                   (aref slen 2) 0 (aref slen 3) 0 bn 2))))
        (let ((isc (ash sfc -1)))
          (cond
            ((< isc 180)
             (setf (aref slen 0) (floor isc 36) (aref slen 1) (floor (mod isc 36) 6)
                   (aref slen 2) (mod (mod isc 36) 6) (aref slen 3) 0 bn 3))
            ((< isc 244)
             (let ((x (- isc 180)))
               (setf (aref slen 0) (ash (mod x 64) -4) (aref slen 1) (ash (mod x 16) -2)
                     (aref slen 2) (mod x 4) (aref slen 3) 0 bn 4)))
            (t
             (let ((x (- isc 244)))
               (setf (aref slen 0) (floor x 3) (aref slen 1) (mod x 3)
                     (aref slen 2) 0 (aref slen 3) 0 bn 5))))))
    (flet ((rd (g) (let ((sl (aref slen g))) (if (> sl 0) (read-bits br sl) 0))))
      (cond
        ((= btn 0)                       ; long blocks
         (let ((k 0))
           (dotimes (grp 4) (dotimes (n (aref +nr-of-sfb+ bn 0 grp))
                              (setf (aref scl gr ch k) (rd grp)) (incf k)))))
        ((= btn 1)                       ; short blocks (sfb-major, window-minor)
         (let ((k 0))
           (dotimes (grp 4) (dotimes (n (aref +nr-of-sfb+ bn 1 grp))
                              (setf (aref scs gr ch (floor k 3) (mod k 3)) (rd grp))
                              (incf k)))))
        (t                               ; mixed blocks
         (dotimes (n (aref +nr-of-sfb+ bn 2 0)) (setf (aref scl gr ch n) (rd 0)))
         (let ((k 0))
           (loop for grp from 1 to 3 do
             (dotimes (n (aref +nr-of-sfb+ bn 2 grp))
               (setf (aref scs gr ch (+ 3 (floor k 3)) (mod k 3)) (rd grp))
               (incf k)))))))))

(defun read-huffman (br is si header gr ch part2-start)
  "Decode the big-value and count1 regions into the 576-line buffer IS."
  (declare (type bitreader br) (type samples is) (type fixnum gr ch part2-start)
           (optimize (speed 3) (safety 1)))
  (let ((p23 (aref (si-part2-3-length si) gr ch)))
    (declare (type fixnum p23))
    (when (zerop p23)
      (dotimes (i 576) (setf (aref is i) 0.0d0))
      (setf (aref (si-count1 si) gr ch) 0)
      (return-from read-huffman))
    (let* ((bit-end (+ part2-start p23 -1))
           (short-p (and (= 1 (aref (si-win-switch si) gr ch))
                         (= 2 (aref (si-block-type si) gr ch))))
           (long-tab (sfb-long (fh-sfband-index header)))
           (r0 (aref (si-region0-count si) gr ch))
           (r1 (aref (si-region1-count si) gr ch))
           (region1 (if short-p 36 (aref long-tab (+ r0 1))))
           (region2 (if short-p 576 (aref long-tab (min 22 (+ r0 r1 2)))))
           (bign (* 2 (aref (si-big-values si) gr ch)))
           (ts0 (aref (si-table-select si) gr ch 0))
           (ts1 (aref (si-table-select si) gr ch 1))
           (ts2 (aref (si-table-select si) gr ch 2))
           (is-pos 0))
      (declare (type fixnum bit-end region1 region2 bign ts0 ts1 ts2 is-pos)
               (type (simple-array fixnum (*)) long-tab))
      ;; big values (two lines per codeword)
      (loop while (< is-pos bign) do
        (let ((table (cond ((< is-pos region1) ts0)
                           ((< is-pos region2) ts1)
                           (t ts2))))
          (multiple-value-bind (x y) (huffman-decode br table)
            (setf (aref is is-pos) (coerce x 'double-float)) (incf is-pos)
            (setf (aref is is-pos) (coerce y 'double-float)) (incf is-pos))))
      ;; count1 (quadruples)
      (let ((table (+ 32 (aref (si-count1table-select si) gr ch))))
        (declare (type fixnum table))
        (block count1
          (loop while (and (<= is-pos 572) (<= (br-pos br) bit-end)) do
            (multiple-value-bind (x y v w) (huffman-decode br table)
              (setf (aref is is-pos) (coerce v 'double-float)) (incf is-pos)
              (when (>= is-pos 576) (return-from count1))
              (setf (aref is is-pos) (coerce w 'double-float)) (incf is-pos)
              (when (>= is-pos 576) (return-from count1))
              (setf (aref is is-pos) (coerce x 'double-float)) (incf is-pos)
              (when (>= is-pos 576) (return-from count1))
              (setf (aref is is-pos) (coerce y 'double-float)) (incf is-pos)))))
      ;; back off a quadruple if we overran
      (when (> (br-pos br) (1+ bit-end)) (decf is-pos 4))
      (setf (aref (si-count1 si) gr ch) (max is-pos 0))
      (loop for i of-type fixnum from (max is-pos 0) below 576
            do (setf (aref is i) 0.0d0))
      (br-set-pos br (1+ bit-end)))))

;;; ---- per-frame decode -----------------------------------------------------
(defun decode-frame-granules (br header si scl scs is-vec store vvec out0 out1 emit)
  "Decode both granules from main-data reader BR; call (funcall EMIT out0 out1 ngr)
once per granule with the channel time-sample buffers filled."
  (declare (type bitreader br) (type simple-vector is-vec)
           (type (simple-array double-float (2 32 18)) store)
           (type (simple-array double-float (2 1024)) vvec)
           (type samples out0 out1))
  (let ((nch (fh-channels header))
        (ngr (si-ngr si))
        (mpeg1p (eq (fh-version header) :mpeg1)))
    ;; reset scalefactors
    (dotimes (a (array-total-size scl)) (setf (row-major-aref scl a) 0))
    (dotimes (a (array-total-size scs)) (setf (row-major-aref scs a) 0))
    ;; --- parse scalefactors + huffman for every granule/channel ---
    (dotimes (gr ngr)
      (dotimes (ch nch)
        (let ((part2-start (br-pos br))
              (is (svref is-vec (+ (* gr 2) ch))))
          (if mpeg1p
              (read-scalefactors-mpeg1 br si scl scs gr ch)
              (read-scalefactors-mpeg2 br si scl scs header gr ch))
          (read-huffman br is si header gr ch part2-start))))
    ;; --- DSP per granule ---
    (dotimes (gr ngr)
      (dotimes (ch nch)
        (let ((is (svref is-vec (+ (* gr 2) ch))))
          (requantize is scl scs si header gr ch)
          (reorder is si header gr ch)))
      (when (= nch 2)
        (stereo (svref is-vec (* gr 2)) (svref is-vec (+ (* gr 2) 1))
                scl scs si header gr))
      (dotimes (ch nch)
        (let ((is (svref is-vec (+ (* gr 2) ch)))
              (out (if (= ch 0) out0 out1)))
          (antialias is si header gr ch)
          (hybrid-synthesis is store si header gr ch)
          (frequency-inversion is)
          (subband-synthesis is vvec ch out)))
      (funcall emit out0 out1 nch))))
