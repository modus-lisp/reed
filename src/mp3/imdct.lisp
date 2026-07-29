;;;; src/imdct.lisp — reorder, alias reduction, IMDCT + windowing, overlap-add,
;;;; and frequency inversion (the "hybrid synthesis" stage).
(in-package #:reed)

(defconstant +pi+ (coerce pi 'double-float))

;;; ---- precomputed IMDCT windows and cosine matrices ------------------------
(declaim (type (simple-array double-float (4 36)) +imdct-win+))
(defparameter +imdct-win+
  (let ((w (make-array '(4 36) :element-type 'double-float :initial-element 0.0d0)))
    (flet ((s (den i off) (sin (* (/ +pi+ den) (+ i 0.5d0 off)))))
      (dotimes (i 36) (setf (aref w 0 i) (s 36 i 0.0d0)))
      (dotimes (i 18) (setf (aref w 1 i) (s 36 i 0.0d0)))
      (loop for i from 18 below 24 do (setf (aref w 1 i) 1.0d0))
      (loop for i from 24 below 30 do (setf (aref w 1 i) (s 12 i -18.0d0)))
      (dotimes (i 12) (setf (aref w 2 i) (s 12 i 0.0d0)))
      (loop for i from 6 below 12 do (setf (aref w 3 i) (s 12 i -6.0d0)))
      (loop for i from 12 below 18 do (setf (aref w 3 i) 1.0d0))
      (loop for i from 18 below 36 do (setf (aref w 3 i) (s 36 i 0.0d0))))
    w))

(declaim (type (simple-array double-float (18 36)) +cos-n36+))
(defparameter +cos-n36+
  (let ((a (make-array '(18 36) :element-type 'double-float)))
    (dotimes (m 18 a)
      (dotimes (p 36)
        (setf (aref a m p)
              (cos (* (/ +pi+ 72.0d0) (+ (* 2 p) 1 18) (+ (* 2 m) 1))))))))

(declaim (type (simple-array double-float (6 12)) +cos-n12+))
(defparameter +cos-n12+
  (let ((a (make-array '(6 12) :element-type 'double-float)))
    (dotimes (m 6 a)
      (dotimes (p 12)
        (setf (aref a m p)
              (cos (* (/ +pi+ 24.0d0) (+ (* 2 p) 1 6) (+ (* 2 m) 1))))))))

;;; ---- reorder short blocks -------------------------------------------------
(defun reorder (is si header gr ch)
  (declare (type samples is) (type fixnum gr ch) (optimize (speed 3) (safety 1)))
  (unless (and (= 1 (aref (si-win-switch si) gr ch))
               (= 2 (aref (si-block-type si) gr ch)))
    (return-from reorder))
  (let* ((short-tab (sfb-short (fh-sfband-index header)))
         (count1 (aref (si-count1 si) gr ch))
         (re (make-array 576 :element-type 'double-float :initial-element 0.0d0))
         (sfb (if (/= 0 (aref (si-mixed si) gr ch)) 3 0))
         (next (* 3 (aref short-tab (1+ sfb))))
         (win-len (- (aref short-tab (1+ sfb)) (aref short-tab sfb)))
         (i (if (= sfb 0) 0 36)))
    (declare (type (simple-array fixnum (*)) short-tab)
             (type fixnum count1 sfb next win-len i)
             (dynamic-extent re))
    (loop while (< i 576) do
      (when (= i next)
        (dotimes (j (* 3 win-len))
          (setf (aref is (+ (* 3 (aref short-tab sfb)) j)) (aref re j)))
        (when (>= i count1) (return-from reorder))
        (incf sfb)
        (setf next (* 3 (aref short-tab (1+ sfb)))
              win-len (- (aref short-tab (1+ sfb)) (aref short-tab sfb))))
      (dotimes (win 3)
        (dotimes (j win-len)
          (setf (aref re (+ (* j 3) win)) (aref is i))
          (incf i))))
    (dotimes (j (* 3 win-len))
      (setf (aref is (+ (* 3 (aref short-tab 12)) j)) (aref re j)))))

;;; ---- alias reduction ------------------------------------------------------
(defun antialias (is si header gr ch)
  (declare (type samples is) (type fixnum gr ch) (ignore header)
           (optimize (speed 3) (safety 1)))
  (when (and (= 1 (aref (si-win-switch si) gr ch))
             (= 2 (aref (si-block-type si) gr ch))
             (= 0 (aref (si-mixed si) gr ch)))
    (return-from antialias))
  (let ((sblim (if (and (= 1 (aref (si-win-switch si) gr ch))
                        (= 2 (aref (si-block-type si) gr ch))
                        (= 1 (aref (si-mixed si) gr ch)))
                   2 32)))
    (declare (type fixnum sblim))
    (loop for sb of-type fixnum from 1 below sblim do
      (dotimes (i 8)
        (let* ((li (- (* 18 sb) 1 i))
               (ui (+ (* 18 sb) i))
               (cs (aref +cs+ i)) (ca (aref +ca+ i))
               (lo (aref is li)) (up (aref is ui)))
          (declare (type fixnum li ui) (type double-float cs ca lo up))
          (setf (aref is li) (- (* lo cs) (* up ca))
                (aref is ui) (+ (* up cs) (* lo ca))))))))

;;; ---- IMDCT + windowing ----------------------------------------------------
(declaim (ftype (function (samples fixnum (simple-array double-float (36)) fixnum))
                imdct-win))
(defun imdct-win (is base out block-type)
  "IMDCT+window the 18 lines IS[base..base+18) into OUT[0..36)."
  (declare (type samples is) (type fixnum base block-type)
           (type (simple-array double-float (36)) out)
           (optimize (speed 3) (safety 1)))
  (dotimes (i 36) (setf (aref out i) 0.0d0))
  (cond
    ((= block-type 2)
     (dotimes (i 3)
       (dotimes (p 12)
         (let ((sum 0.0d0))
           (declare (type double-float sum))
           (dotimes (m 6)
             (incf sum (* (aref is (+ base i (* 3 m))) (aref +cos-n12+ m p))))
           (incf (aref out (+ (* 6 i) p 6)) (* sum (aref +imdct-win+ 2 p)))))))
    (t
     (dotimes (p 36)
       (let ((sum 0.0d0))
         (declare (type double-float sum))
         (dotimes (m 18)
           (incf sum (* (aref is (+ base m)) (aref +cos-n36+ m p))))
         (setf (aref out p) (* sum (aref +imdct-win+ block-type p))))))))

;;; ---- hybrid synthesis: IMDCT per subband + overlap-add --------------------
(defun hybrid-synthesis (is store si header gr ch)
  "Run the IMDCT for all 32 subbands of channel CH and overlap-add with the
persistent STORE[ch][32][18]."
  (declare (type samples is) (type (simple-array double-float (2 32 18)) store)
           (type fixnum gr ch) (ignore header) (optimize (speed 3) (safety 1)))
  (let ((rawout (make-array 36 :element-type 'double-float)))
    (declare (dynamic-extent rawout))
    (dotimes (sb 32)
      (let ((bt (if (and (= 1 (aref (si-win-switch si) gr ch))
                         (= 1 (aref (si-mixed si) gr ch))
                         (< sb 2))
                    0
                    (aref (si-block-type si) gr ch))))
        (imdct-win is (* sb 18) rawout bt)
        (dotimes (i 18)
          (setf (aref is (+ (* sb 18) i)) (+ (aref rawout i) (aref store ch sb i))
                (aref store ch sb i) (aref rawout (+ i 18))))))))

;;; ---- frequency inversion --------------------------------------------------
(defun frequency-inversion (is)
  (declare (type samples is) (optimize (speed 3) (safety 1)))
  (loop for sb of-type fixnum from 1 below 32 by 2 do
    (loop for i of-type fixnum from 1 below 18 by 2 do
      (setf (aref is (+ (* sb 18) i)) (- (aref is (+ (* sb 18) i)))))))
