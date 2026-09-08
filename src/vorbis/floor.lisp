;;;; src/vorbis/floor.lisp — the spectral envelope, drawn with a line algorithm.
;;;;
;;;; A Vorbis frame is a FLOOR times a RESIDUE.  The floor is the coarse shape of the spectrum —
;;;; where the energy is — and the residue is the fine structure that gets multiplied by it.  That
;;;; split is the codec's central idea: the ear cares about the envelope far more than about the
;;;; detail under it, so the envelope is coded carefully and cheaply and the detail is coded coarsely
;;;; and expensively, and the product is what you hear.
;;;;
;;;; FLOOR 1 IS A POLYLINE ON A LOG-AMPLITUDE SCALE, and the specification draws it with an integer
;;;; Bresenham line — not with floating point, not with interpolation.  §7.2.4 warns that "deviation
;;;; from implementing a strictly equivalent algorithm can result in serious decoding errors", and
;;;; it means it: the encoder chose the Y values knowing exactly which samples this algorithm would
;;;; produce, so a line drawn a different way is a different floor and every sample under it is
;;;; wrong.  Integer division here rounds toward zero for both signs, which is Lisp's TRUNCATE and
;;;; not its FLOOR.
;;;;
;;;; Floor 0 — a line spectral pair representation — is refused.  No encoder in use has emitted it
;;;; since the format was frozen; libvorbis has never produced one.

(in-package #:reed)

;;; ---- the dB table, which is a formula ----------------------------------------------------------
;;;
;;; The reference implementation carries 256 printed floats.  They are exactly
;;; 10^((i-255) * 140/5120): a hundred and forty decibels spread over two hundred and fifty six
;;; steps, 0.546875 dB each, ending at unity.  Generating them from that says what the table IS,
;;; and agrees with libvorbis's printed constants to 6.7e-7 relative — which is float32 rounding in
;;; the printed values, not disagreement.

(defparameter +floor1-inverse-db+
  (let ((tab (make-array 256 :element-type 'double-float)))
    (dotimes (i 256 tab)
      (setf (aref tab i) (expt 10d0 (/ (* (- i 255) 140d0) 5120d0)))))
  "Amplitude for a floor Y value: 140 dB over 256 steps (§9.2 floor1_inverse_dB_static_table).")
(declaim (type (simple-array double-float (256)) +floor1-inverse-db+))

;;; ---- configuration ------------------------------------------------------------------------------

(defstruct (vfloor1 (:conc-name f1-))
  (partition-class #() :type simple-vector)
  (class-dimensions #() :type simple-vector)
  (class-subclasses #() :type simple-vector)
  (class-masterbooks #() :type simple-vector)
  (subclass-books #() :type simple-vector)      ; per class, a vector of book numbers or -1
  (multiplier 1 :type fixnum)
  (x-list #() :type (simple-array fixnum (*)))
  (values 0 :type fixnum)
  (sorted #() :type (simple-array fixnum (*)))) ; indices of X-LIST in ascending X order

(defun read-floor (v ncodebooks)
  "One floor configuration (§7.1, §7.2.2)."
  (declare (type vbits v))
  (let ((type (vb v 16)))
    (case type
      (0 (vorbis-error "floor 0 (line spectral pair) is not supported"))
      (1
       (let* ((partitions (vb v 5))
              (class-list (make-array partitions)))
         (dotimes (i partitions) (setf (aref class-list i) (vb v 4)))
         (let* ((maximum-class (if (plusp partitions) (reduce #'max class-list) -1))
                (nclasses (1+ maximum-class))
                (dims (make-array (max 0 nclasses)))
                (subs (make-array (max 0 nclasses)))
                (masters (make-array (max 0 nclasses) :initial-element -1))
                (books (make-array (max 0 nclasses))))
           (dotimes (i nclasses)
             (setf (aref dims i) (1+ (vb v 3)))
             (setf (aref subs i) (vb v 2))
             (when (plusp (aref subs i))
               (setf (aref masters i) (vb v 8))
               (unless (< (aref masters i) ncodebooks)
                 (vorbis-error "floor 1: master book ~d is past the end of the codebook list"
                               (aref masters i))))
             (let ((n (ash 1 (aref subs i))))
               (setf (aref books i) (make-array n))
               (dotimes (j n)
                 (let ((b (1- (vb v 8))))
                   (unless (< b ncodebooks)
                     (vorbis-error "floor 1: subclass book ~d is past the end of the codebook list"
                                   b))
                   (setf (aref (aref books i) j) b)))))
           (let* ((multiplier (1+ (vb v 2)))
                  (rangebits (vb v 4))
                  (total (+ 2 (loop for i below partitions
                                    sum (aref dims (aref class-list i)))))
                  (x (make-array total :element-type 'fixnum :initial-element 0)))
             (when (> total 65)
               (vorbis-error "floor 1: ~d X values, more than the 65 the format allows" total))
             (setf (aref x 0) 0 (aref x 1) (ash 1 rangebits))
             (let ((n 2))
               (dotimes (i partitions)
                 (dotimes (j (aref dims (aref class-list i)))
                   (setf (aref x n) (vb v rangebits))
                   (incf n))))
             (when (vb-eop v) (vorbis-error "floor 1: setup packet ended early"))
             ;; every X must be distinct — the curve is a function of X and two points at the same
             ;; place have no defined order
             (let ((seen (make-hash-table)))
               (dotimes (i total)
                 (when (gethash (aref x i) seen)
                   (vorbis-error "floor 1: X value ~d appears twice" (aref x i)))
                 (setf (gethash (aref x i) seen) t)))
             (make-vfloor1 :partition-class class-list :class-dimensions dims
                           :class-subclasses subs :class-masterbooks masters
                           :subclass-books books :multiplier multiplier
                           :x-list x :values total
                           :sorted (let ((idx (sort (loop for i below total collect i)
                                                    #'< :key (lambda (i) (aref x i)))))
                                     (make-array total :element-type 'fixnum
                                                       :initial-contents idx)))))))
      (t (vorbis-error "floor type ~d is not defined by Vorbis I" type)))))

;;; ---- packet decode ------------------------------------------------------------------------------

(defparameter +floor1-ranges+ #(256 128 86 64))

(defun floor1-decode (f v codebooks y)
  "Read one channel's floor Y values (§7.2.3).  NIL means the channel is unused this frame — which
   is not the same as a floor at minimum amplitude, and the difference is audible."
  (declare (type vfloor1 f) (type vbits v) (type (simple-array fixnum (*)) y))
  (when (zerop (vb1 v)) (return-from floor1-decode nil))
  (let* ((range (aref +floor1-ranges+ (1- (f1-multiplier f))))
         (bits (ilog (1- range))))
    (setf (aref y 0) (vb v bits)
          (aref y 1) (vb v bits))
    (let ((offset 2))
      (dotimes (i (length (f1-partition-class f)))
        (let* ((cls (aref (f1-partition-class f) i))
               (cdim (aref (f1-class-dimensions f) cls))
               (cbits (aref (f1-class-subclasses f) cls))
               (csub (1- (ash 1 cbits)))
               (cval 0))
          (when (plusp cbits)
            (let ((e (vc-scalar (aref codebooks (aref (f1-class-masterbooks f) cls)) v)))
              (when (minusp e) (return-from floor1-decode nil))
              (setf cval e)))
          (dotimes (j cdim)
            (let ((book (aref (aref (f1-subclass-books f) cls) (logand cval csub))))
              (setf cval (ash cval (- cbits)))
              (setf (aref y (+ j offset))
                    (if (minusp book)
                        0
                        (let ((e (vc-scalar (aref codebooks book) v)))
                          (when (minusp e) (return-from floor1-decode nil))
                          e)))))
          (incf offset cdim))))
    ;; running out of packet here is nominal: the channel is simply unused (§7.2.3)
    (if (vb-eop v) nil t)))

(defun %render-point (x0 y0 x1 y1 x)
  "§9.2.6: the Y of the line through (x0,y0)-(x1,y1) at X, without leaving the integers."
  (declare (type fixnum x0 y0 x1 y1 x))
  (let* ((dy (- y1 y0)) (adx (- x1 x0)) (ady (abs dy))
         (err (* ady (- x x0)))
         (off (truncate err adx)))
    (if (minusp dy) (- y0 off) (+ y0 off))))

(defun %render-line (x0 y0 x1 y1 out n)
  "§9.2.7, clamped to N.  Integer division toward zero for both signs — TRUNCATE, not FLOOR."
  (declare (type fixnum x0 y0 x1 y1 n) (type (simple-array double-float (*)) out))
  (let* ((dy (- y1 y0)) (adx (- x1 x0)) (ady (abs dy))
         (base (truncate dy adx))
         (sy (if (minusp dy) (1- base) (1+ base)))
         (x x0) (y y0) (err 0))
    (declare (type fixnum dy adx ady base sy x y err))
    (decf ady (* (abs base) adx))
    (when (< x n) (setf (aref out x) (aref +floor1-inverse-db+ (logand y 255))))
    (loop for xx of-type fixnum from (1+ x0) below x1
          do (incf err ady)
             (if (>= err adx)
                 (progn (decf err adx) (incf y sy))
                 (incf y base))
             (when (< xx n)
               (setf (aref out xx) (aref +floor1-inverse-db+ (logand y 255)))))))

(defun floor1-curve (f y out n)
  "Amplitude synthesis then curve synthesis (§7.2.4), writing N amplitudes into OUT."
  (declare (type vfloor1 f) (type (simple-array fixnum (*)) y)
           (type (simple-array double-float (*)) out) (type fixnum n))
  (let* ((values (f1-values f))
         (x (f1-x-list f))
         (range (aref +floor1-ranges+ (1- (f1-multiplier f))))
         (final (make-array values :element-type 'fixnum :initial-element 0))
         (step2 (make-array values :element-type 'bit :initial-element 0)))
    ;; ---- step 1: unwrap the differences against a running line prediction
    (setf (aref step2 0) 1 (aref step2 1) 1
          (aref final 0) (aref y 0) (aref final 1) (aref y 1))
    (loop for i of-type fixnum from 2 below values
          do (let ((low 0) (high 0))
               (declare (type fixnum low high))
               ;; low_neighbor / high_neighbor (§9.2.4-5): the nearest already-placed X on each side
               (let ((lv -1) (hv most-positive-fixnum))
                 (dotimes (k i)
                   (let ((xk (aref x k)))
                     (when (and (< xk (aref x i)) (> xk lv)) (setf lv xk low k))
                     (when (and (> xk (aref x i)) (< xk hv)) (setf hv xk high k)))))
               (let* ((predicted (%render-point (aref x low) (aref final low)
                                                (aref x high) (aref final high)
                                                (aref x i)))
                      (val (aref y i))
                      (highroom (- range predicted))
                      (lowroom predicted)
                      (room (* 2 (min highroom lowroom))))
                 (declare (type fixnum predicted val highroom lowroom room))
                 (if (zerop val)
                     (setf (aref step2 i) 0 (aref final i) predicted)
                     (progn
                       (setf (aref step2 low) 1 (aref step2 high) 1 (aref step2 i) 1)
                       (setf (aref final i)
                             (if (>= val room)
                                 (if (> highroom lowroom)
                                     (+ (- val lowroom) predicted)
                                     (- (+ predicted highroom) val 1))
                                 (if (oddp val)
                                     (- predicted (truncate (1+ val) 2))
                                     (+ predicted (truncate val 2))))))))))
    ;; the spec's suggested guard: a hostile setup can drive these out of range, and the table
    ;; lookup below is what would notice
    (dotimes (i values) (setf (aref final i) (max 0 (min (1- range) (aref final i)))))
    ;; ---- step 2: draw the polyline through the points that survived
    (let* ((order (f1-sorted f))
           (mult (f1-multiplier f))
           (lx 0) (hx 0)
           (ly (* (aref final (aref order 0)) mult))
           (hy 0))
      (declare (type fixnum lx hx ly hy mult))
      (loop for i of-type fixnum from 1 below values
            do (let ((k (aref order i)))
                 (when (plusp (aref step2 k))
                   (setf hy (* (aref final k) mult) hx (aref x k))
                   (%render-line lx ly hx hy out n)
                   (setf lx hx ly hy))))
      (if (< hx n)
          (%render-line hx hy n hy out n)
          (when (zerop hx)                     ; nothing was drawn at all
            (%render-line 0 ly n ly out n))))
    out))
