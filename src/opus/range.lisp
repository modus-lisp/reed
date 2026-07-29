;;;; src/opus/range.lisp — the Opus range decoder (RFC 6716 §4.1).
;;;;
;;;; The range coder is the shared entropy layer under both SILK and CELT.  It
;;;; is a byte-wise range decoder (Martin 1979 / Pasco 1976): symbols are drawn
;;;; from the front of the packet through a scaled arithmetic interval, while a
;;;; separate stream of "raw bits" is consumed from the BACK of the same packet
;;;; (ec_dec_bits), so the two never collide.  Everything here is a direct,
;;;; bit-exact port of libopus celt/entdec.c + entcode.c; all register math is
;;;; unsigned 32-bit (masked with +u32+).
(in-package #:reed)

;;; ---- constants (celt/mfrngcod.h) ----------------------------------------
(defconstant +ec-sym-bits+   8)
(defconstant +ec-code-bits+  32)
(defconstant +ec-sym-max+    255)                  ; (1<<8)-1
(defconstant +ec-code-top+   #x80000000)           ; 1<<31
(defconstant +ec-code-bot+   #x00800000)           ; top>>8
(defconstant +ec-code-extra+ 7)                    ; (32-2)%8+1
(defconstant +ec-uint-bits+  8)
(defconstant +ec-window-size+ 32)
(defconstant +u32+           #xFFFFFFFF)

(declaim (inline u32 ec-ilog))
(defun u32 (x) (logand x +u32+))

(defun ec-ilog (v)
  "Index of the most-significant 1 bit + 1 (0 for v=0); == 32-clz for u32."
  (integer-length (logand v +u32+)))

;;; ---- decoder context ----------------------------------------------------
(defstruct (ec-dec (:constructor %make-ec-dec))
  (buf        #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (storage    0 :type fixnum)   ; number of usable bytes in BUF from BASE
  (base       0 :type fixnum)   ; byte offset of this frame within BUF
  (offs       0 :type fixnum)   ; next range-coder byte (from the front)
  (end-offs   0 :type fixnum)   ; raw bits consumed from the back
  (end-window 0 :type (unsigned-byte 32))
  (nend-bits  0 :type fixnum)
  (nbits-total 0 :type fixnum)
  (rng        0 :type (unsigned-byte 32))
  (val        0 :type (unsigned-byte 32))
  (ext        0 :type (unsigned-byte 32))
  (rem        0 :type fixnum)
  (error      0 :type fixnum))

(declaim (inline ec-read-byte ec-read-byte-from-end))
(defun ec-read-byte (d)
  (let ((o (ec-dec-offs d)))
    (if (< o (ec-dec-storage d))
        (prog1 (aref (ec-dec-buf d) (+ (ec-dec-base d) o))
          (setf (ec-dec-offs d) (1+ o)))
        0)))

(defun ec-read-byte-from-end (d)
  (if (< (ec-dec-end-offs d) (ec-dec-storage d))
      (progn (incf (ec-dec-end-offs d))
             (aref (ec-dec-buf d)
                   (+ (ec-dec-base d) (- (ec-dec-storage d) (ec-dec-end-offs d)))))
      0))

;;; ---- normalization ------------------------------------------------------
(defun ec-dec-normalize (d)
  "Rescale RNG back into the high-order symbol, pulling input bytes as needed."
  (loop while (<= (ec-dec-rng d) +ec-code-bot+) do
    (let (sym)
      (incf (ec-dec-nbits-total d) +ec-sym-bits+)
      (setf (ec-dec-rng d) (u32 (ash (ec-dec-rng d) +ec-sym-bits+)))
      (setf sym (ec-dec-rem d))
      (setf (ec-dec-rem d) (ec-read-byte d))
      ;; take the low EC_CODE_EXTRA bits of (sym:rem)
      (setf sym (logand (ash (logior (ash sym +ec-sym-bits+) (ec-dec-rem d))
                             (- (- +ec-sym-bits+ +ec-code-extra+)))
                        +ec-sym-max+))
      (setf (ec-dec-val d)
            (logand (u32 (+ (u32 (ash (ec-dec-val d) +ec-sym-bits+))
                            (logand +ec-sym-max+ (lognot sym))))
                    (1- +ec-code-top+))))))

(defun ec-dec-init (buf &key (base 0) (storage (- (length buf) base)))
  "Initialize a range decoder over BUF[BASE, BASE+STORAGE)."
  (let ((d (%make-ec-dec :buf buf :base base :storage storage
                         :nbits-total (+ +ec-code-bits+ 1
                                         (- (* (floor (- +ec-code-bits+ +ec-code-extra+)
                                                      +ec-sym-bits+)
                                               +ec-sym-bits+)))
                         :rng (ash 1 +ec-code-extra+))))
    (setf (ec-dec-rem d) (ec-read-byte d))
    (setf (ec-dec-val d)
          (u32 (- (ec-dec-rng d) 1 (ash (ec-dec-rem d) (- (- +ec-sym-bits+ +ec-code-extra+))))))
    (ec-dec-normalize d)
    d))

;;; ---- symbol decoding ----------------------------------------------------
(defun ec-decode (d ft)
  "Return the cumulative frequency fs in [0,ft) for the next symbol."
  (setf (ec-dec-ext d) (floor (ec-dec-rng d) ft))
  (let ((s (floor (ec-dec-val d) (ec-dec-ext d))))
    (- ft (min (1+ s) ft))))

(defun ec-decode-bin (d bits)
  (setf (ec-dec-ext d) (ash (ec-dec-rng d) (- bits)))
  (let ((s (floor (ec-dec-val d) (ec-dec-ext d))))
    (- (ash 1 bits) (min (1+ s) (ash 1 bits)))))

(defun ec-dec-update (d fl fh ft)
  "Advance the decoder after ec-decode/ec-decode-bin, given the symbol [fl,fh)."
  (let ((s (u32 (* (ec-dec-ext d) (- ft fh)))))
    (setf (ec-dec-val d) (u32 (- (ec-dec-val d) s)))
    (setf (ec-dec-rng d)
          (if (> fl 0) (u32 (* (ec-dec-ext d) (- fh fl)))
              (u32 (- (ec-dec-rng d) s))))
    (ec-dec-normalize d)))

(defun ec-dec-bit-logp (d logp)
  "Decode a single bit whose probability of being 1 is 1/(1<<logp)."
  (let* ((r (ec-dec-rng d))
         (dd (ec-dec-val d))
         (s (ash r (- logp)))
         (ret (if (< dd s) 1 0)))
    (when (zerop ret) (setf (ec-dec-val d) (u32 (- dd s))))
    (setf (ec-dec-rng d) (if (= ret 1) s (u32 (- r s))))
    (ec-dec-normalize d)
    ret))

(defun ec-dec-icdf (d icdf ftb)
  "Decode a symbol from an inverse-CDF table (values scaled by 1<<ftb)."
  (let* ((s (ec-dec-rng d))
         (dd (ec-dec-val d))
         (r (ash s (- ftb)))
         (ret -1)
         (tt 0))
    (loop
      (setf tt s)
      (setf s (u32 (* r (aref icdf (incf ret)))))
      (when (>= dd s) (return)))
    (setf (ec-dec-val d) (u32 (- dd s)))
    (setf (ec-dec-rng d) (u32 (- tt s)))
    (ec-dec-normalize d)
    ret))

(defun ec-dec-icdf16 (d icdf ftb)
  "ec-dec-icdf variant for a 16-bit icdf table (used by the Laplace decoder)."
  (let* ((s (ec-dec-rng d))
         (dd (ec-dec-val d))
         (r (ash s (- ftb)))
         (ret -1)
         (tt 0))
    (loop
      (setf tt s)
      (setf s (u32 (* r (aref icdf (incf ret)))))
      (when (>= dd s) (return)))
    (setf (ec-dec-val d) (u32 (- dd s)))
    (setf (ec-dec-rng d) (u32 (- tt s)))
    (ec-dec-normalize d)
    ret))

;;; ---- raw bits (consumed from the back of the buffer) --------------------
(defun ec-dec-bits (d bits)
  "Read BITS raw bits from the end of the buffer (LSB-first packing)."
  (let ((window (ec-dec-end-window d))
        (available (ec-dec-nend-bits d)))
    (when (< available bits)
      (loop
        (setf window (u32 (logior window (ash (ec-read-byte-from-end d) available))))
        (incf available +ec-sym-bits+)
        (when (> available (- +ec-window-size+ +ec-sym-bits+)) (return))))
    (let ((ret (logand window (1- (ash 1 bits)))))
      (setf (ec-dec-end-window d) (u32 (ash window (- bits))))
      (setf (ec-dec-nend-bits d) (- available bits))
      (incf (ec-dec-nbits-total d) bits)
      ret)))

(defun ec-dec-uint (d ft)
  "Decode a uniformly distributed integer in [0,ft) (ft>1)."
  (assert (> ft 1))
  (let* ((ftm1 (1- ft))
         (ftb (ec-ilog ftm1)))
    (if (> ftb +ec-uint-bits+)
        (let* ((ftb2 (- ftb +ec-uint-bits+))
               (ft2 (1+ (ash ftm1 (- ftb2))))
               (s (ec-decode d ft2)))
          (ec-dec-update d s (1+ s) ft2)
          (let ((tt (logior (ash s ftb2) (ec-dec-bits d ftb2))))
            (if (<= tt ftm1) tt
                (progn (setf (ec-dec-error d) 1) ftm1))))
        (let* ((ft2 (1+ ftm1))
               (s (ec-decode d ft2)))
          (ec-dec-update d s (1+ s) ft2)
          s))))

;;; ---- introspection ------------------------------------------------------
(declaim (inline ec-tell))
(defun ec-tell (d)
  "Whole bits used so far (always a slight over-estimate)."
  (- (ec-dec-nbits-total d) (ec-ilog (ec-dec-rng d))))

(defun ec-tell-frac (d)
  "Fractional (1/8-bit) bit usage — needed by CELT's bit allocation."
  (let* ((correction #(35733 38967 42495 46340 50535 55109 60097 65535))
         (nbits (ash (ec-dec-nbits-total d) 3))    ; BITRES = 3
         (l (ec-ilog (ec-dec-rng d)))
         (r (u32 (ash (ec-dec-rng d) (- (- l 16)))))
         (b (- (ash r -12) 8)))
    (when (> r (aref correction b)) (incf b))
    (setf l (+ (ash l 3) b))
    (- nbits l)))
