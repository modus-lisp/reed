;;;; src/aac/huffman.lisp --- AAC Huffman decoding, dequantization helpers,
;;;; and the analysis/synthesis windows (sine + Kaiser-Bessel derived).
(in-package #:reed)

;;; ------------------------------------------------------------------ ;;;
;;; Prefix-code decoders                                                ;;;
;;; ------------------------------------------------------------------ ;;;
;;;
;;; Each AAC codeword is prefix free, so we index a hash table by the value
;;; (1 << length) | codeword: reading bits MSB-first and appending them to a
;;; register seeded with 1 reproduces exactly that key at the terminating bit.

(defun %aac-build-decoder (codes bits)
  (let ((h (make-hash-table :test 'eql :size (* 2 (length codes)))))
    (dotimes (i (length codes) h)
      (setf (gethash (logior (ash 1 (aref bits i)) (aref codes i)) h) i))))

(defvar *aac-spec-decoders* nil
  "Vector of 11 hash tables (codebooks 1..11), lazily built.")
(defvar *aac-sf-decoder* nil "Scalefactor codebook decoder.")

(defun aac-spec-decoder (cb0)
  "CB0 is the 0-based codebook (0 => AAC codebook 1)."
  (unless *aac-spec-decoders*
    (setf *aac-spec-decoders*
          (let ((v (make-array 11)))
            (dotimes (i 11 v)
              (setf (aref v i)
                    (%aac-build-decoder (aref +aac-spec-codes+ i)
                                        (aref +aac-spec-bits+ i)))))))
  (aref *aac-spec-decoders* cb0))

(defun aac-sf-decoder ()
  (or *aac-sf-decoder*
      (setf *aac-sf-decoder* (%aac-build-decoder +aac-sf-codes+ +aac-sf-bits+))))

(declaim (inline aac-huff-decode))
(defun aac-huff-decode (br table)
  "Decode one symbol index from BR using prefix-code hash TABLE."
  (declare (type bitreader br))
  (let ((node 1))
    (declare (type fixnum node))
    (loop
      (setf node (logior (ash node 1) (read-bit br)))
      (let ((s (gethash node table)))
        (when s (return s))))))

(defun aac-decode-scalefactor (br)
  "Decode one differential scalefactor value (already offset by -60)."
  (- (aac-huff-decode br (aac-sf-decoder)) 60))

;;; ------------------------------------------------------------------ ;;;
;;; x^(4/3) inverse-quantization table                                  ;;;
;;; ------------------------------------------------------------------ ;;;

(defparameter +aac-pow43-max+ 8192)
(defvar *aac-pow43* nil)

(defun aac-pow43 (q)
  "|q|^(4/3) for non-negative integer Q (double-float)."
  (declare (type (integer 0) q))
  (unless *aac-pow43*
    (let ((tab (make-array +aac-pow43-max+ :element-type 'double-float)))
      (dotimes (i +aac-pow43-max+)
        (setf (aref tab i) (expt (coerce i 'double-float) 4/3)))
      (setf *aac-pow43* tab)))
  (if (< q +aac-pow43-max+)
      (aref (the (simple-array double-float (*)) *aac-pow43*) q)
      (expt (coerce q 'double-float) 4/3)))

;;; ------------------------------------------------------------------ ;;;
;;; Spectral Huffman decode of one scalefactor band                     ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-read-escape (br)
  "Codebook-11 escape sequence: N ones, a zero, then N+4 magnitude bits."
  (declare (type bitreader br))
  (let ((n 0))
    (declare (type fixnum n))
    (loop while (= 1 (read-bit br)) do (incf n))
    (let ((b (+ n 4)))
      (+ (ash 1 b) (read-bits br b)))))

(defun aac-decode-spectral-band (br cb0 dst base n sf)
  "Decode N quantized spectral coefficients of codebook CB0 (0-based) into the
double array DST[BASE..BASE+N), dequantized: sign*|q|^(4/3) * SF."
  (declare (type bitreader br) (type fixnum cb0 base n)
           (type double-float sf)
           (type (simple-array double-float (*)) dst))
  (let* ((params (aref +aac-cb-params+ cb0))
         (dim (aref params 0))
         (unsignedp (= 1 (aref params 1)))
         (modu (aref params 2))
         (off (aref params 3))
         (escp (= 1 (aref params 4)))
         (table (aac-spec-decoder cb0))
         (m2 (* modu modu))
         (m3 (* m2 modu))
         (tup (make-array 4 :element-type 'fixnum)))
    (declare (type fixnum dim modu off m2 m3))
    (do ((i 0 (+ i dim)))
        ((>= i n))
      (let ((idx (aac-huff-decode br table)))
        (declare (type fixnum idx))
        ;; expand symbol index into DIM tuple values
        (if (= dim 4)
            (progn
              (setf (aref tup 0) (- (floor idx m3) off)
                    (aref tup 1) (- (mod (floor idx m2) modu) off)
                    (aref tup 2) (- (mod (floor idx modu) modu) off)
                    (aref tup 3) (- (mod idx modu) off)))
            (setf (aref tup 0) (- (floor idx modu) off)
                  (aref tup 1) (- (mod idx modu) off)))
        ;; Signs are transmitted immediately after the codeword (one bit per
        ;; non-zero value, in order); escape magnitudes for codebook 11 follow
        ;; *after* all sign bits.  (ISO 14496-3 4.6.3.3.)
        (let ((s0 0) (s1 0) (s2 0) (s3 0))
          (declare (type fixnum s0 s1 s2 s3))
          (when unsignedp
            (when (/= (aref tup 0) 0) (setf s0 (read-bit br)))
            (when (/= (aref tup 1) 0) (setf s1 (read-bit br)))
            (when (= dim 4)
              (when (/= (aref tup 2) 0) (setf s2 (read-bit br)))
              (when (/= (aref tup 3) 0) (setf s3 (read-bit br)))))
          (dotimes (d dim)
            (let ((v (aref tup d))
                  (sgn (case d (0 s0) (1 s1) (2 s2) (t s3))))
              (declare (type fixnum v sgn))
              (when (and escp (= v 16))       ; codebook 11 escape magnitude
                (setf v (aac-read-escape br)))
              (when (= sgn 1) (setf v (- v)))
              (setf (aref dst (+ base i d))
                    (if (>= v 0)
                        (* sf (aac-pow43 v))
                        (* (- sf) (aac-pow43 (- v))))))))))))

;;; ------------------------------------------------------------------ ;;;
;;; Windows: sine and Kaiser-Bessel derived                             ;;;
;;; ------------------------------------------------------------------ ;;;

(defun %bessel-i0 (x)
  "Modified Bessel function of the first kind, order 0."
  (declare (type double-float x))
  (let ((sum 1d0) (term 1d0) (y (* 0.25d0 x x)))
    (declare (type double-float sum term y))
    (loop for k of-type fixnum from 1 below 60
          do (setf term (* term (/ y (* k k))))
             (incf sum term)
          while (> term (* sum 1d-18)))
    sum))

(defun %sine-half (n2)
  "Rising half (length N2) of a sine window of total length 2*N2."
  (let ((w (make-array n2 :element-type 'double-float))
        (nn (* 2d0 n2)))
    (dotimes (k n2 w)
      (setf (aref w k) (sin (* (/ pi nn) (+ k 0.5d0)))))))

(defun %kbd-half (n alpha)
  "Kaiser-Bessel derived window of length N (the AAC half window), alpha in
half-integer units of pi.  Mirrors ffmpeg/ISO kbd_window_init."
  (let* ((temp (make-array (1+ (floor n 2)) :element-type 'double-float))
         (w (make-array n :element-type 'double-float))
         (a (/ (* alpha pi) n))
         (alpha2 (* 4d0 a a))
         (scale 0d0) (sum 0d0))
    (declare (type double-float alpha2 scale sum))
    (loop for i from 0 to (floor n 2)
          do (setf (aref temp i)
                   (%bessel-i0 (sqrt (* i (- n i) alpha2))))
             (incf scale (* (aref temp i)
                            (if (and (/= i 0) (< i (floor n 2))) 2d0 1d0))))
    (setf scale (/ 1d0 (+ scale 1d0)))
    (loop for i from 0 to (floor n 2)
          do (incf sum (aref temp i))
             (setf (aref w i) (sqrt (* sum scale))))
    (loop for i from (1+ (floor n 2)) below n
          do (incf sum (aref temp (- n i)))
             (setf (aref w i) (sqrt (* sum scale))))
    w))

(defun %full-window (half)
  "Build the full symmetric window (length 2*N2) from its rising half."
  (let* ((n2 (length half))
         (nn (* 2 n2))
         (w (make-array nn :element-type 'double-float)))
    (dotimes (k n2 w)
      (setf (aref w k) (aref half k)
            (aref w (- nn 1 k)) (aref half k)))))

;; Full-length windows (long = 2048, short = 256), sine and KBD, cached.
(defvar *win-sine-long*) (defvar *win-kbd-long*)
(defvar *win-sine-short*) (defvar *win-kbd-short*)
(defvar *aac-windows-ready* nil)

(defun aac-ensure-windows ()
  (unless *aac-windows-ready*
    (setf *win-sine-long*  (%full-window (%sine-half 1024))
          *win-kbd-long*   (%full-window (%kbd-half 1024 4d0))
          *win-sine-short* (%full-window (%sine-half 128))
          *win-kbd-short*  (%full-window (%kbd-half 128 6d0))
          *aac-windows-ready* t)))

(defun aac-long-window (kbd-p)
  (aac-ensure-windows)
  (if kbd-p *win-kbd-long* *win-sine-long*))
(defun aac-short-window (kbd-p)
  (aac-ensure-windows)
  (if kbd-p *win-kbd-short* *win-sine-short*))
