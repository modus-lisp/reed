;;;; src/huffman.lisp — Layer III Huffman decoding.
;;;;
;;;; The 32 big-value tables plus the two count1 (quad) tables A/B are stored as
;;;; binary decode trees in +huffman-tree+ (see tables.lisp).  A node is a 16-bit
;;;; word: high byte = left (bit 0) child step, low byte = right (bit 1) child
;;;; step; a word with a zero high byte is a leaf carrying x=(w>>4)&15, y=w&15.
;;;; Steps of 250+ are followed repeatedly to reach children beyond one byte.
(in-package #:reed)

;; Per-table (offset into +huffman-tree+, tree length, linbits).  Index = table
;; number 0..33; tables 4 and 14 are empty.  Note: quad table B (33) lives at
;; offset 2773 (a well-known off-by-one exists in some reference sources).
(declaim (type (simple-array fixnum (34)) +huff-offset+ +huff-treelen+ +huff-linbits+))
(defparameter +huff-offset+
  (make-array 34 :element-type 'fixnum :initial-contents
   '(0 0 7 24 0 41 72 103 174 245 316 443 570 697 0 1208
     1719 1719 1719 1719 1719 1719 1719 1719 2230 2230 2230 2230 2230 2230 2230 2230
     2742 2773)))
(defparameter +huff-treelen+
  (make-array 34 :element-type 'fixnum :initial-contents
   '(0 7 17 17 0 31 31 71 71 71 127 127 127 511 0 511
     511 511 511 511 511 511 511 511 512 512 512 512 512 512 512 512
     31 31)))
(defparameter +huff-linbits+
  (make-array 34 :element-type 'fixnum :initial-contents
   '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
     1 2 3 4 6 8 10 13 4 5 6 7 8 9 11 13
     0 0)))

(defun huffman-decode (br table-num)
  "Decode one Huffman codeword from BR with TABLE-NUM.  Returns (values x y v w);
v,w are non-zero only for the count1 quad tables (32,33)."
  (declare (type bitreader br) (type (integer 0 33) table-num)
           (optimize (speed 3) (safety 1)))
  (let ((treelen (aref +huff-treelen+ table-num)))
    (declare (type fixnum treelen))
    (when (zerop treelen)
      (return-from huffman-decode (values 0 0 0 0)))
    (let ((tree +huffman-tree+)
          (off (aref +huff-offset+ table-num))
          (linbits (aref +huff-linbits+ table-num))
          (point 0) (x 0) (y 0) (v 0) (w 0) (bitsleft 32))
      (declare (type octets-u16 tree)
               (type fixnum off linbits point x y v w bitsleft))
      (loop
        (let ((node (aref tree (+ off point))))
          (declare (type (unsigned-byte 16) node))
          (when (zerop (logand node #xff00))     ; leaf?
            (setf x (logand (ash node -4) #xf)
                  y (logand node #xf))
            (return))
          (if (= 1 (read-bit br))                ; go right (low byte)
              (progn
                (loop while (>= (logand (aref tree (+ off point)) #xff) 250)
                      do (incf point (logand (aref tree (+ off point)) #xff)))
                (incf point (logand (aref tree (+ off point)) #xff)))
              (progn                              ; go left (high byte)
                (loop while (>= (ash (aref tree (+ off point)) -8) 250)
                      do (incf point (ash (aref tree (+ off point)) -8)))
                (incf point (ash (aref tree (+ off point)) -8))))
          (decf bitsleft)
          (when (or (<= bitsleft 0) (>= point treelen)) (return))))
      (cond
        ((> table-num 31)                         ; count1 quad tables
         (setf v (logand (ash y -3) 1)
               w (logand (ash y -2) 1)
               x (logand (ash y -1) 1)
               y (logand y 1))
         (when (and (> v 0) (= 1 (read-bit br))) (setf v (- v)))
         (when (and (> w 0) (= 1 (read-bit br))) (setf w (- w)))
         (when (and (> x 0) (= 1 (read-bit br))) (setf x (- x)))
         (when (and (> y 0) (= 1 (read-bit br))) (setf y (- y))))
        (t                                        ; big-value tables
         (when (and (> linbits 0) (= x 15)) (incf x (read-bits br linbits)))
         (when (and (> x 0) (= 1 (read-bit br))) (setf x (- x)))
         (when (and (> linbits 0) (= y 15)) (incf y (read-bits br linbits)))
         (when (and (> y 0) (= 1 (read-bit br))) (setf y (- y)))))
      (values x y v w))))
