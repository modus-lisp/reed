;;;; src/vorbis/codebook.lisp — the entropy codebooks, and the vector quantiser hiding inside them.
;;;;
;;;; A Vorbis codebook is two things wearing one name.  It is a Huffman code, which is what the
;;;; floor and the residue partition selectors read through; and it is optionally a VECTOR
;;;; QUANTISER, where the entry number that comes out of the Huffman code indexes a table of
;;;; vectors rather than being a value.  Residue decode uses the second almost exclusively — one
;;;; codeword yields four or eight spectral values at once — which is most of why a Vorbis packet
;;;; is so much smaller than the number of values it produces.
;;;;
;;;; THE CODEWORD ASSIGNMENT IS THE PART TO GET RIGHT.  The stream gives lengths, not codewords:
;;;; each used entry takes "the lowest valued unused binary Huffman codeword" of its length, in
;;;; ENTRY ORDER (§3.2.1).  Entry order, not length order — the same trap Theora's VLC reader fell
;;;; into in this repository, where a table that happens to be sorted by length hides the bug until
;;;; one that is not sorted arrives.
;;;;
;;;; Rather than the reference implementation's carry-propagating marker array, the assignment here
;;;; keeps the FREE SUBTREES of the decision tree in a list ordered left to right.  Taking a
;;;; codeword of length L means taking the first free subtree no deeper than L and splitting it down
;;;; to L, putting the right-hand siblings back where it was.  The list stays ordered because a
;;;; subtree's descendants all sort between it and whatever followed it, so "the first free subtree"
;;;; IS "the lowest unused codeword" by construction rather than by argument — and both error
;;;; conditions the spec names fall straight out of it: a list that empties early is an
;;;; over-populated tree, one that is not empty at the end is an under-populated one.

(in-package #:reed)

(defstruct (vcodebook (:conc-name vc-))
  (dimensions 0 :type fixnum)
  (entries 0 :type fixnum)
  (tree #.(make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (single -1 :type fixnum)              ; the sole entry of a single-entry book, else -1
  (lookup 0 :type fixnum)               ; 0 none, 1 lattice, 2 explicit
  (values nil))                         ; (entries*dimensions) doubles when LOOKUP is 1 or 2

;;; ---- the decision tree -------------------------------------------------------------------------
;;;
;;; Two fixnums per node: the branch for a zero bit and the branch for a one.  A positive value is
;;; the base index of another node, a negative value is the leaf -(entry+1), and zero is "nothing
;;; here" — which a conforming stream can only reach in the single-entry case the 2015 errata
;;; legalised, and which is why zero cannot also mean the root.  It does not: the root is node zero
;;; and the root is nobody's child.

(defun %build-huffman (lengths entries)
  "Assign codewords in entry order.  Returns the tree, how many entries were used, and whatever
   free subtrees are left over — which the caller must find empty."
  (declare (type (simple-array fixnum (*)) lengths) (type fixnum entries))
  (let ((tree (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 2
                             :initial-element 0))
        ;; the root's two branches, left to right; a codeword is never shorter than one bit
        (free (list (cons 1 0) (cons 1 1)))
        (used 0))
    (declare (type fixnum used))
    (dotimes (e entries)
      (let ((len (aref lengths e)))
        (declare (type fixnum len))
        (when (plusp len)
          (incf used)
          (let ((prev nil) (cell free))
            (loop while (and cell (> (the fixnum (car (first cell))) len))
                  do (setf prev cell cell (rest cell)))
            (unless cell
              (vorbis-error "codebook: over-populated Huffman tree at entry ~d" e))
            (let* ((d (car (first cell))) (s (cdr (first cell))) (siblings '()))
              (declare (type fixnum d s))
              ;; split down to LEN, keeping each right-hand branch as a free subtree.  PUSHing them
              ;; leaves the list head-first in left-to-right order, which is what the splice wants.
              (loop while (< d len)
                    do (let ((n (fill-pointer tree)))
                         (declare (type fixnum n))
                         (vector-push-extend 0 tree)
                         (vector-push-extend 0 tree)
                         (setf (aref tree s) n)
                         (push (cons (1+ d) (1+ n)) siblings)
                         (setf s n d (1+ d))))
              (setf (aref tree s) (- (1+ e)))
              (let ((tail (rest cell)))
                (cond (siblings
                       (setf (first cell) (first siblings))
                       (setf (rest cell) (nconc (rest siblings) tail)))
                      (prev (setf (rest prev) tail))
                      (t (setf free tail)))))))))
    (values (coerce tree '(simple-array fixnum (*))) used free)))

;;; ---- reading one ------------------------------------------------------------------------------

(defun vc-scalar (cb v)
  "One entry number, or -1 if the packet ended first."
  (declare (type vcodebook cb) (type vbits v) (optimize (speed 3) (safety 1)))
  (let ((single (vc-single cb)))
    (declare (type fixnum single))
    (unless (minusp single)
      ;; the errata's single-entry book: sink one bit, and do not care what it was
      (vb v 1)
      (return-from vc-scalar (if (vb-eop v) -1 single))))
  (let ((tree (vc-tree cb)) (n 0))
    (declare (type (simple-array fixnum (*)) tree) (type fixnum n))
    (loop
      (let ((bit (vb v 1)))
        (when (vb-eop v) (return-from vc-scalar -1))
        (let ((next (aref tree (+ n bit))))
          (declare (type fixnum next))
          (cond ((minusp next) (return-from vc-scalar (- (- next) 1)))
                ((zerop next) (return-from vc-scalar -1))
                (t (setf n next))))))))

;;; ---- the vector quantiser ----------------------------------------------------------------------

(defun %vq-values (lookup entries dimensions multiplicands minimum delta sequence-p lookup-values)
  "Expand the multiplicands into ENTRIES vectors of DIMENSIONS scalars (§3.2.1).

   SEQUENCE-P makes each scalar relative to the one before it, which is how a book that codes a
   rising curve — a floor, most often — costs little more than the differences."
  (declare (type fixnum lookup entries dimensions lookup-values)
           (type (simple-array fixnum (*)) multiplicands)
           (type double-float minimum delta))
  (let ((out (make-array (* entries dimensions) :element-type 'double-float
                                                :initial-element 0d0)))
    (dotimes (e entries out)
      (let ((last 0d0) (o (* e dimensions)))
        (declare (type double-float last) (type fixnum o))
        (if (= lookup 1)
            ;; lattice: the entry number is a mixed-radix index into one shared scalar list
            (let ((divisor 1))
              (declare (type fixnum divisor))
              (dotimes (i dimensions)
                (let* ((off (mod (floor e divisor) lookup-values))
                       (val (+ (* (aref multiplicands off) delta) minimum last)))
                  (setf (aref out (+ o i)) val)
                  (when sequence-p (setf last val))
                  (setf divisor (* divisor lookup-values)))))
            ;; explicit: DIMENSIONS scalars per entry, written out in full
            (let ((off (* e dimensions)))
              (declare (type fixnum off))
              (dotimes (i dimensions)
                (let ((val (+ (* (aref multiplicands (+ off i)) delta) minimum last)))
                  (setf (aref out (+ o i)) val)
                  (when sequence-p (setf last val))))))))))

(defun read-codebook (v)
  "One packed codebook (§3.2.1)."
  (declare (type vbits v))
  (let ((sync (vb v 24)))
    (unless (= sync #x564342)
      (vorbis-error "codebook: sync pattern is #x~6,'0x, not #x564342" sync)))
  (let* ((dimensions (vb v 16))
         (entries (vb v 24))
         (ordered (plusp (vb1 v)))
         (lengths (make-array (max 1 entries) :element-type 'fixnum :initial-element 0)))
    (when (zerop dimensions) (vorbis-error "codebook: zero dimensions"))
    (when (zerop entries) (vorbis-error "codebook: zero entries"))
    (if ordered
        ;; lengths ascending: the stream gives a run length per codeword length, so a book whose
        ;; lengths are already sorted costs a few bits instead of five per entry
        (let ((entry 0) (len (1+ (vb v 5))))
          (declare (type fixnum entry len))
          (loop while (< entry entries)
                do (let ((number (vb v (ilog (- entries entry)))))
                     (when (> (+ entry number) entries)
                       (vorbis-error "codebook: ordered length run overruns the entry count"))
                     (dotimes (i number) (setf (aref lengths (+ entry i)) len))
                     (incf entry number)
                     (incf len)
                     (when (> len 33) (vorbis-error "codebook: codeword longer than 32 bits"))
                     (when (vb-eop v)
                       (vorbis-error "codebook: packet ended inside the ordered length list")))))
        (let ((sparse (plusp (vb1 v))))
          (dotimes (e entries)
            (if sparse
                (when (plusp (vb1 v)) (setf (aref lengths e) (1+ (vb v 5))))
                (setf (aref lengths e) (1+ (vb v 5)))))))
    (when (vb-eop v) (vorbis-error "codebook: packet ended inside the length list"))
    (multiple-value-bind (tree used free) (%build-huffman lengths entries)
      (let ((single -1))
        (cond ((zerop used) (vorbis-error "codebook: no used entries"))
              ((= used 1)
               ;; the 2015 errata: one used entry is legal and must have been coded with length 1
               (let ((e (position 1 lengths)))
                 (unless e (vorbis-error "codebook: single used entry with length /= 1"))
                 (setf single e)))
              (free (vorbis-error "codebook: under-populated Huffman tree")))
        (let ((lookup (vb v 4))
              (values nil))
          (when (> lookup 2) (vorbis-error "codebook: lookup type ~d is reserved" lookup))
          (when (plusp lookup)
            (let* ((minimum (float32-unpack (vb v 32)))
                   (delta (float32-unpack (vb v 32)))
                   (value-bits (1+ (vb v 4)))
                   (sequence-p (plusp (vb1 v)))
                   (lookup-values (if (= lookup 1)
                                      (lookup1-values entries dimensions)
                                      (* entries dimensions)))
                   (multiplicands (make-array (max 1 lookup-values) :element-type 'fixnum
                                                                    :initial-element 0)))
              (when (> (* entries dimensions) (ash 1 22))
                (vorbis-error "codebook: a ~d by ~d value table is not a real stream"
                              entries dimensions))
              (dotimes (i lookup-values) (setf (aref multiplicands i) (vb v value-bits)))
              (when (vb-eop v) (vorbis-error "codebook: packet ended inside the lookup table"))
              (setf values (%vq-values lookup entries dimensions multiplicands
                                       minimum delta sequence-p lookup-values))))
          (make-vcodebook :dimensions dimensions :entries entries :tree tree :single single
                          :lookup lookup :values values))))))
