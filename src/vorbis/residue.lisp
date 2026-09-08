;;;; src/vorbis/residue.lisp — the fine structure that gets multiplied by the floor.
;;;;
;;;; The residue is a spectrum's detail, coded in PARTITIONS of a few dozen values each.  Every
;;;; partition is given a class, the class chooses a codebook per pass, and up to eight passes
;;;; refine the same values — a cascade, so the first pass is coarse and cheap and later passes add
;;;; precision only where the encoder thought it was worth bits.  The class numbers themselves are
;;;; packed several to a codeword in a radix that the classbook's dimensions decide, which is why
;;;; the class read loop counts DOWN: the last class in a codeword occupies the least significant
;;;; digit.
;;;;
;;;; THE THREE FORMATS DIFFER ONLY IN HOW A PARTITION IS LAID OUT.  Format 0 scatters a codebook's
;;;; dimensions across the partition with a stride; format 1 lays them down consecutively; format 2
;;;; is format 1 applied to all channels interleaved into one vector, then deinterleaved.  Format 2
;;;; is what every real encoder uses for stereo, and its "interleaved" is per-sample round-robin,
;;;; not per-partition — get that backwards and the two channels swap content at partition
;;;; boundaries, which sounds like a phasing artefact rather than like a bug.
;;;;
;;;; Running out of packet in here is NORMAL (§8.6.2).  The encoder stops writing when the rest
;;;; would be zero, so the tail of the last partition is simply absent, and a decoder that treats
;;;; that as an error refuses most real files.

(in-package #:reed)

(defstruct (vresidue (:conc-name vr-))
  (type 0 :type fixnum)
  (begin 0 :type fixnum)
  (end 0 :type fixnum)
  (partition-size 1 :type fixnum)
  (classifications 1 :type fixnum)
  (classbook 0 :type fixnum)
  (books #() :type simple-vector))       ; [classification] -> vector of 8 book numbers or -1

(defun read-residue (v ncodebooks codebooks)
  "One residue configuration (§8.6.1)."
  (declare (type vbits v))
  (let ((type (vb v 16)))
    (unless (<= type 2) (vorbis-error "residue type ~d is not defined by Vorbis I" type))
    (let* ((begin (vb v 24))
           (end (vb v 24))
           (partition-size (1+ (vb v 24)))
           (classifications (1+ (vb v 6)))
           (classbook (vb v 8)))
      (unless (< classbook ncodebooks)
        (vorbis-error "residue: class book ~d is past the end of the codebook list" classbook))
      (let ((cb (aref codebooks classbook)))
        ;; the classbook has to be able to represent every class combination it will be asked for
        (when (> (expt classifications (vc-dimensions cb)) (vc-entries cb))
          (vorbis-error "residue: class book holds ~d entries, fewer than the ~d^~d combinations it must code"
                        (vc-entries cb) classifications (vc-dimensions cb))))
      (let ((cascade (make-array classifications :element-type 'fixnum :initial-element 0))
            (books (make-array classifications)))
        (dotimes (i classifications)
          (let* ((low (vb v 3))
                 (high (if (plusp (vb1 v)) (vb v 5) 0)))
            (setf (aref cascade i) (+ (* high 8) low))))
        (dotimes (i classifications)
          (let ((row (make-array 8 :initial-element -1)))
            (dotimes (j 8)
              (when (logbitp j (aref cascade i))
                (let ((b (vb v 8)))
                  (unless (< b ncodebooks)
                    (vorbis-error "residue: book ~d is past the end of the codebook list" b))
                  ;; §8.6.1: a residue book without a value mapping cannot produce a vector
                  (when (zerop (vc-lookup (aref codebooks b)))
                    (vorbis-error "residue: book ~d has no value mapping" b))
                  (setf (aref row j) b))))
            (setf (aref books i) row)))
        (when (vb-eop v) (vorbis-error "residue: setup packet ended early"))
        (make-vresidue :type type :begin begin :end end :partition-size partition-size
                       :classifications classifications :classbook classbook :books books)))))

(declaim (inline %vq-add))
(defun %vq-add (cb entry out base stride)
  "Add one codebook vector into OUT, its J-th scalar at BASE + J*STRIDE."
  (declare (type vcodebook cb) (type fixnum entry base stride)
           (type (simple-array double-float (*)) out) (optimize (speed 3) (safety 1)))
  (let ((vals (vc-values cb)) (dim (vc-dimensions cb)))
    (declare (type (simple-array double-float (*)) vals) (type fixnum dim))
    (let ((o (* entry dim)))
      (declare (type fixnum o))
      (dotimes (j dim)
        (let ((k (+ base (* j stride))))
          (declare (type fixnum k))
          (when (< -1 k (length out))
            (incf (aref out k) (aref vals (+ o j)))))))))

(defun %residue-partition (cb v out offset n type)
  "One partition, laid out as the format asks (§8.6.3-4).  Returns NIL at end of packet."
  (declare (type vcodebook cb) (type vbits v) (type (simple-array double-float (*)) out)
           (type fixnum offset n type))
  (let ((dim (vc-dimensions cb)))
    (declare (type fixnum dim))
    (if (zerop type)
        ;; format 0: the codebook's dimensions are spread across the partition with a stride
        (let ((step (floor n dim)))
          (declare (type fixnum step))
          (dotimes (i step t)
            (let ((e (vc-scalar cb v)))
              (when (minusp e) (return nil))
              (%vq-add cb e out (+ offset i) step))))
        ;; formats 1 and 2: consecutive
        (let ((i 0))
          (declare (type fixnum i))
          (loop while (< i n)
                do (let ((e (vc-scalar cb v)))
                     (when (minusp e) (return-from %residue-partition nil))
                     (%vq-add cb e out (+ offset i) 1)
                     (incf i dim)))
          t))))

(defun residue-decode (r v codebooks vectors do-not-decode n)
  "Decode a residue into VECTORS, one double-float array per channel (§8.6.2).

   VECTORS are already allocated and zeroed; DO-NOT-DECODE is a bit per channel.  N is the
   half-blocksize — the length of one channel's spectrum."
  (declare (type vresidue r) (type vbits v) (type simple-vector vectors codebooks)
           (type simple-vector do-not-decode) (type fixnum n))
  (let* ((ch (length vectors))
         (type (vr-type r)))
    (if (= type 2)
        ;; format 2: one interleaved vector for every channel at once, then deinterleave
        (let ((any (loop for j below ch thereis (not (aref do-not-decode j)))))
          (let ((inter (make-array (* ch n) :element-type 'double-float :initial-element 0d0)))
            (when any
              (%residue-1 r v codebooks (vector inter) (vector nil) (* ch n)))
            (dotimes (i n)
              (dotimes (j ch)
                (setf (aref (the (simple-array double-float (*)) (aref vectors j)) i)
                      (aref inter (+ (* i ch) j)))))))
        (%residue-1 r v codebooks vectors do-not-decode n))
    vectors))

(defun %residue-1 (r v codebooks vectors do-not-decode actual-size)
  "The decode loop shared by all three formats, over ACTUAL-SIZE scalars per vector."
  (declare (type vresidue r) (type vbits v) (type simple-vector vectors codebooks do-not-decode)
           (type fixnum actual-size))
  (let* ((ch (length vectors))
         (begin (min (vr-begin r) actual-size))
         (end (min (vr-end r) actual-size))
         (psize (vr-partition-size r))
         (n-to-read (- end begin)))
    (declare (type fixnum ch begin end psize n-to-read))
    (when (<= n-to-read 0) (return-from %residue-1 vectors))
    (let* ((classbook (aref codebooks (vr-classbook r)))
           (cpc (vc-dimensions classbook))
           (nparts (floor n-to-read psize))
           (classes (make-array (list ch (+ nparts cpc)) :element-type 'fixnum
                                                         :initial-element 0)))
      (declare (type fixnum cpc nparts))
      (dotimes (pass 8 vectors)
        (let ((pc 0))
          (declare (type fixnum pc))
          (loop while (< pc nparts)
                do (when (zerop pass)
                     ;; one codeword carries CPC class numbers, least significant last
                     (dotimes (j ch)
                       (unless (aref do-not-decode j)
                         (let ((temp (vc-scalar classbook v)))
                           (when (minusp temp) (return-from %residue-1 vectors))
                           (loop for i of-type fixnum downfrom (1- cpc) to 0
                                 do (setf (aref classes j (+ i pc))
                                          (mod temp (vr-classifications r)))
                                    (setf temp (floor temp (vr-classifications r))))))))
                   (dotimes (i cpc)
                     (when (< pc nparts)
                       (dotimes (j ch)
                         (unless (aref do-not-decode j)
                           (let* ((vqclass (aref classes j pc))
                                  (vqbook (aref (aref (vr-books r) vqclass) pass)))
                             (unless (minusp vqbook)
                               (unless (%residue-partition
                                        (aref codebooks vqbook) v (aref vectors j)
                                        (+ begin (* pc psize)) psize (vr-type r))
                                 (return-from %residue-1 vectors))))))
                       (incf pc)))))))))
