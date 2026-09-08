;;;; src/vorbis/decode.lisp — one audio packet, and the lapped transform that joins them.
;;;;
;;;; A Vorbis frame is a floor times a residue, inverse-transformed, windowed, and overlapped with
;;;; the frame before it.  The overlap is the part with the sharp edges.
;;;;
;;;; BLOCK SIZES CHANGE FROM FRAME TO FRAME.  A mode is either long or short, and the encoder
;;;; switches to short blocks where the signal has a transient so that quantisation noise cannot
;;;; smear backwards in time before the attack.  Two consecutive blocks of different sizes still
;;;; have to overlap exactly, which is what the HYBRID window shapes are for: a long block next to a
;;;; short one confines its slope to a short-block-sized region, and is flat one either side of it.
;;;; A long window has to know what its NEIGHBOURS are, which is why a long packet carries two extra
;;;; bits saying whether the frames either side were long.
;;;;
;;;; THE FRAME BOUNDARY IS THE WINDOW CENTRE, not the block boundary.  Consecutive centres are
;;;; (prev_n + n)/4 samples apart, and that is how many samples a frame contributes.  With equal
;;;; block sizes that is the familiar n/2 and the previous block's tail lines up with the current
;;;; block's head; with unequal ones the current block starts (n - prev_n)/4 samples before or after
;;;; the previous centre, which is negative half the time.  Getting this wrong does not produce
;;;; silence or noise — it produces audio that is subtly wrong only where the encoder switched
;;;; blocks, which is exactly at the transients where it is hardest to hear and most annoying.

(in-package #:reed)

;;; ---- windows -----------------------------------------------------------------------------------

(defun %vorbis-window (n bs0 blockflag prev-long next-long)
  "The window for one block (§4.3.1).  Slope y = sin(pi/2 * sin^2(pi/2 * (x+1/2)/len))."
  (declare (type fixnum n bs0))
  (let* ((w (make-array n :element-type 'double-float :initial-element 0d0))
         (centre (ash n -1))
         (lws (if (and blockflag (not prev-long)) (- (ash n -2) (ash bs0 -2)) 0))
         (lwe (if (and blockflag (not prev-long)) (+ (ash n -2) (ash bs0 -2)) centre))
         (ln (if (and blockflag (not prev-long)) (ash bs0 -1) (ash n -1)))
         (rws (if (and blockflag (not next-long)) (- (* 3 (ash n -2)) (ash bs0 -2)) centre))
         (rwe (if (and blockflag (not next-long)) (+ (* 3 (ash n -2)) (ash bs0 -2)) n))
         (rn (if (and blockflag (not next-long)) (ash bs0 -1) (ash n -1))))
    (declare (type fixnum centre lws lwe ln rws rwe rn))
    (loop for i of-type fixnum from lws below lwe
          do (let ((x (sin (* (/ pi 2d0) (/ (+ (- i lws) 0.5d0) ln)))))
               (setf (aref w i) (sin (* (/ pi 2d0) x x)))))
    (loop for i of-type fixnum from lwe below rws do (setf (aref w i) 1d0))
    (loop for i of-type fixnum from rws below rwe
          do (let ((x (sin (+ (* (/ pi 2d0) (/ (+ (- i rws) 0.5d0) rn)) (/ pi 2d0)))))
               (setf (aref w i) (sin (* (/ pi 2d0) x x)))))
    w))

;;; ---- the inverse transform ----------------------------------------------------------------------
;;;
;;; A DIRECT SUM, not a fast one.  N/2 coefficients times N outputs is a million multiply-adds for a
;;; 2048-sample block, which is far more than an FFT-based MDCT would cost — but it is the
;;; definition, written the way the definition is written, and a fast MDCT is a thing to check
;;; against something known to be right rather than a thing to start with.
;;;
;;; THE NAME IS QUALIFIED BECAUSE REED IS ONE FLAT PACKAGE across every codec in it.  Calling this
;;; %IMDCT redefined AAC's inverse transform, which takes six arguments rather than three, and the
;;; AAC suite failed with "invalid number of arguments: 6" from inside a file this work never
;;; touched.  A flat package means every internal name is a global name.

(defvar *vorbis-imdct-tables* (make-hash-table)
  "N -> the cosine matrix for a block of that size, built once and shared.")

(defun %vorbis-imdct-table (n)
  (or (gethash n *vorbis-imdct-tables*)
      (setf (gethash n *vorbis-imdct-tables*)
            (let ((tab (make-array (* n (ash n -1)) :element-type 'double-float)))
              (dotimes (i n tab)
                (dotimes (k (ash n -1))
                  (setf (aref tab (+ (* i (ash n -1)) k))
                        (cos (/ (* pi (+ (* 2 i) 1 (ash n -1)) (+ (* 2 k) 1))
                                (* 2d0 n))))))))))

(defun %vorbis-imdct (spectrum n out)
  "y[i] = sum_k X[k] cos(pi (2i+1+n/2)(2k+1) / 2n), the inverse of the MDCT Vorbis uses.

   NO 4/n.  The textbook inverse MDCT carries a 4/N normalisation, and the reference encoder folds
   exactly that factor into its FORWARD transform instead — so the coefficients in the bitstream are
   already scaled and applying it again here makes the output five hundred times too quiet.  The
   giveaway is a correlation of 0.999 against the reference with a relative RMS error of 0.998,
   which is what a pure gain error looks like and nothing else does."
  (declare (type (simple-array double-float (*)) spectrum out) (type fixnum n)
           (optimize (speed 3) (safety 1)))
  (let ((tab (the (simple-array double-float (*)) (%vorbis-imdct-table n)))
        (half (ash n -1)))
    (declare (type fixnum half))
    (dotimes (i n out)
      (let ((acc 0d0) (base (* i half)))
        (declare (type double-float acc) (type fixnum base))
        (dotimes (k half)
          (incf acc (* (aref spectrum k) (aref tab (+ base k)))))
        (setf (aref out i) acc)))))

;;; ---- decoder state -------------------------------------------------------------------------------

(defstruct (vorbis-decoder (:conc-name vd-) (:constructor %make-vorbis-decoder))
  (setup nil :type (or null vorbis-setup))
  (lap #() :type simple-vector)         ; per channel: the previous block past its centre
  (prev-n 0 :type fixnum)               ; that block's size, or zero before the first frame
  (windows (make-hash-table :test #'equal))
  ;; COVERAGE, not statistics.  Short blocks and coupled channels are paths a fixture either
  ;; exercises or does not, and a fixture re-encoded with different settings can stop exercising
  ;; one without any test failing.  The suite asserts on these.
  (long-blocks 0 :type fixnum)
  (short-blocks 0 :type fixnum)
  (hybrid-windows 0 :type fixnum)
  (coupled-blocks 0 :type fixnum)
  (unused-channels 0 :type fixnum))

(defun make-vorbis-decoder (setup)
  (let ((ch (vi-channels (vs-info setup))))
    (%make-vorbis-decoder
     :setup setup
     :lap (let ((v (make-array ch)))
            (dotimes (i ch v)
              (setf (aref v i) (make-array 0 :element-type 'double-float)))))))

(defun %window-for (d n bs0 blockflag prev-long next-long)
  (let ((key (list n blockflag prev-long next-long)))
    (or (gethash key (vd-windows d))
        (setf (gethash key (vd-windows d))
              (%vorbis-window n bs0 blockflag prev-long next-long)))))

(defun decode-audio-packet (d packet)
  "One audio packet to PCM, or NIL if it produced no output (the first frame, or a packet the
   format says to discard).  Returns a vector of double-float arrays, one per channel."
  (declare (type vorbis-decoder d))
  (let* ((setup (vd-setup d))
         (info (vs-info setup))
         (channels (vi-channels info))
         (bs0 (vi-blocksize-0 info))
         (bs1 (vi-blocksize-1 info))
         (v (make-vbits packet)))
    (when (zerop (length packet)) (return-from decode-audio-packet nil))
    (unless (zerop (vb1 v)) (return-from decode-audio-packet nil))   ; not an audio packet
    (let* ((modes (vs-modes setup))
           (mode-number (vb v (ilog (1- (length modes))))))
      (when (or (vb-eop v) (>= mode-number (length modes)))
        (return-from decode-audio-packet nil))
      (let* ((mode (aref modes mode-number))
             (blockflag (vmo-blockflag mode))
             (n (if blockflag bs1 bs0))
             (prev-long t) (next-long t))
        (when blockflag
          (setf prev-long (plusp (vb1 v)))
          (setf next-long (plusp (vb1 v))))
        (if blockflag (incf (vd-long-blocks d)) (incf (vd-short-blocks d)))
        (when (and blockflag (not (and prev-long next-long))) (incf (vd-hybrid-windows d)))
        (when (vb-eop v) (return-from decode-audio-packet nil))
        (let* ((half (ash n -1))
               (map (aref (vs-mappings setup) (vmo-mapping mode)))
               (floors (vs-floors setup))
               (codebooks (vs-codebooks setup))
               (residue-vectors (make-array channels))
               (floor-data (make-array channels :initial-element nil))
               (no-residue (make-array channels :initial-element t)))
          (dotimes (i channels)
            (setf (aref residue-vectors i)
                  (make-array half :element-type 'double-float :initial-element 0d0)))
          ;; ---- floors, in channel order
          (dotimes (i channels)
            (let* ((submap (aref (vm-mux map) i))
                   (f (aref floors (aref (vm-submap-floor map) submap)))
                   (y (make-array (f1-values f) :element-type 'fixnum :initial-element 0)))
              (if (floor1-decode f v codebooks y)
                  (setf (aref floor-data i) (cons f y) (aref no-residue i) nil)
                  (setf (aref floor-data i) nil (aref no-residue i) t))))
          (dotimes (i channels) (when (aref no-residue i) (incf (vd-unused-channels d))))
          (when (plusp (length (vm-coupling map))) (incf (vd-coupled-blocks d)))
          ;; ---- a coupled pair is used if either half is (§4.3.3)
          (loop for pair across (vm-coupling map)
                do (unless (and (aref no-residue (car pair)) (aref no-residue (cdr pair)))
                     (setf (aref no-residue (car pair)) nil
                           (aref no-residue (cdr pair)) nil)))
          ;; ---- residues, in submap order
          (dotimes (s (vm-submaps map))
            (let ((chans (loop for j below channels when (= (aref (vm-mux map) j) s) collect j)))
              (when chans
                (let ((vecs (make-array (length chans)))
                      (skip (make-array (length chans))))
                  (loop for j in chans for k from 0
                        do (setf (aref vecs k) (aref residue-vectors j)
                                 (aref skip k) (aref no-residue j)))
                  (residue-decode (aref (vs-residues setup) (aref (vm-submap-residue map) s))
                                  v codebooks vecs skip half)))))
          ;; ---- inverse coupling, last step first
          (loop for idx of-type fixnum downfrom (1- (length (vm-coupling map))) to 0
                do (let* ((pair (aref (vm-coupling map) idx))
                          (mv (the (simple-array double-float (*))
                                   (aref residue-vectors (car pair))))
                          (av (the (simple-array double-float (*))
                                   (aref residue-vectors (cdr pair)))))
                     (dotimes (k half)
                       (let ((m (aref mv k)) (a (aref av k)))
                         (multiple-value-bind (nm na)
                             (if (plusp m)
                                 (if (plusp a) (values m (- m a)) (values (+ m a) m))
                                 (if (plusp a) (values m (+ m a)) (values (- m a) m)))
                           (setf (aref mv k) nm (aref av k) na))))))
          ;; ---- floor times residue, then the inverse transform
          (let ((blocks (make-array channels))
                (curve (make-array half :element-type 'double-float :initial-element 0d0)))
            (dotimes (i channels)
              (let ((spec (aref residue-vectors i))
                    (out (make-array n :element-type 'double-float :initial-element 0d0)))
                (if (aref floor-data i)
                    (progn
                      (fill curve 0d0)
                      (floor1-curve (car (aref floor-data i)) (cdr (aref floor-data i))
                                    curve half)
                      (dotimes (k half) (setf (aref spec k) (* (aref spec k) (aref curve k))))
                      (%vorbis-imdct spec n out))
                    (fill out 0d0))
                (setf (aref blocks i) out)))
            ;; ---- window and lap
            (let ((w (%window-for d n bs0 blockflag prev-long next-long)))
              (declare (type (simple-array double-float (*)) w))
              (dotimes (i channels)
                (let ((b (the (simple-array double-float (*)) (aref blocks i))))
                  (dotimes (k n) (setf (aref b k) (* (aref b k) (aref w k)))))))
            (let* ((prev-n (vd-prev-n d))
                   (out (when (plusp prev-n)
                          (let* ((len (ash (+ prev-n n) -2))
                                 (off (ash (- n prev-n) -2))
                                 (res (make-array channels)))
                            (dotimes (i channels res)
                              (let ((o (make-array len :element-type 'double-float
                                                       :initial-element 0d0))
                                    (tail (the (simple-array double-float (*)) (aref (vd-lap d) i)))
                                    (cur (the (simple-array double-float (*)) (aref blocks i))))
                                (dotimes (a len)
                                  (let ((acc 0d0) (k (+ a off)))
                                    (when (< a (length tail)) (incf acc (aref tail a)))
                                    (when (< -1 k n) (incf acc (aref cur k)))
                                    (setf (aref o a) acc)))
                                (setf (aref res i) o)))))))
              ;; the new tail is this block from its centre on
              (dotimes (i channels)
                (setf (aref (vd-lap d) i)
                      (subseq (the (simple-array double-float (*)) (aref blocks i)) (ash n -1))))
              (setf (vd-prev-n d) n)
              out)))))))

;;; ---- Ogg, and the whole file ---------------------------------------------------------------------

(defun %reassemble-vorbis-packets (pages)
  "Join packet fragments across page boundaries for the first logical stream in PAGES.
   Returns (values packets last-granule)."
  (let ((all '()) (pending nil) (serial nil) (last-granule 0))
    (dolist (pg pages)
      (when (null serial) (setf serial (ogg-page-serial pg)))
      (when (= (ogg-page-serial pg) serial)
        (setf last-granule (ogg-page-granule pg))
        (loop for (pk . completep) in (ogg-page-packets pg)
              for first = t then nil
              do (let ((frag (if (and first pending) (%concat-octets pending pk) pk)))
                   (when (and first pending) (setf pending nil))
                   (if completep (push frag all) (setf pending frag))))))
    (values (nreverse all) last-granule)))

(defun decode-vorbis-ogg (octets)
  "Demux an Ogg Vorbis stream and decode it to a PCM struct.

   The first three packets are the identification, comment and setup headers; the rest are audio.
   The last page's granule position is the stream's true length in samples, which is how a file
   whose final block runs past the end of the music is trimmed."
  (multiple-value-bind (packets last-granule) (%reassemble-vorbis-packets (%parse-ogg-pages octets))
    (when (< (length packets) 3)
      (vorbis-error "an Ogg stream with ~d packets is not Vorbis" (length packets)))
    (let* ((info (parse-identification (first packets)))
           (setup (parse-setup (third packets) info))
           (d (make-vorbis-decoder setup))
           (channels (vi-channels info))
           (chunks '())
           (total 0))
      (dolist (pk (cdddr packets))
        (let ((out (decode-audio-packet d pk)))
          (when out
            (push out chunks)
            (incf total (length (the (simple-array double-float (*)) (aref out 0)))))))
      (setf chunks (nreverse chunks))
      (let* ((want (if (and (plusp last-granule) (< last-granule total)) last-granule total))
             (samples (make-pcm16 (* want channels)))
             (o 0))
        (block fill
          (dolist (chunk chunks)
            (let ((len (length (the (simple-array double-float (*)) (aref chunk 0)))))
              (dotimes (k len)
                (when (>= (floor o channels) want) (return-from fill))
                (dotimes (c channels)
                  (setf (aref samples o)
                        (clamp16 (round (* 32768d0
                                           (aref (the (simple-array double-float (*)) (aref chunk c))
                                                 k)))))
                  (incf o))))))
        (make-pcm :samples samples :channels channels :sample-rate (vi-rate info)
                  :format :pcm16 :frame-count want)))))

(defun decode-vorbis-file (path)
  "Decode an Ogg Vorbis file to PCM."
  (decode-vorbis-ogg
   (with-open-file (s path :element-type '(unsigned-byte 8))
     (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
       (read-sequence buf s)
       buf))))

;;; ---- one packet at a time, for a container that hands them over --------------------------------

(defun decode-vorbis-packet (d packet)
  "One audio packet as a REED PCM struct, or NIL when the packet produced no samples.

   The first audio packet of a stream always produces none: a lapped transform has nothing to lap
   against until the second one has been decoded."
  (declare (type vorbis-decoder d))
  (let ((out (decode-audio-packet d packet)))
    (when out
      (let* ((channels (length out))
             (len (length (the (simple-array double-float (*)) (aref out 0))))
             (samples (make-pcm16 (* len channels)))
             (o 0))
        (dotimes (k len)
          (dotimes (c channels)
            (setf (aref samples o)
                  (clamp16 (round (* 32768d0
                                     (aref (the (simple-array double-float (*)) (aref out c)) k)))))
            (incf o)))
        (make-pcm :samples samples :channels channels
                  :sample-rate (vi-rate (vs-info (vd-setup d)))
                  :format :pcm16 :frame-count len)))))

(defun vorbis-headers-from-xiph (private)
  "The three Vorbis headers out of a Matroska CodecPrivate, which packs them with Xiph lacing:
   a count byte, then all but the last length as chains of 255s, then the packets themselves."
  (declare (type (simple-array (unsigned-byte 8) (*)) private))
  (when (< (length private) 3) (vorbis-error "CodecPrivate is too short to hold three headers"))
  (let ((npackets (1+ (aref private 0)))
        (p 1)
        (lengths '()))
    (unless (= npackets 3)
      (vorbis-error "CodecPrivate declares ~d Vorbis headers, not three" npackets))
    (dotimes (i (1- npackets))
      (let ((len 0))
        (loop (when (>= p (length private))
                (vorbis-error "CodecPrivate ends inside its lacing table"))
              (incf len (aref private p))
              (incf p)
              (when (< (aref private (1- p)) 255) (return)))
        (push len lengths)))
    (setf lengths (nreverse lengths))
    (let* ((first-len (first lengths)) (second-len (second lengths))
           (third-len (- (length private) p first-len second-len)))
      (when (minusp third-len)
        (vorbis-error "CodecPrivate lacing overruns the data it describes"))
      (list (subseq private p (+ p first-len))
            (subseq private (+ p first-len) (+ p first-len second-len))
            (subseq private (+ p first-len second-len))))))

(defun make-vorbis-decoder-for-headers (headers)
  "A decoder from the three header packets, however the container delivered them."
  (let* ((info (parse-identification (first headers)))
         (setup (parse-setup (third headers) info)))
    (make-vorbis-decoder setup)))

(defun vorbis-packet-block-size (setup packet)
  "The block size one audio packet will use, read without decoding it.

   A container that has to put a timestamp on a packet before anything decodes it needs this: the
   mode number is the first field after the packet-type bit, and the mode says long or short."
  (declare (type vorbis-setup setup))
  (when (zerop (length packet)) (return-from vorbis-packet-block-size 0))
  (let ((v (make-vbits packet))
        (info (vs-info setup))
        (modes (vs-modes setup)))
    (unless (zerop (vb1 v)) (return-from vorbis-packet-block-size 0))
    (let ((m (vb v (ilog (1- (length modes))))))
      (if (or (vb-eop v) (>= m (length modes)))
          0
          (if (vmo-blockflag (aref modes m)) (vi-blocksize-1 info) (vi-blocksize-0 info))))))

(defun vorbis-setup-from-headers (headers)
  "The parsed setup, for a caller that needs to reason about packets before decoding them."
  (parse-setup (third headers) (parse-identification (first headers))))
