;;;; mp2/decode.lisp — MPEG-1/2 Audio Layer II.
;;;;
;;;; THE LAYER THAT DVDS AND BROADCAST USE, and the simplest of the three.  Layer III spends most
;;;; of its size on a modified DCT, a bit reservoir and Huffman-coded spectra; Layer II has none of
;;;; those.  It quantises the 32 subbands directly, sends a bit allocation saying how finely each
;;;; one was quantised, and lets the polyphase filterbank — the same one Layer III ends with — put
;;;; them back together.  So this file is a few hundred lines beside Layer III's few thousand, and
;;;; the two share the only expensive part.
;;;;
;;;; The one clever thing in it is GROUPING.  A subband quantised to three levels does not get two
;;;; bits per sample; three consecutive samples share one five-bit code, because three levels cubed
;;;; is twenty-seven and fits in five bits where six would be needed otherwise.  The same trick
;;;; applies at five and nine levels.

(in-package #:reed)

(defstruct (mp2-decoder (:conc-name mp2d-) (:constructor %make-mp2-decoder))
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (vvec (make-array '(2 1024) :element-type 'double-float :initial-element 0d0)
        :type (simple-array double-float (2 1024)))
  (header nil))

(defun make-mp2-decoder (octets &key (start 0) (end (length octets)))
  (%make-mp2-decoder :bytes (coerce octets '(simple-array (unsigned-byte 8) (*)))
                     :pos start :end end))

(defun mp2-select-table (bitrate channels sample-rate lsf)
  "Which of the five allocation tables this frame uses (Table 3-B.2).

   The choice is by bit rate PER CHANNEL, not per frame, because what it really decides is how many
   subbands there are bits to describe — and a stereo frame at 192 kbit/s is two 96 kbit/s channels."
  (declare (type fixnum bitrate channels sample-rate))
  (if lsf
      4
      (let ((ch (floor bitrate (* 1000 channels))))
        (declare (type fixnum ch))
        (cond ((or (and (= sample-rate 48000) (>= ch 56)) (<= 56 ch 80)) 0)
              ((and (/= sample-rate 48000) (>= ch 96)) 1)
              ((and (/= sample-rate 32000) (<= ch 48)) 2)
              (t 3)))))

;;; ---- a bit reader over the frame ----------------------------------------------------------------

(defstruct (mp2-bits (:conc-name mb-))
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum) (end 0 :type fixnum))

(declaim (inline mp2-read))
(defun mp2-read (b n)
  (declare (type mp2-bits b) (type (integer 0 24) n) (optimize (speed 3) (safety 1)))
  (let ((v 0))
    (declare (type fixnum v))
    (dotimes (i n v)
      (let* ((p (mb-pos b)) (byte (ash p -3)))
        (declare (type fixnum p byte))
        (setf v (logior (ash v 1)
                        (if (< byte (mb-end b))
                            (logand (ash (aref (mb-data b) byte) (- (- 7 (logand p 7)))) 1)
                            0)))
        (setf (mb-pos b) (1+ p))))))

;;; ---- one frame -----------------------------------------------------------------------------

(defun decode-mp2-frame (d h off out)
  "Decode one Layer II frame at byte offset OFF into OUT, which receives 1152 samples per channel
   interleaved.  Returns the number of sample frames written."
  (declare (type mp2-decoder d) (type fixnum off) (type (simple-array double-float (*)) out)
           (optimize (speed 3) (safety 1)))
  (let* ((bytes (mp2d-bytes d))
         (nch (fh-channels h))
         (lsf (not (eq (fh-version h) :mpeg1)))
         (table (mp2-select-table (fh-bitrate h) nch (fh-sample-rate h) lsf))
         (sblimit (aref +mp2-sblimit+ table))
         (alloc (aref +mp2-alloc-tables+ table))
         (bound (if (= 1 (fh-mode h)) (* 4 (1+ (fh-mode-extension h))) sblimit))
         (b (make-mp2-bits :data bytes
                           :pos (* 8 (+ off 4 (if (zerop (fh-protection h)) 2 0)))
                           :end (min (length bytes) (+ off (fh-frame-length h)))))
         (bit-alloc (make-array '(2 32) :element-type 'fixnum :initial-element 0))
         (scf (make-array '(2 32 3) :element-type 'fixnum :initial-element 0))
         (sb (make-array '(2 36 32) :element-type 'double-float :initial-element 0d0)))
    (declare (type fixnum nch table sblimit bound)
             (type (simple-array fixnum (2 32)) bit-alloc)
             (type (simple-array fixnum (2 32 3)) scf)
             (type (simple-array double-float (2 36 32)) sb))
    (setf bound (min bound sblimit))
    ;; ---- the bit allocation.  Above the JOINT STEREO BOUND both channels share one field, which
    ;; is the whole of what joint stereo means in Layer II: not a clever prediction, just an
    ;; agreement to describe the high subbands once.
    (let ((j 0))
      (declare (type fixnum j))
      (dotimes (i bound)
        (let ((nb (aref alloc j)))
          (declare (type fixnum nb))
          (dotimes (ch nch) (setf (aref bit-alloc ch i) (mp2-read b nb)))
          (incf j (ash 1 nb))))
      (loop for i of-type fixnum from bound below sblimit
            do (let* ((nb (aref alloc j)) (v (mp2-read b nb)))
                 (declare (type fixnum nb v))
                 (dotimes (ch nch) (setf (aref bit-alloc ch i) v))
                 (incf j (ash 1 nb)))))
    ;; ---- how many scalefactors each allocated subband sent, then the scalefactors themselves
    (let ((code (make-array '(2 32) :element-type 'fixnum :initial-element 0)))
      (declare (type (simple-array fixnum (2 32)) code))
      (dotimes (i sblimit)
        (dotimes (ch nch)
          (when (plusp (aref bit-alloc ch i)) (setf (aref code ch i) (mp2-read b 2)))))
      (dotimes (i sblimit)
        (dotimes (ch nch)
          (when (plusp (aref bit-alloc ch i))
            ;; THREE SCALEFACTORS PER FRAME, one per third, and the code says which of them were
            ;; actually sent: a subband whose level is steady sends one and repeats it.
            (case (aref code ch i)
              (0 (dotimes (k 3) (setf (aref scf ch i k) (mp2-read b 6))))
              (2 (let ((v (mp2-read b 6))) (dotimes (k 3) (setf (aref scf ch i k) v))))
              (1 (let ((a (mp2-read b 6)) (c (mp2-read b 6)))
                   (setf (aref scf ch i 0) a (aref scf ch i 1) a (aref scf ch i 2) c)))
              (t (let ((a (mp2-read b 6)) (c (mp2-read b 6)))
                   (setf (aref scf ch i 0) a (aref scf ch i 1) c (aref scf ch i 2) c))))))))
    ;; ---- the samples: three per subband per group, twelve groups per third, three thirds
    ;; THREE THIRDS OF FOUR GROUPS OF THREE SAMPLES: thirty-six sets of thirty-two subband samples,
    ;; which is 1152 output samples once the filterbank has had them.  The scalefactor changes only
    ;; between thirds, which is what makes three of them enough.
    (dotimes (k 3)
      (dotimes (lg 4)
        (let ((j 0))
          (declare (type fixnum j))
          (dotimes (i sblimit)
            (let ((nb (aref alloc j)))
              (declare (type fixnum nb))
              (dotimes (ch (if (< i bound) nch 1))
                (let ((a (aref bit-alloc ch i)))
                  (declare (type fixnum a))
                  (when (plusp a)
                    (let* ((qindex (aref alloc (+ j a)))
                           (bits (aref +mp2-quant-bits+ qindex))
                           (steps (aref +mp2-quant-steps+ qindex))
                           (sf (aref +mp2-scalefactor+ (aref scf ch i k)))
                           (row (+ (* k 12) (* lg 3))))
                      (declare (type fixnum qindex bits steps) (type double-float sf))
                      (flet ((put (m code)
                               (declare (type fixnum m code))
                               ;; ONE FORMULA FOR BOTH GROUPED AND UNGROUPED.  The code is a level
                               ;; index; centring it and dividing by the number of levels gives a
                               ;; fraction in (-1, 1), and that is the sample.
                               (let ((v (* sf (/ (* 2d0 (- code (ash steps -1)))
                                                 (coerce steps 'double-float)))))
                                 (setf (aref sb ch (+ row m) i) v)
                                 (when (and (>= i bound) (= nch 2))
                                   (setf (aref sb 1 (+ row m) i) v)))))
                        (if (minusp bits)
                            ;; grouped: one code carries three levels, as digits in base STEPS
                            (let ((v (mp2-read b (- bits))))
                              (declare (type fixnum v))
                              (put 0 (mod v steps))
                              (put 1 (mod (floor v steps) steps))
                              (put 2 (floor v (* steps steps))))
                            (dotimes (m 3) (put m (mp2-read b bits)))))))))
              (incf j (ash 1 nb)))))))
    ;; ---- and the same filterbank Layer III ends with
    (let ((s-vec (make-array 32 :element-type 'double-float))
          (u-vec (make-array 512 :element-type 'double-float))
          (chan (make-array 1152 :element-type 'double-float)))
      (declare (dynamic-extent s-vec u-vec chan))
      (dotimes (ch nch)
        (dotimes (g 36)
          (dotimes (i 32) (setf (aref s-vec i) (aref sb ch g i)))
          (synthesis-step s-vec u-vec (mp2d-vvec d) ch chan (* 32 g)))
        ;; interleaved on the way out, which is what every consumer of this wants
        (dotimes (i 1152) (setf (aref out (+ (* i nch) ch)) (aref chan i)))))
    1152))

;;; ---- walking a stream ---------------------------------------------------------------------------

(defun mp2-next-frame (d)
  "Find and decode the next Layer II frame.  Returns (values samples channels rate), or NIL at end.

   Synchronising is the same problem MP3 has and is solved the same way: a candidate header is only
   believed if it PARSES, because eleven set bits occur often enough in coded audio to find by
   accident."
  (let* ((bytes (mp2d-bytes d)) (end (mp2d-end d)) (p (mp2d-pos d)))
    (declare (type fixnum end p))
    (loop
      (when (> (+ p 4) end) (return nil))
      (let ((h (and (valid-frame-sync-p (aref bytes p) (aref bytes (1+ p)))
                    (parse-frame-header (u32be bytes p) :layer-wanted 2))))
        (cond
          ((and h (<= (+ p (fh-frame-length h)) end) (plusp (fh-frame-length h)))
           (let* ((nch (fh-channels h))
                  (out (make-array (* 1152 nch) :element-type 'double-float)))
             (decode-mp2-frame d h p out)
             (setf (mp2d-pos d) (+ p (fh-frame-length h))
                   (mp2d-header d) h)
             (return (values out nch (fh-sample-rate h)))))
          (t (incf p)))))))

(defun decode-mp2 (octets &key (format :pcm16) (start 0) (end (length octets)))
  "Decode a whole Layer II elementary stream into a PCM struct."
  (let ((d (make-mp2-decoder octets :start start :end end))
        (chunks '()) (total 0) (nch 2) (rate 48000))
    (declare (type fixnum total))
    (loop
      (multiple-value-bind (samples channels sr) (mp2-next-frame d)
        (when (null samples) (return))
        (setf nch channels rate sr)
        (incf total (length samples))
        (push samples chunks)))
    (let ((out (make-array total :element-type (if (eq format :float32) 'single-float
                                                   '(signed-byte 16))))
          (o 0))
      (declare (type fixnum o))
      (dolist (c (nreverse chunks))
        (declare (type (simple-array double-float (*)) c))
        (dotimes (i (length c))
          (let ((v (aref c i)))
            (setf (aref out o)
                  (if (eq format :float32)
                      (coerce (max -1d0 (min 1d0 v)) 'single-float)
                      (max -32768 (min 32767 (round (* v 32768d0))))))
            (incf o))))
      (make-pcm :samples out :channels nch :sample-rate rate :format format
                :frame-count (floor total (max 1 nch))))))

(defun decode-mp2-packet (d bytes &key (format :pcm16))
  "One Layer II frame through D, keeping the filterbank's state across frames.

   The state is the reason this takes a decoder rather than being a function of the bytes: the
   polyphase filterbank carries a thousand samples of history, and starting it fresh for every frame
   puts a click at every frame boundary — twenty-four times a second, which is a buzz."
  (let ((b (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (setf (mp2d-bytes d) b (mp2d-pos d) 0 (mp2d-end d) (length b))
    (multiple-value-bind (samples nch rate) (mp2-next-frame d)
      (unless samples (return-from decode-mp2-packet nil))
      (let ((out (make-array (length samples)
                             :element-type (if (eq format :float32) 'single-float
                                               '(signed-byte 16)))))
        (dotimes (i (length samples))
          (let ((v (aref samples i)))
            (setf (aref out i)
                  (if (eq format :float32)
                      (coerce (max -1d0 (min 1d0 v)) 'single-float)
                      (max -32768 (min 32767 (round (* v 32768d0))))))))
        (make-pcm :samples out :channels nch :sample-rate rate :format format
                  :frame-count (floor (length out) (max 1 nch)))))))
