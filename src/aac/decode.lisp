;;;; src/aac/decode.lisp --- AAC-LC top level: raw_data_block syntax, ADTS
;;;; framing, and the decode-aac / decode-aac-file entry points.
;;;; ISO/IEC 14496-3 (MPEG-4 Audio, Low Complexity profile / AOT 2).
(in-package #:reed)

(define-condition aac-error (error)
  ((message :initarg :message :reader aac-error-message))
  (:report (lambda (c s) (format s "AAC decode error: ~a" (aac-error-message c)))))

(defparameter *aac-output-scale* (/ 1d0 32768d0)
  "Global scale from filterbank output to normalized [-1,1] PCM.  The dequant
and 2/N IMDCT leave samples in the integer-PCM range, so this is 1/32768.")

;;; syntactic element ids (ISO Table 4.71)
(defconstant +id-sce+ 0) (defconstant +id-cpe+ 1) (defconstant +id-cce+ 2)
(defconstant +id-lfe+ 3) (defconstant +id-dse+ 4) (defconstant +id-pce+ 5)
(defconstant +id-fil+ 6) (defconstant +id-end+ 7)

(declaim (inline br-align))
(defun br-align (br)
  (setf (bitreader-pos br) (* 8 (ceiling (bitreader-pos br) 8))))

;;; ------------------------------------------------------------------ ;;;
;;; ics_info                                                            ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-read-ics-info (br chan sr-index)
  (read-bit br)                              ; reserved
  (setf (aac-chan-wseq-prev chan) (aac-chan-wseq chan)
        (aac-chan-wseq chan) (read-bits br 2)
        (aac-chan-kbd-prev chan) (aac-chan-kbd chan)
        (aac-chan-kbd chan) (read-bit br))
  (let ((wseq (aac-chan-wseq chan)))
    (cond
      ((= wseq +wseq-eight-short+)
       (setf (aac-chan-max-sfb chan) (read-bits br 4))
       (let ((ng 1) (gl (aac-chan-group-len chan)))
         (setf (aref gl 0) 1)
         (dotimes (i 7)
           (if (= 1 (read-bit br))
               (incf (aref gl (1- ng)))
               (progn (incf ng) (setf (aref gl (1- ng)) 1))))
         (setf (aac-chan-num-groups chan) ng
               (aac-chan-num-windows chan) 8
               (aac-chan-swb chan) (aref +aac-swb-128+ sr-index)
               (aac-chan-num-swb chan) (1- (length (aref +aac-swb-128+ sr-index)))
               (aac-chan-tns-max-bands chan) (aref +aac-tns-max-bands-128+ sr-index))))
      (t
       (setf (aac-chan-max-sfb chan) (read-bits br 6)
             (aac-chan-num-windows chan) 1
             (aac-chan-num-groups chan) 1
             (aref (aac-chan-group-len chan) 0) 1
             (aac-chan-swb chan) (aref +aac-swb-1024+ sr-index)
             (aac-chan-num-swb chan) (1- (length (aref +aac-swb-1024+ sr-index)))
             (aac-chan-tns-max-bands chan) (aref +aac-tns-max-bands-1024+ sr-index))
       (when (= 1 (read-bit br))               ; predictor_present
         (error 'aac-error :message "prediction not supported in AAC-LC"))))))

;;; ------------------------------------------------------------------ ;;;
;;; section_data (band types) and scale_factor_data                     ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-read-band-types (br chan)
  (let* ((bits (if (= (aac-chan-wseq chan) +wseq-eight-short+) 3 5))
         (max-sfb (aac-chan-max-sfb chan))
         (bt (aac-chan-band-type chan)))
    (dotimes (g (aac-chan-num-groups chan))
      (let ((k 0))
        (loop while (< k max-sfb) do
          (let ((sect-bt (read-bits br 4))
                (sect-end k))
            (loop for incr = (read-bits br bits)
                  do (incf sect-end incr)
                  while (= incr (1- (ash 1 bits))))
            (loop while (< k sect-end) do
              (setf (aref bt (+ (* g max-sfb) k)) sect-bt) (incf k)))))
      ;; fill remaining columns for this group already advanced; nothing.
      )))

(defun aac-read-scalefactors (br chan global-gain)
  (let* ((max-sfb (aac-chan-max-sfb chan))
         (bt (aac-chan-band-type chan))
         (sf (aac-chan-sfmult chan))
         (off0 global-gain) (off1 (- global-gain 90)) (off2 0)
         (noise-flag t))
    (dotimes (g (aac-chan-num-groups chan))
      (dotimes (sfb max-sfb)
        (let* ((idx (+ (* g max-sfb) sfb)) (b (aref bt idx)))
          (cond
            ((= b +bt-zero+) (setf (aref sf idx) 0d0))
            ((or (= b +bt-intensity+) (= b +bt-intensity2+))
             (incf off2 (aac-decode-scalefactor br))
             (setf (aref sf idx) (aac-pow2sf (- (max -155 (min 100 off2))))))
            ((= b +bt-noise+)
             (if noise-flag
                 (progn (setf noise-flag nil)
                        (incf off1 (- (read-bits br 9) 256)))
                 (incf off1 (aac-decode-scalefactor br)))
             (setf (aref sf idx) (aac-pow2sf (max -100 (min 155 off1)))))
            (t
             (incf off0 (aac-decode-scalefactor br))
             (when (> off0 255)
               (error 'aac-error :message "scalefactor out of range"))
             (setf (aref sf idx) (aac-pow2sf (- off0 100))))))))))

;;; ------------------------------------------------------------------ ;;;
;;; TNS                                                                 ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-read-tns (br chan)
  (let* ((tns (aac-chan-tns chan))
         (is8 (if (= (aac-chan-wseq chan) +wseq-eight-short+) 1 0))
         (maxord (if (= is8 1) 7 12)))
    (setf (tns-present tns) t)
    (dotimes (w (aac-chan-num-windows chan))
      (let ((nf (read-bits br (- 2 is8))))
        (setf (aref (tns-n-filt tns) w) nf)
        (when (plusp nf)
          (let ((coef-res (read-bit br)))
            (dotimes (filt nf)
              (setf (aref (tns-length tns) w filt) (read-bits br (- 6 (* 2 is8))))
              (let ((order (read-bits br (- 5 (* 2 is8)))))
                (when (> order maxord) (setf order maxord))
                (setf (aref (tns-order tns) w filt) order)
                (when (plusp order)
                  (setf (aref (tns-direction tns) w filt) (read-bit br))
                  (let* ((compress (read-bit br))
                         (clen (+ coef-res 3 (- compress)))
                         (map (aref +aac-tns-tmp2-map+ (+ (* 2 compress) coef-res))))
                    (dotimes (i order)
                      (setf (aref (tns-coef tns) w filt i)
                            (aref map (read-bits br clen))))))))))))))

;;; ------------------------------------------------------------------ ;;;
;;; pulse data                                                          ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-read-pulses (br chan)
  "Return (values num pos amp) for pulse_data; positions absolute."
  (let* ((num (1+ (read-bits br 2)))
         (swb (aac-chan-swb chan))
         (pos (make-array num :element-type 'fixnum))
         (amp (make-array num :element-type 'fixnum))
         (start-swb (read-bits br 6)))
    (setf (aref pos 0) (+ (aref swb start-swb) (read-bits br 5))
          (aref amp 0) (read-bits br 4))
    (loop for i from 1 below num do
      (setf (aref pos i) (+ (aref pos (1- i)) (read-bits br 5))
            (aref amp i) (read-bits br 4)))
    (values num pos amp)))

(defun aac-apply-pulses (chan num pos amp)
  (let* ((coef (aac-chan-coeffs chan))
         (swb (aac-chan-swb chan))
         (bt (aac-chan-band-type chan))
         (sf (aac-chan-sfmult chan)))
    (dotimes (i num)
      (let ((p (aref pos i)) (idx 0))
        (loop while (<= (aref swb (1+ idx)) p) do (incf idx))
        (when (and (/= (aref bt idx) +bt-noise+) (/= (aref sf idx) 0d0))
          (let ((co (aref coef p)) (ico (- (coerce (aref amp i) 'double-float)))
                (s (aref sf idx)))
            (when (/= co 0d0)
              (setf co (/ co s)
                    ico (+ (/ co (sqrt (sqrt (abs co))))
                           (if (> co 0d0) (- ico) ico))))
            (setf (aref coef p) (* (expt (abs ico) 1/3) ico s))))))))

;;; ------------------------------------------------------------------ ;;;
;;; spectral data (Huffman + dequant + PNS)                             ;;;
;;; ------------------------------------------------------------------ ;;;

(defvar *aac-rng* 0)

(declaim (inline aac-lcg))
(defun aac-lcg (x)
  (let ((u (logand (+ (* x 1664525) 1013904223) #xffffffff)))
    (if (>= u #x80000000) (- u #x100000000) u)))

(defun aac-decode-spectrum (br chan)
  (let* ((coef (aac-chan-coeffs chan))
         (bt (aac-chan-band-type chan))
         (sf (aac-chan-sfmult chan))
         (swb (aac-chan-swb chan))
         (max-sfb (aac-chan-max-sfb chan))
         (base 0))
    (declare (type (simple-array double-float (*)) coef))
    (fill coef 0d0)
    (dotimes (g (aac-chan-num-groups chan))
      (let ((glen (aref (aac-chan-group-len chan) g)))
        (dotimes (sfb max-sfb)
          (let* ((idx (+ (* g max-sfb) sfb)) (b (aref bt idx))
                 (start (aref swb sfb)) (len (- (aref swb (1+ sfb)) (aref swb sfb))))
            (cond
              ((or (= b +bt-zero+) (>= b +bt-intensity2+)) nil) ; zeros already
              ((= b +bt-noise+)
               (dotimes (grp glen)
                 (let ((o (+ base (* grp 128) start)) (energy 0d0))
                   (dotimes (k len)
                     (setf *aac-rng* (aac-lcg *aac-rng*))
                     (let ((v (coerce *aac-rng* 'double-float)))
                       (setf (aref coef (+ o k)) v)
                       (incf energy (* v v))))
                   (let ((scale (if (> energy 0d0) (/ (aref sf idx) (sqrt energy)) 0d0)))
                     (dotimes (k len) (setf (aref coef (+ o k)) (* (aref coef (+ o k)) scale)))))))
              (t
               (dotimes (grp glen)
                 (aac-decode-spectral-band br (1- b) coef (+ base (* grp 128) start)
                                           len (aref sf idx)))))))
        (incf base (* glen 128))))))

;;; ------------------------------------------------------------------ ;;;
;;; individual_channel_stream                                           ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-decode-ics (br chan sr-index common-window)
  (let ((global-gain (read-bits br 8)))
    (unless common-window (aac-read-ics-info br chan sr-index))
    (aac-read-band-types br chan)
    (aac-read-scalefactors br chan global-gain)
    (let ((num 0) pos amp)
      (when (= 1 (read-bit br))              ; pulse_data_present
        (when (= (aac-chan-wseq chan) +wseq-eight-short+)
          (error 'aac-error :message "pulse in eight-short"))
        (multiple-value-setq (num pos amp) (aac-read-pulses br chan)))
      (setf (tns-present (aac-chan-tns chan)) nil)
      (when (= 1 (read-bit br))              ; tns_data_present
        (aac-read-tns br chan))
      (when (= 1 (read-bit br))              ; gain_control_present
        (error 'aac-error :message "gain control (SSR) not supported"))
      (aac-decode-spectrum br chan)
      (when (plusp num) (aac-apply-pulses chan num pos amp)))))

(defun aac-run-channel (chan)
  "TNS + filterbank for one decoded channel -> its OUTPUT buffer."
  (when (tns-present (aac-chan-tns chan))
    (aac-apply-tns (aac-chan-coeffs chan) chan))
  (aac-filterbank (aac-chan-coeffs chan) (aac-chan-saved chan)
                  (aac-chan-output chan) (aac-chan-wseq chan)
                  (= 1 (aac-chan-kbd chan)) (= 1 (aac-chan-kbd-prev chan))))

;;; ------------------------------------------------------------------ ;;;
;;; raw_data_block                                                      ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-decode-raw-block (br sr-index elements)
  "Decode one raw_data_block.  ELEMENTS is a hash keyed (type . id) of channel
lists (persistent).  Returns an ordered list of aac-chan whose OUTPUT is filled."
  (let ((out-order '()))
    (loop
      (when (< (br-remaining br) 3) (return))
      (let ((id (read-bits br 3)))
        (cond
          ((= id +id-end+) (return))
          ((or (= id +id-sce+) (= id +id-lfe+))
           (read-bits br 4)                  ; element_instance_tag
           (let ((chan (or (gethash (cons id 0) elements)
                           (setf (gethash (cons id 0) elements) (make-aac-chan)))))
             (aac-decode-ics br chan sr-index nil)
             (aac-run-channel chan)
             (push chan out-order)))
          ((= id +id-cpe+)
           (read-bits br 4)
           (let* ((pair (or (gethash (cons id 0) elements)
                            (setf (gethash (cons id 0) elements)
                                  (cons (make-aac-chan) (make-aac-chan)))))
                  (ch0 (car pair)) (ch1 (cdr pair))
                  (common (= 1 (read-bit br)))
                  (ms-present 0)
                  (ms-mask (make-array 512 :initial-element nil)))
             (when common
               (aac-read-ics-info br ch0 sr-index)
               ;; copy shared ics_info to ch1, preserving ch1's own history
               (let ((wp (aac-chan-wseq ch1)) (kp (aac-chan-kbd ch1)))
                 (declare (ignore wp kp))
                 (setf (aac-chan-wseq-prev ch1) (aac-chan-wseq ch1)
                       (aac-chan-kbd-prev ch1) (aac-chan-kbd ch1)
                       (aac-chan-wseq ch1) (aac-chan-wseq ch0)
                       (aac-chan-kbd ch1) (aac-chan-kbd ch0)
                       (aac-chan-max-sfb ch1) (aac-chan-max-sfb ch0)
                       (aac-chan-num-windows ch1) (aac-chan-num-windows ch0)
                       (aac-chan-num-groups ch1) (aac-chan-num-groups ch0)
                       (aac-chan-swb ch1) (aac-chan-swb ch0)
                       (aac-chan-num-swb ch1) (aac-chan-num-swb ch0)
                       (aac-chan-tns-max-bands ch1) (aac-chan-tns-max-bands ch0))
                 (dotimes (i 8)
                   (setf (aref (aac-chan-group-len ch1) i)
                         (aref (aac-chan-group-len ch0) i))))
               (setf ms-present (read-bits br 2))
               (when (= ms-present 3) (error 'aac-error :message "ms_present=3 reserved"))
               (when (= ms-present 1)
                 (let ((n (* (aac-chan-num-groups ch0) (aac-chan-max-sfb ch0))))
                   (dotimes (i n) (setf (aref ms-mask i) (= 1 (read-bit br))))))
               (when (= ms-present 2)
                 (dotimes (i 512) (setf (aref ms-mask i) t))))
             (aac-decode-ics br ch0 sr-index common)
             (aac-decode-ics br ch1 sr-index common)
             (when (and common (plusp ms-present))
               (aac-apply-ms ch0 ch1 ms-mask))
             (aac-apply-intensity ch0 ch1 (plusp ms-present) ms-mask)
             (aac-run-channel ch0)
             (aac-run-channel ch1)
             (push ch0 out-order) (push ch1 out-order)))
          ((= id +id-dse+)
           (read-bits br 4)
           (let ((align (read-bit br)) (cnt (read-bits br 8)))
             (when (= cnt 255) (incf cnt (read-bits br 8)))
             (when (= align 1) (br-align br))
             (dotimes (i cnt) (read-bits br 8))))
          ((= id +id-fil+)
           (let ((cnt (read-bits br 4)))
             (when (= cnt 15) (incf cnt (1- (read-bits br 8))))
             (dotimes (i cnt) (read-bits br 8))))
          ((= id +id-pce+)
           (error 'aac-error :message "program_config_element not supported"))
          ((= id +id-cce+)
           (error 'aac-error :message "coupling channel element not supported"))
          (t (return)))))
    (br-align br)
    (nreverse out-order)))

;;; ------------------------------------------------------------------ ;;;
;;; ADTS framing + top level                                            ;;;
;;; ------------------------------------------------------------------ ;;;

(defun aac-adts-header (bytes p)
  "Parse an ADTS header at byte offset P.  Returns
(values sr-index channel-config frame-length header-len aot) or NIL."
  (when (and (< (+ p 7) (length bytes))
             (= (aref bytes p) #xff)
             (= (logand (aref bytes (+ p 1)) #xf6) #xf0)) ; sync + layer 0
    (let* ((protection-absent (logand (aref bytes (+ p 1)) 1))
           (b2 (aref bytes (+ p 2)))
           (profile (ash b2 -6))
           (sr-index (logand (ash b2 -2) #xf))
           (chan-cfg (logior (ash (logand b2 1) 2)
                             (ash (aref bytes (+ p 3)) -6)))
           (frame-len (logior (ash (logand (aref bytes (+ p 3)) 3) 11)
                              (ash (aref bytes (+ p 4)) 3)
                              (ash (aref bytes (+ p 5)) -5)))
           (header-len (if (= protection-absent 1) 7 9)))
      (values sr-index chan-cfg frame-len header-len (1+ profile)))))

(defun aac-config-channels (cfg)
  (case cfg (0 2) (7 8) (t cfg)))

(defun decode-aac (octets &key (format :pcm16))
  "Decode AAC-LC (auto-detecting ADTS framing) into a PCM struct."
  (declare (type (array (unsigned-byte 8)) octets))
  (let ((bytes (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (unless (and (>= (length bytes) 2)
                 (= (aref bytes 0) #xff)
                 (= (logand (aref bytes 1) #xf6) #xf0))
      (error 'aac-error :message "not an ADTS AAC stream (raw ASC not supported)"))
    (let ((elements (make-hash-table :test 'equal))
          (frames '())                     ; list of per-frame channel-sample vectors
          (*aac-rng* #x1f2e3d4c)
          (sr-index 0) (chan-cfg 0) (nchan 0) (p 0) (len (length bytes)))
      (loop
        (when (> (+ p 7) len) (return))
        (multiple-value-bind (sri cfg flen hlen aot) (aac-adts-header bytes p)
          (unless sri (incf p) (return))    ; lost sync: stop (already decoded frames kept)
          aot
          (setf sr-index sri chan-cfg cfg)
          (when (zerop nchan) (setf nchan (aac-config-channels cfg)))
          (when (or (< flen hlen) (> (+ p flen) len)) (return))
          (let* ((br (make-bitreader bytes :start (+ p hlen) :end (+ p flen)))
                 (chans (aac-decode-raw-block br sr-index elements)))
            ;; snapshot this frame's channel outputs (buffers are reused next frame)
            (let ((fr (make-array (max 1 (length chans)))))
              (loop for c in chans for i from 0
                    do (setf (aref fr i)
                             (copy-seq (the (simple-array double-float (*))
                                            (aac-chan-output c)))))
              (push (cons (length chans) fr) frames)))
          (incf p flen)))
      (setf frames (nreverse frames))
      (aac-assemble-pcm frames nchan (aref +aac-sample-rates+ sr-index) format))))

(defun aac-assemble-pcm (frames nchan sample-rate format)
  (let* ((nframes (length frames))
         (total (* nframes 1024))
         (scale *aac-output-scale*)
         (samples (make-array (* total nchan)
                              :element-type (if (eq format :float32) 'single-float '(signed-byte 16)))))
    (loop for (n . fr) in frames for f from 0
          do (dotimes (s 1024)
               (dotimes (ch nchan)
                 (let* ((src (if (< ch n) (aref fr ch) nil))
                        (v (if src (* scale (aref (the (simple-array double-float (*)) src) s)) 0d0))
                        (oi (+ (* (+ (* f 1024) s) nchan) ch)))
                   (if (eq format :float32)
                       (setf (aref samples oi) (coerce (max -1d0 (min 1d0 v)) 'single-float))
                       (setf (aref samples oi)
                             (max -32768 (min 32767 (round (* v 32768d0))))))))))
    (make-pcm :samples samples :channels nchan :sample-rate sample-rate
              :format format :frame-count total)))

(defun aac-decode-access-units (aus sr-index channels sample-rate &key (format :pcm16))
  "Decode a sequence of raw AAC access units (each a byte vector, one
raw_data_block) sharing a config -> PCM.  Used by the MP4/M4A demuxer, which
supplies the config from the AudioSpecificConfig rather than per-frame ADTS."
  (let ((elements (make-hash-table :test 'equal))
        (*aac-rng* #x1f2e3d4c)
        (frames '()))
    (dolist (au aus)
      (let* ((br (make-bitreader (coerce au '(simple-array (unsigned-byte 8) (*)))))
             (chans (aac-decode-raw-block br sr-index elements))
             (fr (make-array (max 1 (length chans)))))
        (loop for c in chans for i from 0
              do (setf (aref fr i)
                       (copy-seq (the (simple-array double-float (*))
                                      (aac-chan-output c)))))
        (push (cons (length chans) fr) frames)))
    (aac-assemble-pcm (nreverse frames) channels sample-rate format)))

(defun decode-aac-file (path &key (format :pcm16))
  "Decode an AAC file (.aac/.adts ADTS, or .m4a/.mp4) into a PCM struct."
  (let* ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence v s) v))))
    (if (aac-mp4-p bytes)
        (decode-m4a bytes :format format)
        (decode-aac bytes :format format))))
