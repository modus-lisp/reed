;;;; src/mp3/decode.lisp — MP3 container framing, bit-reservoir assembly, the
;;;; decode driver, and PCM output.  The PCM struct and WAV writer it emits into
;;;; live in src/common/pcm.lisp (shared by every reed codec).
(in-package #:reed)

;;; ---- decoder state ------------------------------------------------------
(defstruct (decoder (:constructor %make-decoder))
  (bytes #.(make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (header nil)
  ;; bit reservoir: RESV (a simple octet array) holds the tail of all main-data
  ;; bytes; RESV-FILL is how many are valid; RESV-BASE is the absolute index (in
  ;; the full main-data stream) of RESV element 0.
  (resv (make-array 16384 :element-type '(unsigned-byte 8)) :type octets)
  (resv-fill 0 :type fixnum)
  (resv-base 0 :type fixnum)
  (scl (make-array '(2 2 22) :element-type 'fixnum))
  (scs (make-array '(2 2 13 3) :element-type 'fixnum))
  (is-vec (let ((v (make-array 4)))
            (dotimes (i 4 v)
              (setf (svref v i) (make-array 576 :element-type 'double-float
                                                :initial-element 0.0d0)))))
  (store (make-array '(2 32 18) :element-type 'double-float :initial-element 0.0d0))
  (vvec  (make-array '(2 1024) :element-type 'double-float :initial-element 0.0d0))
  (out0 (make-array 576 :element-type 'double-float :initial-element 0.0d0))
  (out1 (make-array 576 :element-type 'double-float :initial-element 0.0d0))
  (sample-rate 0 :type fixnum)
  (channels 0 :type fixnum))

(defun make-decoder (octets &key (start 0) (end (length octets)))
  "Create a streaming decoder over OCTETS.  Skips a leading ID3v2 tag and a
trailing ID3v1 tag."
  (declare (type octets octets))
  (let ((s (max start (skip-id3v2 octets)))
        (e (min end (end-offset octets))))
    (%make-decoder :bytes octets :pos s :end e)))

(declaim (inline sideinfo-size))
(defun sideinfo-size (header)
  (if (eq (fh-version header) :mpeg1)
      (if (= (fh-channels header) 1) 17 32)
      (if (= (fh-channels header) 1) 9 17)))

;;; ---- reservoir ----------------------------------------------------------
(defun reservoir-append (d main-bytes start len)
  "Append LEN main-data bytes to the reservoir; return the absolute index at
which they begin."
  (declare (type octets main-bytes) (type fixnum start len))
  (let* ((resv (decoder-resv d))
         (fill (decoder-resv-fill d))
         (begin (+ (decoder-resv-base d) fill)))
    (declare (type octets resv) (type fixnum fill))
    ;; trim the front if the reservoir grows large (keep well over 511 bytes)
    (when (> fill 8192)
      (let ((keep 2048))
        (replace resv resv :start2 (- fill keep) :end2 fill)
        (incf (decoder-resv-base d) (- fill keep))
        (setf fill keep)))
    ;; grow the backing array if needed
    (when (> (+ fill len) (length resv))
      (let ((bigger (make-array (* 2 (+ fill len)) :element-type '(unsigned-byte 8))))
        (replace bigger resv :end2 fill)
        (setf resv bigger (decoder-resv d) bigger)))
    (replace resv main-bytes :start1 fill :start2 start :end2 (+ start len))
    (setf (decoder-resv-fill d) (+ fill len))
    begin))

;;; ---- frame decode -------------------------------------------------------
(defun decode-one-frame (d emit)
  "Locate and decode the next frame, calling EMIT for each granule.  Returns the
FRAME-HEADER decoded, :skipped for a VBR-tag frame, or NIL at end of stream."
  (multiple-value-bind (off header) (find-frame-sync (decoder-bytes d) (decoder-pos d)
                                                     (decoder-end d))
    (unless off (setf (decoder-pos d) (decoder-end d)) (return-from decode-one-frame nil))
    (let* ((bytes (decoder-bytes d))
           (flen (fh-frame-length header))
           (crc-len (if (= 0 (fh-protection header)) 2 0))
           (si-size (sideinfo-size header))
           (si-start (+ off 4 crc-len))
           (main-start (+ si-start si-size))
           (main-size (- flen 4 crc-len si-size)))
      (setf (decoder-header d) header
            (decoder-pos d) (+ off flen))
      (when (or (< main-size 0) (> (+ main-start main-size) (decoder-end d)))
        (return-from decode-one-frame :skipped))
      ;; VBR-tag frame carries no audio
      (when (xing/info/vbri-frame-p bytes off header)
        ;; still record its (empty) main data for reservoir continuity
        (reservoir-append d bytes main-start (max 0 main-size))
        (return-from decode-one-frame :skipped))
      (when (zerop (decoder-sample-rate d))
        (setf (decoder-sample-rate d) (fh-sample-rate header)
              (decoder-channels d) (fh-channels header)))
      (let* ((mdb (let ((br (make-bitreader bytes :start si-start :end main-start)))
                    (si-main-data-begin (parse-side-info header br))))
             (chunk-begin (reservoir-append d bytes main-start main-size))
             (win-begin (- chunk-begin mdb)))
        ;; parse side info again over the proper reader (cheap) for full struct
        (let* ((si (parse-side-info header
                                    (make-bitreader bytes :start si-start :end main-start)))
               (resv (decoder-resv d))
               (base (decoder-resv-base d)))
          (declare (ignore mdb))
          (when (< win-begin base)
            ;; not enough history yet (start of stream) — skip decoding
            (return-from decode-one-frame :skipped))
          (let ((br (make-bitreader resv :start (- win-begin base)
                                         :end (decoder-resv-fill d))))
            (handler-case
                (decode-frame-granules br header si (decoder-scl d) (decoder-scs d)
                                       (decoder-is-vec d) (decoder-store d) (decoder-vvec d)
                                       (decoder-out0 d) (decoder-out1 d) emit)
              (mp3-error (e) (declare (ignore e)) (return-from decode-one-frame :skipped)))))
        header))))

;;; ---- public: streaming frame API ---------------------------------------
(defun decode-next-frame (d &key (format :pcm16))
  "Decode the next audio frame and return its interleaved samples (or NIL at end
of stream).  Format is :pcm16 or :float32."
  (let ((acc (make-array 0 :adjustable t :fill-pointer 0)))
    (loop
      (let ((r (decode-one-frame
                d (lambda (out0 out1 nch) (emit-granule acc out0 out1 nch format)))))
        (cond ((null r) (return nil))
              ((eq r :skipped))         ; keep scanning
              (t (return (finalize-samples acc format (fh-channels r)))))))))

;;; ---- sample emission ----------------------------------------------------
(declaim (inline clip16))
(defun clip16 (x)
  (declare (type double-float x))
  (let ((v (round (* x 32767.0d0))))
    (the (signed-byte 16) (max -32768 (min 32767 v)))))

(defun emit-granule (acc out0 out1 nch format)
  (declare (type samples out0 out1) (type fixnum nch))
  (ecase format
    (:pcm16
     (if (= nch 1)
         (dotimes (i 576) (vector-push-extend (clip16 (aref out0 i)) acc))
         (dotimes (i 576)
           (vector-push-extend (clip16 (aref out0 i)) acc)
           (vector-push-extend (clip16 (aref out1 i)) acc))))
    (:float32
     (flet ((cf (x) (float (max -1.0d0 (min 1.0d0 x)) 1.0f0)))
       (if (= nch 1)
           (dotimes (i 576) (vector-push-extend (cf (aref out0 i)) acc))
           (dotimes (i 576)
             (vector-push-extend (cf (aref out0 i)) acc)
             (vector-push-extend (cf (aref out1 i)) acc)))))))

(defun finalize-samples (acc format nch)
  (declare (ignore nch))
  (let ((etype (ecase format (:pcm16 '(signed-byte 16)) (:float32 'single-float))))
    (make-array (length acc) :element-type etype :initial-contents acc)))

;;; ---- public: one-shot decode -------------------------------------------
(defun decode-mp3 (octets &key (format :pcm16) (start 0) (end (length octets)))
  "Decode MP3 OCTETS to a PCM struct.  FORMAT is :pcm16 (default) or :float32."
  (declare (type octets octets))
  (let* ((d (make-decoder octets :start start :end end))
         (acc (make-array (* 4 1152) :adjustable t :fill-pointer 0)))
    (loop for r = (decode-one-frame
                   d (lambda (out0 out1 nch) (emit-granule acc out0 out1 nch format)))
          while r)
    (let* ((ch (max 1 (decoder-channels d)))
           (samples (finalize-samples acc format ch)))
      (make-pcm :samples samples :channels ch
                :sample-rate (decoder-sample-rate d) :format format
                :frame-count (if (plusp ch) (floor (length samples) ch) 0)))))

(defun decode-mp3-file (path &key (format :pcm16))
  "Decode the MP3 file at PATH to a PCM struct."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence bytes s)
      (decode-mp3 bytes :format format))))
