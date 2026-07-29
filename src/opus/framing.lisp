;;;; src/opus/framing.lisp — Opus packet framing (RFC 6716 §3).
;;;;
;;;; An Opus packet begins with a one-byte TOC (table-of-contents): a 5-bit
;;;; configuration number (mode + audio bandwidth + frame duration), a stereo
;;;; flag, and a 2-bit frame-count code (0=one frame, 1=two CBR, 2=two VBR,
;;;; 3=an arbitrary count with optional padding).  This file decodes the TOC and
;;;; splits a packet into its constituent compressed frames.  Ogg/.opus page
;;;; demuxing is a later stage; here we consume raw packets and the opus_demo
;;;; test-vector container.  Ported from libopus src/opus.c.
(in-package #:reed)

(define-condition opus-error (error)
  ((message :initarg :message :reader opus-error-message))
  (:report (lambda (c s) (format s "opus-error: ~a" (opus-error-message c)))))

(defun %opus-err (fmt &rest args)
  (error 'opus-error :message (apply #'format nil fmt args)))

;;; ---- TOC decoding -------------------------------------------------------
(defstruct opus-toc
  (config 0 :type fixnum)     ; 0..31
  (stereo nil)                ; t if the TOC stereo bit is set
  (code 0 :type fixnum)       ; frame-count code 0..3
  (mode :celt)                ; :silk | :hybrid | :celt
  (bandwidth :fb)             ; :nb | :mb | :wb | :swb | :fb
  (frame-size 960 :type fixnum) ; samples per frame at 48 kHz
  (channels 1 :type fixnum))

(defun config->mode (config)
  (cond ((< config 12) :silk)
        ((< config 16) :hybrid)
        (t :celt)))

(defun config->bandwidth (config)
  (cond ((< config 4)  :nb)
        ((< config 8)  :mb)
        ((< config 12) :wb)
        ((< config 14) :swb)   ; hybrid SWB
        ((< config 16) :fb)    ; hybrid FB
        ((< config 20) :nb)    ; celt NB
        ((< config 24) :wb)    ; celt WB
        ((< config 28) :swb)   ; celt SWB
        (t :fb)))              ; celt FB

(defun samples-per-frame (toc-byte fs)
  "RFC 6716: frame length in samples at rate FS, from the TOC byte."
  (cond
    ((logtest toc-byte #x80)                 ; CELT-only
     (floor (ash fs (logand (ash toc-byte -3) 3)) 400))
    ((= (logand toc-byte #x60) #x60)         ; hybrid
     (if (logtest toc-byte #x08) (floor fs 50) (floor fs 100)))
    (t                                       ; SILK
     (let ((a (logand (ash toc-byte -3) 3)))
       (if (= a 3) (floor (* fs 60) 1000) (floor (ash fs a) 100))))))

(defun parse-toc (toc-byte &optional (fs 48000))
  "Decode a TOC byte into an OPUS-TOC descriptor."
  (let ((config (logand (ash toc-byte -3) #x1f)))
    (make-opus-toc
     :config config
     :stereo (logtest toc-byte #x04)
     :code (logand toc-byte 3)
     :mode (config->mode config)
     :bandwidth (config->bandwidth config)
     :frame-size (samples-per-frame toc-byte fs)
     :channels (if (logtest toc-byte #x04) 2 1))))

;;; ---- packet -> frames ---------------------------------------------------
(defun %parse-size (data pos len)
  "Return (values frame-size bytes-consumed); frame-size -1 signals truncation."
  (cond
    ((< len 1) (values -1 -1))
    ((< (aref data pos) 252) (values (aref data pos) 1))
    ((< len 2) (values -1 -1))
    (t (values (+ (* 4 (aref data (1+ pos))) (aref data pos)) 2))))

(defstruct opus-frame
  (data nil :type (or null octets))
  (start 0 :type fixnum)
  (size 0 :type fixnum))

(defun parse-opus-packet (data &key (start 0) (end (length data)) (fs 48000))
  "Split an Opus packet into (values toc list-of-opus-frame).  Follows the
RFC 6716 §3.2 code-0/1/2/3 rules including code-3 padding + length fields."
  (let* ((len (- end start))
         (pos start))
    (when (<= len 0) (%opus-err "empty packet"))
    (let* ((toc-byte (aref data pos))
           (toc (parse-toc toc-byte fs))
           (framesize (samples-per-frame toc-byte 48000))
           (sizes (make-array 48 :initial-element 0))
           (count 1) (cbr nil) (pad 0))
      (incf pos) (decf len)
      (let ((last-size len))
        (ecase (logand toc-byte 3)
          (0 (setf count 1))
          (1 (setf count 2 cbr t)
             (when (oddp len) (%opus-err "code 1: odd length"))
             (setf last-size (floor len 2)
                   (aref sizes 0) last-size))
          (2 (setf count 2)
             (multiple-value-bind (s b) (%parse-size data pos len)
               (setf (aref sizes 0) s)
               (decf len b) (incf pos b)
               (when (or (< s 0) (> s len)) (%opus-err "code 2: bad size"))
               (setf last-size (- len s))))
          (3
           (when (< len 1) (%opus-err "code 3: truncated"))
           (let ((ch (aref data pos)))
             (incf pos) (decf len)
             (setf count (logand ch #x3f))
             (when (or (<= count 0) (> (* framesize count) 5760))
               (%opus-err "code 3: bad frame count ~d" count))
             (when (logtest ch #x40)          ; padding present
               (let (p)
                 (loop
                   (when (<= len 0) (%opus-err "code 3: padding overflow"))
                   (setf p (aref data pos)) (incf pos) (decf len)
                   (let ((tmp (if (= p 255) 254 p)))
                     (decf len tmp) (incf pad tmp))
                   (when (/= p 255) (return)))))
             (when (< len 0) (%opus-err "code 3: negative length"))
             (setf cbr (not (logtest ch #x80)))
             (cond
               ((not cbr)                      ; VBR
                (setf last-size len)
                (dotimes (i (1- count))
                  (multiple-value-bind (s b) (%parse-size data pos len)
                    (setf (aref sizes i) s)
                    (decf len b) (incf pos b)
                    (when (or (< s 0) (> s len)) (%opus-err "code 3 VBR: bad size"))
                    (decf last-size (+ b s))))
                (when (< last-size 0) (%opus-err "code 3 VBR: negative last")))
               (t                              ; CBR
                (setf last-size (floor len count))
                (when (/= (* last-size count) len) (%opus-err "code 3 CBR: not divisible"))
                (dotimes (i (1- count)) (setf (aref sizes i) last-size)))))))
        (when (> last-size 1275) (%opus-err "last frame too large: ~d" last-size))
        (setf (aref sizes (1- count)) last-size)
        ;; slice out the frames (data pointer now sits at the first frame)
        (let ((frames '()))
          (dotimes (i count)
            (push (make-opus-frame :data data :start pos :size (aref sizes i)) frames)
            (incf pos (aref sizes i)))
          pad                                   ; parsed but unused (skipped)
          (values toc (nreverse frames)))))))
