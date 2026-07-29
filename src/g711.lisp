;;;; src/g711.lisp — ITU-T G.711 companding: PCMU (mu-law) and PCMA (A-law).
;;;;
;;;; 8-bit logarithmic codewords <-> 16-bit linear PCM (ITU-T Rec. G.711).  PCMU
;;;; is RTP payload type 0, PCMA is 8; both 8 kHz mono in WebRTC.  Unlike the
;;;; block codecs (MP3, and AAC/Opus to come) G.711 is stateless per-sample
;;;; companding, so its entry points are buffer transforms — encode a
;;;; (signed-byte 16) PCM vector to (unsigned-byte 8) codewords, and back —
;;;; rather than PCM-struct decoders.  Bit-exact to what webrtc-media shipped on
;;;; the wire; do not perturb the tables/logic.

(in-package #:reed)

(deftype g711-s16 () '(signed-byte 16))
(deftype g711-u8  () '(unsigned-byte 8))

;;; ---- PCMU (mu-law) ---------------------------------------------------------
(defconstant +ulaw-bias+ #x84)
(defconstant +ulaw-clip+ 32635)
(declaim (type (simple-array (unsigned-byte 16) (8)) +ulaw-exp-lut+))
(defparameter +ulaw-exp-lut+
  (make-array 8 :element-type '(unsigned-byte 16)
                :initial-contents '(0 132 396 924 1980 4092 8316 16764)))

(declaim (inline pcmu-encode-1 pcmu-decode-1))
(defun pcmu-encode-1 (sample)
  "One 16-bit PCM SAMPLE -> one mu-law codeword."
  (declare (type g711-s16 sample))
  (let* ((sign (if (minusp sample) #x80 0))
         (mag  (min +ulaw-clip+ (abs sample)))
         (m    (+ mag +ulaw-bias+))
         (hi   (logand (ash m -7) #xff))
         (exp  (if (zerop hi) 0 (1- (integer-length hi))))
         (mant (logand (ash m (- (+ exp 3))) #x0f)))
    (logand (lognot (logior sign (ash exp 4) mant)) #xff)))

(defun pcmu-decode-1 (code)
  "One mu-law CODE -> one 16-bit PCM sample."
  (declare (type g711-u8 code))
  (let* ((u    (logand (lognot code) #xff))
         (sign (logand u #x80))
         (exp  (logand (ash u -4) #x07))
         (mant (logand u #x0f))
         (mag  (+ (aref +ulaw-exp-lut+ exp) (ash mant (+ exp 3)))))
    (if (zerop sign) mag (- mag))))

;;; ---- PCMA (A-law) ----------------------------------------------------------
(declaim (inline pcma-encode-1 pcma-decode-1))
(defun pcma-encode-1 (sample)
  "One 16-bit PCM SAMPLE -> one A-law codeword."
  (declare (type g711-s16 sample))
  (let ((v (ash sample -3)) mask)               ; 16-bit -> 13-bit magnitude domain
    (if (>= v 0) (setf mask #xd5) (setf mask #x55 v (- (- v) 1)))
    (let* ((seg (if (< v 32) 0 (min 7 (integer-length (ash v -5)))))
           (aval (logior (ash seg 4)
                         (if (< seg 2) (logand (ash v -1) #x0f)
                             (logand (ash v (- seg)) #x0f)))))
      (logand (logxor aval mask) #xff))))

(defun pcma-decode-1 (code)
  "One A-law CODE -> one 16-bit PCM sample."
  (declare (type g711-u8 code))
  (let* ((a (logxor code #x55))
         (tt (ash (logand a #x0f) 4))
         (seg (ash (logand a #x70) -4)))
    (case seg
      (0 (incf tt 8))
      (1 (incf tt #x108))
      (t (incf tt #x108) (setf tt (ash tt (1- seg)))))
    (if (zerop (logand a #x80)) (- tt) tt)))

;;; ---- buffer entry points (a frame at a time) -------------------------------
(defun pcmu-encode (pcm)
  "Vector of (signed-byte 16) PCM -> (unsigned-byte 8) mu-law codewords."
  (let ((out (make-array (length pcm) :element-type '(unsigned-byte 8))))
    (dotimes (i (length pcm) out) (setf (aref out i) (pcmu-encode-1 (aref pcm i))))))
(defun pcmu-decode (codes)
  "Vector of mu-law codewords -> (signed-byte 16) PCM."
  (let ((out (make-array (length codes) :element-type '(signed-byte 16))))
    (dotimes (i (length codes) out) (setf (aref out i) (pcmu-decode-1 (aref codes i))))))
(defun pcma-encode (pcm)
  "Vector of (signed-byte 16) PCM -> (unsigned-byte 8) A-law codewords."
  (let ((out (make-array (length pcm) :element-type '(unsigned-byte 8))))
    (dotimes (i (length pcm) out) (setf (aref out i) (pcma-encode-1 (aref pcm i))))))
(defun pcma-decode (codes)
  "Vector of A-law codewords -> (signed-byte 16) PCM."
  (let ((out (make-array (length codes) :element-type '(signed-byte 16))))
    (dotimes (i (length codes) out) (setf (aref out i) (pcma-decode-1 (aref codes i))))))

;;; ---- descriptive aliases ---------------------------------------------------
;;; PCMU/PCMA are the RTP payload names; mu-law/A-law are the ITU names.  Both
;;; spellings are exported so callers can use whichever reads clearest.
(declaim (inline mulaw-encode mulaw-decode alaw-encode alaw-decode))
(defun mulaw-encode (pcm)   (pcmu-encode pcm))
(defun mulaw-decode (codes) (pcmu-decode codes))
(defun alaw-encode (pcm)    (pcma-encode pcm))
(defun alaw-decode (codes)  (pcma-decode codes))
