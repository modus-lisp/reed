;;;; src/common/pcm.lisp — the uniform PCM representation shared by every reed
;;;; codec, plus the RIFF/WAVE writer.
;;;;
;;;; Every decoder in reed (MP3 today; AAC-LC and Opus next) produces a PCM
;;;; struct: interleaved samples, channel count, sample rate, and a sample
;;;; format tag.  The G.711 companders operate on the raw sample vectors that
;;;; back a PCM (on-wire buffers), so they interoperate with this type without
;;;; forcing a struct on the RTP path.
(in-package #:reed)

;;; ---- uniform PCM representation -----------------------------------------
(defstruct pcm
  "Decoded PCM, the common currency of every reed codec.  SAMPLES is an
interleaved vector: (signed-byte 16) for :pcm16, single-float [-1,1] for
:float32.  CHANNELS is the interleave width, SAMPLE-RATE is in Hz, FORMAT is the
sample tag (:pcm16 | :float32), and FRAME-COUNT is samples per channel."
  samples
  (channels 2 :type fixnum)
  (sample-rate 44100 :type fixnum)
  (format :pcm16)
  (frame-count 0 :type fixnum))

;;; ---- WAV output ---------------------------------------------------------
(defun pcm->wav-octets (pcm)
  "Serialize PCM (16-bit) into a RIFF/WAVE byte vector."
  (let* ((format (pcm-format pcm))
         (ch (pcm-channels pcm))
         (rate (pcm-sample-rate pcm))
         (src (pcm-samples pcm))
         (nframes (pcm-frame-count pcm))
         (bits 16)
         (block-align (* ch (/ bits 8)))
         (data-bytes (* nframes block-align))
         (out (make-array (+ 44 data-bytes) :element-type '(unsigned-byte 8)))
         (p 0))
    (labels ((wb (b) (setf (aref out p) (logand b #xff)) (incf p))
             (str (s) (loop for c across s do (wb (char-code c))))
             (u16 (v) (wb v) (wb (ash v -8)))
             (u32 (v) (wb v) (wb (ash v -8)) (wb (ash v -16)) (wb (ash v -24))))
      (str "RIFF") (u32 (+ 36 data-bytes)) (str "WAVE")
      (str "fmt ") (u32 16) (u16 1) (u16 ch) (u32 rate)
      (u32 (* rate block-align)) (u16 block-align) (u16 bits)
      (str "data") (u32 data-bytes)
      (ecase format
        (:pcm16 (dotimes (i (* nframes ch)) (u16 (logand (aref src i) #xffff))))
        (:float32 (dotimes (i (* nframes ch))
                    (u16 (logand (max -32768 (min 32767 (round (* (aref src i) 32767.0)))) #xffff))))))
    out))

(defun write-wav-file (pcm path)
  "Write PCM to a .wav file at PATH."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (write-sequence (pcm->wav-octets pcm) s))
  path)

(defun write-wav (pcm destination)
  "Write PCM as WAV to DESTINATION (a pathname/string, or return octets for T)."
  (if (eq destination t)
      (pcm->wav-octets pcm)
      (write-wav-file pcm destination)))
