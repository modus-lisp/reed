;;;; src/opus/decode.lisp — Opus packet driver + public API (Stage 1).
;;;;
;;;; Stage 1 delivers the CELT-only decode path: range coder (range.lisp),
;;;; TOC/packet framing (framing.lisp) and the CELT decoder (celt.lisp).  A
;;;; packet is parsed into its constituent frames; each CELT frame is decoded
;;;; and de-emphasised to 48 kHz interleaved PCM.  SILK, hybrid and Ogg/.opus
;;;; encapsulation are later stages; a packet in a SILK/hybrid config signals
;;;; an opus-error rather than producing wrong audio.
;;;;
;;;; Intended final API (once SILK + Ogg land):
;;;;   (decode-opus octets &key ...)       -> pcm   ; a raw Opus packet stream
;;;;   (decode-opus-file path &key ...)    -> pcm   ; an Ogg-encapsulated .opus
;;;; For now the working, verified entry points are the ones below.
(in-package #:reed)

(defstruct (opus-decoder-state (:constructor %make-opus-decoder-state))
  celt silk (channels 2 :type fixnum) (sample-rate 48000 :type fixnum)
  (last-final-range 0 :type (unsigned-byte 32)))

(defun make-opus-decoder (&key (channels 2))
  "Create a persistent Opus decoder state for CHANNELS output channels."
  (%make-opus-decoder-state :celt (make-celt-decoder channels)
                            :silk (make-silk-decoder) :channels channels))

(defun %silk-internal-fs-khz (bandwidth)
  (ecase bandwidth (:nb 8) (:mb 12) (:wb 16)))

(defun %decode-opus-silk-packet (state toc frames)
  "Route a SILK-only Opus packet (configs 0-11) → float32 48 kHz PCM."
  (let* ((silk (opus-decoder-state-silk state))
         (n-api (opus-decoder-state-channels state))
         (n-internal (if (opus-toc-stereo toc) 2 1))
         (fs-khz (%silk-internal-fs-khz (opus-toc-bandwidth toc)))
         (fsz (opus-toc-frame-size toc))              ; samples @48k per opus frame
         (payload-ms (truncate (* fsz 1000) 48000))
         (total (* fsz (length frames)))
         (i16 (make-array (* n-api total) :element-type 'fixnum :initial-element 0))
         (base 0))
    (dolist (fr frames)
      (let ((d (ec-dec-init (opus-frame-data fr)
                            :base (opus-frame-start fr) :storage (opus-frame-size fr)))
            (decoded 0) (first t))
        (loop while (< decoded fsz) do
          (incf decoded (silk-decode silk d n-internal n-api fs-khz payload-ms first
                                     i16 (+ base decoded)))
          (setf first nil))
        (setf (opus-decoder-state-last-final-range state) (ec-dec-rng d))
        (incf base fsz)))
    (let ((out (make-array (* n-api total) :element-type 'single-float :initial-element 0f0)))
      (dotimes (i (* n-api total))
        (setf (aref out i) (* (float (aref i16 i) 1f0) #.(/ 1f0 32768f0))))
      (make-pcm :samples out :channels n-api :sample-rate 48000
                :format :float32 :frame-count total))))

(defun %celt-endband (bandwidth)
  (ecase bandwidth (:nb 13) (:mb 17) (:wb 17) (:swb 19) (:fb 21)))

(defun %frame-size->lm (fs)
  (ecase fs (120 0) (240 1) (480 2) (960 3)))

(defun decode-opus-packet (state data &key (start 0) (end (length data)))
  "Decode one Opus packet (CELT-only configs) into a float32 PCM struct at
48 kHz.  STATE is a persistent opus-decoder-state (energy/overlap carry across
packets).  Signals opus-error for SILK/hybrid configs (Stage 2/3)."
  (multiple-value-bind (toc frames) (parse-opus-packet data :start start :end end)
    (when (eq (opus-toc-mode toc) :silk)
      (return-from decode-opus-packet (%decode-opus-silk-packet state toc frames)))
    (unless (eq (opus-toc-mode toc) :celt)
      (%opus-err "Stage 1/2 decode CELT (16-31) and SILK (0-11); got ~a/~a (config ~d)"
                 (opus-toc-mode toc) (opus-toc-bandwidth toc) (opus-toc-config toc)))
    (let* ((celt (opus-decoder-state-celt state))
           (lm (%frame-size->lm (opus-toc-frame-size toc)))
           (n (opus-toc-frame-size toc))
           (cc (celt-decoder-channels celt))
           (chunks '()))
      (setf (celt-decoder-stream-channels celt) (opus-toc-channels toc)
            (celt-decoder-start celt) 0
            (celt-decoder-end celt) (%celt-endband (opus-toc-bandwidth toc)))
      (dolist (fr frames)
        (push (celt-decode-frame celt (opus-frame-data fr) (opus-frame-start fr)
                                 (opus-frame-size fr) lm)
              chunks))
      (let* ((chunks (nreverse chunks))
             (total (* n (length chunks)))
             (out (make-array (* cc total) :element-type 'single-float :initial-element 0f0))
             (off 0))
        (dolist (ch chunks)
          (dotimes (i (length ch))
            (setf (aref out (+ off i)) (float (aref ch i) 1f0)))
          (incf off (length ch)))
        (make-pcm :samples out :channels cc :sample-rate 48000
                  :format :float32 :frame-count total)))))

(defun decode-opus-celt (packets &key (channels 2))
  "Decode a sequence of raw CELT-only Opus PACKETS (each an octet vector) into a
single concatenated float32 PCM struct.  Convenience driver over
decode-opus-packet with a fresh persistent state."
  (let* ((state (make-opus-decoder :channels channels))
         (parts (mapcar (lambda (p) (decode-opus-packet state p)) packets))
         (total (reduce #'+ parts :key #'pcm-frame-count))
         (out (make-array (* channels total) :element-type 'single-float :initial-element 0f0))
         (off 0))
    (dolist (p parts)
      (let ((s (pcm-samples p)))
        (replace out s :start1 off)
        (incf off (length s))))
    (make-pcm :samples out :channels channels :sample-rate 48000
              :format :float32 :frame-count total)))
