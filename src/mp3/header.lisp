;;;; src/header.lisp — MPEG audio frame header + container framing.
(in-package #:reed)

(define-condition mp3-error (error)
  ((message :initarg :message :reader mp3-error-message :initform "MP3 decode error"))
  (:report (lambda (c s) (format s "~a" (mp3-error-message c)))))

;; Bitrate tables (bits/s), indexed by header bitrate index 0..15 (0 = free,
;; 15 = invalid).  Layer III only.
(defparameter +bitrate-mpeg1-l3+
  #(0 32000 40000 48000 56000 64000 80000 96000
    112000 128000 160000 192000 224000 256000 320000 0))
(defparameter +bitrate-mpeg2-l3+                 ; MPEG-2 and MPEG-2.5 (LSF)
  #(0 8000 16000 24000 32000 40000 48000 56000
    64000 80000 96000 112000 128000 144000 160000 0))

;; Sample rates (Hz) indexed by the 2-bit samplerate field, per version.
(defparameter +samplerate+
  #(#(44100 48000 32000 0)     ; MPEG-1
    #(22050 24000 16000 0)     ; MPEG-2
    #(11025 12000  8000 0)))   ; MPEG-2.5

(defstruct (frame-header (:conc-name fh-))
  (version :mpeg1)             ; :mpeg1 | :mpeg2 | :mpeg25
  (layer 3 :type fixnum)
  (protection 1 :type fixnum)  ; 0 => 16-bit CRC follows the header
  (bitrate-index 0 :type fixnum)
  (samplerate-index 0 :type fixnum)
  (padding 0 :type fixnum)
  (private 0 :type fixnum)
  (mode 0 :type fixnum)        ; 0 stereo 1 joint 2 dual 3 mono
  (mode-extension 0 :type fixnum)
  (copyright 0 :type fixnum)
  (original 0 :type fixnum)
  (emphasis 0 :type fixnum)
  ;; derived
  (sample-rate 44100 :type fixnum)
  (bitrate 0 :type fixnum)
  (channels 2 :type fixnum)
  (frame-length 0 :type fixnum)
  (samples 1152 :type fixnum)
  (sfband-index 0 :type fixnum))   ; index into the scalefactor-band tables

(declaim (inline valid-frame-sync-p))
(defun valid-frame-sync-p (b0 b1)
  (and (= b0 #xff) (= (logand b1 #xe0) #xe0)))

(defun parse-frame-header (word &key (layer-wanted 3))
  "Parse the 32-bit big-endian header WORD.  Returns a FRAME-HEADER, or NIL if the header is not a
valid frame of the layer asked for.

LAYER-WANTED is a parameter rather than something the caller checks afterwards because this doubles
as the SYNC test: a run of bytes is taken to be a frame header only if it parses as one, and
accepting a layer the caller cannot decode would let an MP3 stream resynchronise onto a false
positive."
  (declare (type (unsigned-byte 32) word))
  (when (/= (logand word #xffe00000) #xffe00000) (return-from parse-frame-header nil))
  (let* ((ver-bits (ldb (byte 2 19) word))
         (layer-bits (ldb (byte 2 17) word))
         (version (ecase ver-bits (0 :mpeg25) (1 nil) (2 :mpeg2) (3 :mpeg1)))
         (layer (case layer-bits (1 3) (2 2) (3 1) (t nil)))
         (br-idx (ldb (byte 4 12) word))
         (sr-idx (ldb (byte 2 10) word)))
    (when (or (null version) (null layer) (/= layer layer-wanted)
              (= br-idx 0) (= br-idx 15) (= sr-idx 3))
      (return-from parse-frame-header nil))
    (let* ((ver-i (ecase version (:mpeg1 0) (:mpeg2 1) (:mpeg25 2)))
           (sr (aref (aref +samplerate+ ver-i) sr-idx))
           ;; LAYER II AND LAYER III DO NOT SHARE A BIT RATE TABLE for MPEG-1 — 384 kbit/s exists
           ;; in one and 40 in the other — though they do for the half-rate versions.
           (bitrate (aref (cond ((/= layer 1) (if (eq version :mpeg1)
                                                  (if (= layer 2) +mp2-bitrate-mpeg1+
                                                      +bitrate-mpeg1-l3+)
                                                  +bitrate-mpeg2-l3+))
                                (t +bitrate-mpeg2-l3+))
                          br-idx))
           (mode (ldb (byte 2 6) word))
           (padding (ldb (byte 1 9) word))
           (mpeg1p (eq version :mpeg1))
           ;; Layer II is 1152 samples a frame whatever the version; Layer III halves for the
           ;; half-rate versions, and the frame length follows the sample count
           (samples (if (or (= layer 2) mpeg1p) 1152 576))
           (flen (+ (floor (* (if (= samples 1152) 144 72) bitrate) sr) padding))
           ;; scalefactor-band table index: 0/1/2 by rate for each version group
           (sfb-idx (ecase version
                      (:mpeg1  sr-idx)             ; 44100/48000/32000
                      (:mpeg2  (+ 3 sr-idx))       ; 22050/24000/16000
                      (:mpeg25 (+ 6 sr-idx)))))    ; 11025/12000/8000
      (when (zerop bitrate) (return-from parse-frame-header nil))
      (make-frame-header
       :version version :layer layer
       :protection (ldb (byte 1 16) word)
       :bitrate-index br-idx :samplerate-index sr-idx
       :padding padding :private (ldb (byte 1 8) word)
       :mode mode :mode-extension (ldb (byte 2 4) word)
       :copyright (ldb (byte 1 3) word) :original (ldb (byte 1 2) word)
       :emphasis (ldb (byte 2 0) word)
       :sample-rate sr :bitrate bitrate
       :channels (if (= mode 3) 1 2)
       :frame-length flen :samples samples :sfband-index sfb-idx))))

(declaim (inline u32be))
(defun u32be (bytes i)
  (declare (type octets bytes) (type fixnum i))
  (logior (ash (aref bytes i) 24) (ash (aref bytes (+ i 1)) 16)
          (ash (aref bytes (+ i 2)) 8) (aref bytes (+ i 3))))

(defun skip-id3v2 (bytes)
  "Return the byte offset past a leading ID3v2 tag, or 0 if none."
  (declare (type octets bytes))
  (if (and (>= (length bytes) 10)
           (= (aref bytes 0) (char-code #\I))
           (= (aref bytes 1) (char-code #\D))
           (= (aref bytes 2) (char-code #\3)))
      (let ((size (logior (ash (logand (aref bytes 6) #x7f) 21)
                          (ash (logand (aref bytes 7) #x7f) 14)
                          (ash (logand (aref bytes 8) #x7f) 7)
                          (logand (aref bytes 9) #x7f))))
        (+ 10 size))
      0))

(defun end-offset (bytes)
  "End offset, trimming a trailing 128-byte ID3v1 tag if present."
  (declare (type octets bytes))
  (let ((n (length bytes)))
    (if (and (>= n 128)
             (= (aref bytes (- n 128)) (char-code #\T))
             (= (aref bytes (- n 127)) (char-code #\A))
             (= (aref bytes (- n 126)) (char-code #\G)))
        (- n 128)
        n)))

(defun find-frame-sync (bytes pos end)
  "Scan for the next byte-aligned, plausible Layer III frame header at or after
POS.  Returns (values offset header) or NIL."
  (declare (type octets bytes) (type fixnum pos end))
  (loop for i of-type fixnum from pos below (- end 3)
        when (valid-frame-sync-p (aref bytes i) (aref bytes (1+ i)))
          do (let ((h (parse-frame-header (u32be bytes i))))
               (when (and h (<= (fh-frame-length h) (- end i)) (>= (fh-frame-length h) 4))
                 (return-from find-frame-sync (values i h))))
        finally (return nil)))

(defun xing/info/vbri-frame-p (bytes off header)
  "True if the frame at OFF is a Xing/Info/VBRI VBR-tag frame (no audio)."
  (declare (type octets bytes) (type fixnum off))
  (let* ((mpeg1p (eq (fh-version header) :mpeg1))
         (mono (= (fh-channels header) 1))
         ;; Xing/Info tag sits after the side info
         (xoff (+ off 4 (cond ((and mpeg1p (not mono)) 32)
                              (mpeg1p 17)
                              (mono 9)
                              (t 17))))
         (vbrioff (+ off 4 32)))
    (flet ((tag= (o a b c d)
             (and (< (+ o 3) (length bytes))
                  (= (aref bytes o) (char-code a)) (= (aref bytes (+ o 1)) (char-code b))
                  (= (aref bytes (+ o 2)) (char-code c)) (= (aref bytes (+ o 3)) (char-code d)))))
      (or (tag= xoff #\X #\i #\n #\g) (tag= xoff #\I #\n #\f #\o)
          (tag= vbrioff #\V #\B #\R #\I)))))
