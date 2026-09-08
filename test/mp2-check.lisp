;;;; test/mp2-check.lisp — MPEG audio Layer II against ffmpeg.
;;;;
;;;; Layer II is specified in FLOATING POINT, so bit-exactness is not the test and would not be
;;;; meaningful: two conforming decoders agree to within the precision of the filterbank.  What is
;;;; measured here is the relative RMS difference from ffmpeg's decode, which for a correct decoder
;;;; sits at the level of fixed-point rounding — around -75 dB — and for a structurally wrong one is
;;;; a fraction, not a fraction of a percent.
;;;;
;;;; The fixtures are deliberately spread: mono and stereo, three sampling rates, and pink and white
;;;; noise as well as tones, because noise exercises every subband and every allocation while a
;;;; tone exercises two.
;;;;
;;;; The fixtures, and the references, are made like this:
;;;;
;;;;   ffmpeg -f lavfi -i "sine=frequency=440:duration=2:sample_rate=48000" \
;;;;          -c:a mp2 -b:a 192k -ac 1 test/vectors/mp2-mono.mp2
;;;;   ...mp2-stereo is two merged sines at 256k, mp2-noise is `anoisesrc=c=pink' at 192k,
;;;;      mp2-44k is a 200 Hz tone at 44100 and 128k mono, mp2-32k is white noise at 32000 and 96k
;;;;   for f in ...; do ffmpeg -i $f.mp2 -f s16le -acodec pcm_s16le $f.pcm; done
;;;;
;;;;   sbcl --non-interactive --load test/mp2-check.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system :reed)

(defpackage #:reed-mp2-test (:use #:cl)) (in-package #:reed-mp2-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))
(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))

(defun rms-vs (samples ref-path)
  "Relative RMS difference between SAMPLES and a raw signed 16-bit little-endian reference."
  (let* ((ref (slurp ref-path))
         (n (min (length samples) (floor (length ref) 2)))
         (num 0d0) (den 0d0))
    (dotimes (i n)
      (let* ((raw (logior (aref ref (* 2 i)) (ash (aref ref (1+ (* 2 i))) 8)))
             (rv (if (>= raw 32768) (- raw 65536) raw))
             (ov (aref samples i)))
        (incf num (expt (float (- rv ov) 1d0) 2))
        (incf den (expt (float rv 1d0) 2))))
    (values (if (plusp den) (sqrt (/ num den)) 1d0) n (floor (length ref) 2))))

(format t "~&== Layer II against ffmpeg~%")
(dolist (spec '(("mp2-mono"   1 48000 . "a tone, mono at 48 kHz")
                ("mp2-stereo" 2 48000 . "two tones, stereo")
                ("mp2-noise"  2 48000 . "pink noise, which uses every subband")
                ("mp2-44k"    1 44100 . "44.1 kHz")
                ("mp2-32k"    2 32000 . "32 kHz, where the allocation table changes")))
  (destructuring-bind (name ch rate . what) spec
    (handler-case
        (let* ((pcm (reed:decode-mp2 (slurp (format nil "test/vectors/~a.mp2" name))))
               (s (reed:pcm-samples pcm)))
          (multiple-value-bind (err n nref) (rms-vs s (format nil "test/vectors/~a.pcm" name))
            (ok (format nil "~a (~a): ~d samples, ~d ch, ~d Hz, relative RMS ~,6f"
                        name what n (reed:pcm-channels pcm) (reed:pcm-sample-rate pcm) err)
                (and (= n nref) (= ch (reed:pcm-channels pcm)) (= rate (reed:pcm-sample-rate pcm))
                     (< err 0.002d0)))))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&~a~%" (if (zerop *fails*) "MP2 OK" (format nil "MP2: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
