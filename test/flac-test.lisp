;;;; test/flac-test.lisp — FLAC, held to a standard nothing else in reed can be held to.
;;;;
;;;; Every other decoder here is lossy, so its suite asserts a correlation or an RMS bound against
;;;; ffmpeg.  FLAC is lossless: there is exactly one right answer, and this file checks it twice,
;;;; against two oracles that share nothing.
;;;;
;;;; THE FIRST IS THE FORMAT'S OWN.  STREAMINFO carries the MD5 of the audio the encoder was given.
;;;; Decoding and hashing the result compares this decoder against the ENCODER's record of the
;;;; original, with no reference decoder in the loop at all — if both this and ffmpeg were wrong in
;;;; the same way, this check would still catch it.
;;;;
;;;; THE SECOND IS ffmpeg, byte for byte.  Not a correlation: `equalp' on the raw sample bytes.
;;;;
;;;; And every frame carries a CRC-16 that the decoder checks as it goes, so a desynchronisation of
;;;; one bit anywhere is a stated error rather than plausible-looking noise.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load test/flac-test.lisp
(require :asdf)
(require :sb-md5)
(asdf:load-asd (merge-pathnames "reed.asd" (or *load-truename* *default-pathname-defaults*)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :reed))

(defpackage #:reed-flac-test (:use #:cl)) (in-package #:reed-flac-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun interleave-le (channels total nch bytes)
  "The samples as the MD5 is defined over them: little-endian, interleaved, native depth."
  (let ((raw (make-array (* total nch bytes) :element-type '(unsigned-byte 8))) (o 0))
    (dotimes (i total raw)
      (dotimes (c nch)
        (let ((v (aref (aref channels c) i)))
          (dotimes (k bytes) (setf (aref raw o) (ldb (byte 8 (* 8 k)) v)) (incf o)))))))

(format t "~&== FLAC, against the MD5 the encoder wrote into STREAMINFO~%")
(defvar *stats* (reed:make-flac-stats))
(let ((files (sort (directory "corpus/flac_*.flac") #'string< :key #'namestring)))
  (when (null files) (ok "corpus/flac_*.flac fixtures exist (run test/gen-flac-corpus.sh)" nil))
  (dolist (f files)
    (let ((name (pathname-name f)))
      (handler-case
          (let ((reed:*flac-stats* *stats*))
            (multiple-value-bind (channels total info) (reed:decode-flac-raw (slurp f))
              (let* ((nch (reed:fi-channels info))
                     (bits (reed:fi-bits info))
                     (raw (interleave-le channels total nch (/ bits 8)))
                     (md5 (reed:fi-md5 info)))
                (ok (format nil "~a: ~dch ~d Hz ~d-bit, ~d samples — ~:[no MD5 declared~;MD5 matches~]"
                            name nch (reed:fi-sample-rate info) bits total
                            (and md5 (equalp (coerce md5 'list)
                                             (coerce (sb-md5:md5sum-sequence raw) 'list))))
                    (and md5 (equalp (coerce md5 'list)
                                     (coerce (sb-md5:md5sum-sequence raw) 'list)))))))
        (error (e) (ok (format nil "~a: ~a" name e) nil))))))

(format t "~&== and against ffmpeg's decode, byte for byte~%")
(let ((files (sort (directory "corpus/flac_*.flac") #'string< :key #'namestring)))
  (dolist (f files)
    (let* ((name (pathname-name f))
           (ref (format nil "corpus/~a_ref.s16" name)))
      (handler-case
          (if (not (probe-file ref))
              (ok (format nil "~a: reference decode is present" name) nil)
              (let* ((pcm (reed:decode-flac (slurp f)))
                     (mine (reed:pcm-samples pcm))
                     (want (slurp ref))
                     (n (min (length want) (* 2 (length mine))))
                     (same t))
                ;; PCM-SAMPLES is (signed-byte 16); the reference is raw little-endian s16
                (dotimes (i (floor n 2))
                  (let ((v (aref mine i))
                        (w (let ((u (logior (aref want (* 2 i)) (ash (aref want (1+ (* 2 i))) 8))))
                             (if (>= u #x8000) (- u #x10000) u))))
                    (unless (= v w) (setf same nil) (return))))
                (ok (format nil "~a: ~d samples identical to ffmpeg" name (floor n 2))
                    (and same (= (length want) (* 2 (length mine)))))))
        (error (e) (ok (format nil "~a: ~a" name e) nil))))))

(format t "~&== the paths those fixtures actually took~%")
(let ((s *stats*))
  (ok (format nil "frames decoded: ~d" (reed:fst-frames s)) (plusp (reed:fst-frames s)))
  (ok (format nil "constant subframes: ~d — a channel that holds still" (reed:fst-constant s))
      (plusp (reed:fst-constant s)))
  (ok (format nil "verbatim subframes: ~d — content that would not compress" (reed:fst-verbatim s))
      (plusp (reed:fst-verbatim s)))
  (ok (format nil "fixed-predictor subframes: ~d" (reed:fst-fixed s)) (plusp (reed:fst-fixed s)))
  (ok (format nil "linear-predictor subframes: ~d, highest order ~d"
              (reed:fst-lpc s) (reed:fst-max-lpc-order s))
      (and (plusp (reed:fst-lpc s)) (> (reed:fst-max-lpc-order s) 8)))
  (ok (format nil "subframes using wasted bits: ~d" (reed:fst-wasted s))
      (plusp (reed:fst-wasted s)))
  (ok (format nil "5-bit Rice parameters: ~d partitions" (reed:fst-rice2 s))
      (plusp (reed:fst-rice2 s)))
  (ok (format nil "deepest partition order: ~d" (reed:fst-max-partition-order s))
      (plusp (reed:fst-max-partition-order s)))
  (ok (format nil "stereo: ~d independent, ~d left/side, ~d side/right, ~d mid/side"
              (reed:fst-independent s) (reed:fst-left-side s)
              (reed:fst-side-right s) (reed:fst-mid-side s))
      (and (plusp (reed:fst-left-side s)) (plusp (reed:fst-mid-side s))))
  (format t "~&  --   escaped partitions: ~d (rare; not asserted)~%" (reed:fst-escaped s)))

(format t "~&== what it turns away~%")
(flet ((refuses (name thunk)
         (handler-case (progn (funcall thunk) (ok name nil))
           (reed:flac-error (e) (ok (format nil "~a — ~a" name e) t))
           (error (e) (ok (format nil "~a (wrong condition: ~a)" name e) nil)))))
  (refuses "a stream that does not begin with `fLaC' is named, not guessed at"
           (lambda () (reed:decode-flac (make-array 64 :element-type '(unsigned-byte 8)
                                                       :initial-element 0))))
  ;; corrupting one byte of audio data must be caught by the frame CRC rather than decoded
  (let* ((bytes (copy-seq (slurp "corpus/flac_music_c5.flac")))
         (mid (floor (length bytes) 2)))
    (setf (aref bytes mid) (logxor (aref bytes mid) #xff))
    (refuses "a flipped byte in the middle of the audio is caught by a CRC"
             (lambda () (reed:decode-flac bytes)))))

(format t "~&~:[FLAC OK~;FLAC: ~:*~d FAILED~]~%" (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
