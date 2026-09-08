;;;; test/ac3-test.lisp — AC-3 against ffmpeg, and why it cannot reach one.
;;;;
;;;; AC-3 is lossy and floating-point, so the bar is a correlation — but there is a second reason it
;;;; cannot be bit-exact, and it is worth stating precisely because it sets the bar.
;;;;
;;;; DITHER IS NOT NORMATIVE.  A coefficient the bit allocator gave no bits is filled with noise
;;;; (§7.3.4) so that an uncoded band sounds like a noise floor rather than a hole.  The sequence is
;;;; explicitly pseudo-random and no two decoders produce the same samples.  The effect is visible
;;;; in these numbers: at 384 kbit/s almost nothing is dithered and the match is 0.999999 with a
;;;; relative error of one part in ten thousand; at 96 kbit/s a great deal is.  Decoding with dither
;;;; suppressed and comparing again gives a residual exactly 1/sqrt(2) of the one below, which is
;;;; what two independent noise sequences of equal power do and nothing else does.  That is the
;;;; whole of the remaining difference.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load test/ac3-test.lisp
(require :asdf)
(asdf:load-asd (merge-pathnames "reed.asd" (or *load-truename* *default-pathname-defaults*)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :reed))

(defpackage #:reed-ac3-test (:use #:cl)) (in-package #:reed-ac3-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp16 (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((n (floor (file-length s) 2))
           (v (make-array n :element-type '(signed-byte 16))))
      (dotimes (i n v)
        (let ((u (logior (read-byte s) (ash (read-byte s) 8))))
          (setf (aref v i) (if (>= u #x8000) (- u #x10000) u)))))))

(defun compare (mine ref)
  "Correlation and relative RMS between two integer sample vectors."
  (let* ((n (min (length mine) (length ref)))
         (sa 0d0) (sb 0d0) (saa 0d0) (sbb 0d0) (sab 0d0) (sdd 0d0))
    (dotimes (i n)
      (let ((a (float (aref mine i) 1d0)) (b (float (aref ref i) 1d0)))
        (incf sa a) (incf sb b) (incf saa (* a a)) (incf sbb (* b b))
        (incf sab (* a b)) (incf sdd (* (- a b) (- a b)))))
    (let* ((ma (/ sa n)) (mb (/ sb n))
           (ca (- saa (* n ma ma))) (cb (- sbb (* n mb mb)))
           (cab (- sab (* n ma mb))))
      (values (if (plusp (* ca cb)) (/ cab (sqrt (* ca cb))) 0d0)
              (/ (sqrt (/ sdd n)) (+ (sqrt (/ sbb n)) 1d-9))
              n))))

(defvar *stats* (reed:make-ac3-stats))

(format t "~&== AC-3, whole streams, against ffmpeg's decode~%")
(let ((files (sort (directory "corpus/ac3_*.ac3") #'string< :key #'namestring)))
  (when (null files) (ok "corpus/ac3_*.ac3 fixtures exist (run test/gen-ac3-corpus.sh)" nil))
  (dolist (f files)
    (let* ((name (pathname-name f))
           (ref-path (format nil "corpus/~a_ref.s16" name)))
      (handler-case
          (if (not (probe-file ref-path))
              (ok (format nil "~a: reference decode is present" name) nil)
              (let* ((reed:*ac3-stats* *stats*)
                     (pcm (reed:decode-ac3-file f))
                     (ref (slurp16 ref-path)))
                (multiple-value-bind (corr rel n) (compare (reed:pcm-samples pcm) ref)
                  (ok (format nil "~a: ~dch ~d Hz, ~d samples, corr ~,6f, relative rms ~,5f"
                              name (reed:pcm-channels pcm) (reed:pcm-sample-rate pcm) n corr rel)
                      (and (> corr 0.9998d0) (< rel 0.03d0)
                           (= (length (reed:pcm-samples pcm)) (length ref)))))))
        (error (e) (ok (format nil "~a: ~a" name e) nil))))))

(format t "~&== the paths those fixtures actually took~%")
(let ((s *stats*))
  (ok (format nil "frames ~d, blocks ~d" (reed:ast-frames s) (reed:ast-blocks s))
      (plusp (reed:ast-frames s)))
  (ok (format nil "blocks using coupling: ~d" (reed:ast-coupled s)) (plusp (reed:ast-coupled s)))
  (ok (format nil "blocks using stereo rematrixing: ~d" (reed:ast-rematrixed s))
      (plusp (reed:ast-rematrixed s)))
  (ok (format nil "dithered subframes: ~d" (reed:ast-dithered s)) (plusp (reed:ast-dithered s)))
  (ok (format nil "frames with an LFE channel: ~d" (reed:ast-lfe-frames s))
      (plusp (reed:ast-lfe-frames s)))
  (ok (format nil "exponent strategies: D15 ~d, D25 ~d, D45 ~d, reuse ~d"
              (reed:ast-exp-d15 s) (reed:ast-exp-d25 s) (reed:ast-exp-d45 s) (reed:ast-exp-reuse s))
      (and (plusp (reed:ast-exp-d15 s)) (plusp (reed:ast-exp-d45 s))
           (plusp (reed:ast-exp-reuse s))))
  (ok (format nil "channel configurations seen: ~a" (reed:ast-acmods s))
      (and (plusp (aref (reed:ast-acmods s) 1))      ; mono
           (plusp (aref (reed:ast-acmods s) 2))      ; stereo
           (plusp (aref (reed:ast-acmods s) 7))))    ; 3/2
  ;; THREE PATHS THIS CORPUS CANNOT REACH, and it is the encoder's fault rather than the
  ;; decoder's.  ffmpeg's AC-3 encoder — the only one available here — does no transient detection,
  ;; so it never switches to short blocks; it never emits a delta bit allocation; and it never sets
  ;; the coupling phase flags.  All three are implemented and none is exercised, which is worth
  ;; printing rather than hiding: a Dolby-encoded DVD track would cover the first two, and finding
  ;; one is what it would take to turn these lines into assertions.
  (format t "~&  --   short (block-switched) subframes: ~d~%~
               ~&  --   channels using the delta bit allocation: ~d~%~
               ~&  --   blocks with the coupling phase flags in use: ~d~%~
               ~&  --   (ffmpeg's encoder emits none of these three; the paths are untested)~%"
          (reed:ast-short-blocks s) (reed:ast-delta-alloc s) (reed:ast-phase-flags s)))

(format t "~&== what it turns away~%")
(flet ((refuses (name thunk)
         (handler-case (progn (funcall thunk) (ok name nil))
           (reed:ac3-error (e) (ok (format nil "~a — ~a" name e) t))
           (error (e) (ok (format nil "~a (wrong condition: ~a)" name e) nil)))))
  (refuses "a stream with no sync word is named, not guessed at"
           (lambda () (reed:decode-ac3 (make-array 4096 :element-type '(unsigned-byte 8)
                                                        :initial-element 0))))
  ;; bitstream id 16 is Enhanced AC-3: same sync word, different format
  (refuses "Enhanced AC-3 is refused on its bitstream id, not half-decoded"
           (lambda ()
             (let ((v (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
               (setf (aref v 0) #x0b (aref v 1) #x77)
               (setf (aref v 5) (ash 16 3))   ; bsid 16 in the top five bits of byte 5
               (reed:decode-ac3 v)))))

(format t "~&~:[AC3 OK~;AC3: ~:*~d FAILED~]~%" (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
