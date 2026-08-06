;;;; mp4-delay-gate.lisp — where an m4a's audio actually starts.
;;;;
;;;;     python3 test/gen-mp4-delay.py        # fixtures; aac-corpus/ is ignored
;;;;     sbcl --dynamic-space-size 4096 --disable-debugger \
;;;;       --load inspect/mp4-delay-gate.lisp
;;;;
;;;; An AAC encoder cannot start at sample zero: its filterbank needs a frame of
;;;; overlap before it emits anything, so the first ~1024 samples of a correct
;;;; decode are the encoder warming up rather than the recording.  How many is
;;;; not in the bitstream — it is the container's to declare, either in the
;;;; standard edts/elst edit list or in Apple's iTunSMPB text tag.  A decoder
;;;; that reads neither plays the whole file some tens of milliseconds late.
;;;;
;;;; That is a lag, and a lag is exactly what the existing AAC comparison cannot
;;;; see: test/aac-compare.py cross-correlates to find its own alignment before
;;;; it measures anything, so a decode that is uniformly 57 ms late scores a
;;;; correlation of 1.000000 and passes. It did pass, for as long as this went
;;;; unnoticed. What finally showed it was a speech recognizer reading the same
;;;; file twice, through reed and through ffmpeg, and disagreeing with itself
;;;; about two words.
;;;;
;;;; So this gate asserts the alignment rather than measuring quality: the lag
;;;; against a reference decode must be exactly zero, an integer and not a
;;;; tolerance.  It also runs each fixture with :TRIM NIL and requires the lag
;;;; to come back as exactly the delay the container declared, which is what
;;;; distinguishes "we removed the priming" from "we happen to agree".
;;;;
;;;; The two fixtures are the two mechanisms, and test/gen-mp4-delay.py builds
;;;; each so that only its own mechanism can produce the right answer — the
;;;; iTunSMPB file has its elst zeroed on purpose, because a fixture that
;;;; declares its delay twice would pass while reading either one.

(require :asdf)
(asdf:load-system "reed")
(in-package #:reed)

(defparameter *corpus* #p"aac-corpus/")

(defun read-wav-16 (path)
  "The reference decode as (values samples channels).  A gate needs to read a
wav and reed only writes them, so this is deliberately the smallest reader that
handles what ffmpeg writes rather than a general one."
  (let ((b (with-open-file (s path :element-type '(unsigned-byte 8))
             (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
               (read-sequence v s) v))))
    (flet ((u16 (p) (logior (aref b p) (ash (aref b (+ p 1)) 8)))
           (u32 (p) (logior (aref b p) (ash (aref b (+ p 1)) 8)
                            (ash (aref b (+ p 2)) 16) (ash (aref b (+ p 3)) 24)))
           (name= (p s) (loop for i below 4 always (= (aref b (+ p i)) (char-code (char s i))))))
      (let ((p 12) (channels 0))
        (loop
          (when (> (+ p 8) (length b)) (error "no data chunk in ~a" path))
          (let ((size (u32 (+ p 4))))
            (cond ((name= p "fmt ") (setf channels (u16 (+ p 10))))
                  ((name= p "data")
                   (let* ((n (floor size 2))
                          (v (make-array n :element-type '(signed-byte 16))))
                     (dotimes (i n)
                       (let ((x (u16 (+ p 8 (* i 2)))))
                         (setf (aref v i) (if (>= x 32768) (- x 65536) x))))
                     (return (values v channels)))))
            (incf p (+ 8 size (logand size 1)))))))))

(defun channel-0 (samples channels)
  (let* ((n (floor (length samples) channels))
         (v (make-array n :element-type 'double-float)))
    (dotimes (i n v) (setf (aref v i) (float (aref samples (* i channels)) 1d0)))))

(defun corr-at (a b lag n)
  "A shifted forward by LAG against B."
  (let ((sa 0d0) (sb 0d0) (sab 0d0))
    (dotimes (i n)
      (let ((x (aref a (+ i lag))) (y (aref b i)))
        (incf sa (* x x)) (incf sb (* y y)) (incf sab (* x y))))
    (if (or (zerop sa) (zerop sb)) 0d0 (/ sab (sqrt (* sa sb))))))

(defun best-lag (a b max-lag)
  "The integer shift of A against B that agrees best, positive if A runs ahead."
  (let ((n (min (- (length a) max-lag) (- (length b) max-lag)))
        (best 0) (bestc -2d0))
    (loop for lag from (- max-lag) to max-lag
          for c = (if (minusp lag) (corr-at b a (- lag) n) (corr-at a b lag n))
          when (> c bestc) do (setf bestc c best lag))
    (values best bestc)))

(defvar *failures* 0)

(defun check (label got want)
  (let ((ok (eql got want)))
    (unless ok (incf *failures*))
    (format t "~&  ~:[FAIL~;ok  ~] ~a: ~a~@[ (wanted ~a)~]~%" ok label got (unless ok want))))

(defun run-case (name declared)
  (format t "~&~a, container declares ~d samples of encoder delay~%" name declared)
  (multiple-value-bind (ref channels) (read-wav-16 (merge-pathnames "delay_ref.wav" *corpus*))
    (let* ((r (channel-0 ref channels))
           (path (merge-pathnames name *corpus*))
           (kept (decode-aac-file path))
           (raw (decode-aac-file path :trim nil)))
      (multiple-value-bind (lag c) (best-lag (channel-0 (pcm-samples raw) (pcm-channels raw)) r 4096)
        (format t "    untrimmed: corr ~,6f~%" c)
        (check "untrimmed lag is the declared delay" lag declared))
      (multiple-value-bind (lag c) (best-lag (channel-0 (pcm-samples kept) (pcm-channels kept)) r 4096)
        (format t "    trimmed:   corr ~,6f~%" c)
        (check "trimmed lag" lag 0)
        (when (< c 0.9999d0) (incf *failures*)
          (format t "~&  FAIL correlation ~,6f below 0.9999~%" c)))
      (check "frames dropped" (- (pcm-frame-count raw) (pcm-frame-count kept)) declared))))

(format t "~&MP4 encoder-delay gate: the lag against ffmpeg's decode, as an integer~2%")
(run-case "delay_elst.m4a" 1024)
(run-case "delay_itunsmpb.m4a" 1024)
(format t "~&~%~:[GATE GREEN — both mechanisms read, both files start where they should~;~:*~d CHECK(S) FAILED~]~%"
        (if (zerop *failures*) nil *failures*))
(sb-ext:quit :unix-status (if (zerop *failures*) 0 1))
