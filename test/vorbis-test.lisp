;;;; test/vorbis-test.lisp — the Vorbis decoder against libvorbis, through ffmpeg.
;;;;
;;;; A LOSSY FLOAT CODEC CANNOT BE BIT-EXACT and the comparison says so honestly: the oracle is
;;;; ffmpeg's own decode of the same file, and the bar is a correlation of 1.000000 with a relative
;;;; RMS error under a ten-thousandth.  What that number actually shows in practice is a difference
;;;; of a few hundredths of one LSB out of 32768, which is double-vs-float rounding and nothing else.
;;;;
;;;; The fixtures are chosen for the PATHS they take, not to be a sample of music: clicks force
;;;; short blocks and therefore the hybrid windows, stereo forces square polar coupling, q0 and q10
;;;; produce completely different codebook sets, and 22 kHz moves the whole floor.  The suite counts
;;;; those paths and asserts the counts are positive, because a fixture re-encoded some day with
;;;; different settings could quietly stop covering one and nothing else would notice.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load test/vorbis-test.lisp
(require :asdf)
(asdf:load-asd (merge-pathnames "reed.asd" (or *load-truename* *default-pathname-defaults*)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :reed))

(defpackage #:reed-vorbis-test (:use #:cl)) (in-package #:reed-vorbis-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun compare (ref tst)
  "Run test/compare.py and return (values correlation normalised-rms)."
  (let* ((out (with-output-to-string (s)
                (sb-ext:run-program "python3" (list "test/compare.py" ref tst)
                                    :search t :output s :error nil)))
         (corr (let ((p (search "corr=" out))) (and p (read-from-string out nil nil :start (+ p 5)))))
         (nrms (let ((p (search "nrms=" out))) (and p (read-from-string out nil nil :start (+ p 5))))))
    (values corr nrms)))

;;; ---- every fixture, against ffmpeg's own decode of it -------------------------------------------

(format t "~&== Vorbis, whole files, against ffmpeg~%")
(let ((files (sort (directory "corpus/vorbis_*.ogg") #'string< :key #'namestring))
      (totals (list :long 0 :short 0 :hybrid 0 :coupled 0 :unused 0)))
  (when (null files) (ok "corpus/vorbis_*.ogg fixtures exist (run test/gen-vorbis-corpus.sh)" nil))
  (dolist (f files)
    (let* ((name (pathname-name f))
           (ref (format nil "corpus/~a_ref.wav" name))
           (out (format nil "test/out-~a.wav" name)))
      (handler-case
          (multiple-value-bind (packets granule)
              (reed::%reassemble-vorbis-packets (reed::%parse-ogg-pages (slurp f)))
            (declare (ignore granule))
            (let* ((info (reed:parse-identification (first packets)))
                   (setup (reed:parse-setup (third packets) info))
                   (d (reed:make-vorbis-decoder setup)))
              ;; decode through the same path the file API uses, then account for what it covered
              (let ((pcm (reed:decode-vorbis-file f)))
                (reed:write-wav-file pcm out))
              (dolist (pk (cdddr packets)) (reed:decode-audio-packet d pk))
              (incf (getf totals :long) (reed:vd-long-blocks d))
              (incf (getf totals :short) (reed:vd-short-blocks d))
              (incf (getf totals :hybrid) (reed:vd-hybrid-windows d))
              (incf (getf totals :coupled) (reed:vd-coupled-blocks d))
              (incf (getf totals :unused) (reed:vd-unused-channels d))
              (if (probe-file ref)
                  (multiple-value-bind (corr nrms) (compare ref out)
                    (ok (format nil "~a: ~dch ~d Hz, corr ~,6f, relative RMS ~,6f~@[ (~d short blocks)~]"
                                name (reed:pcm-channels (reed:decode-vorbis-file f))
                                (reed:vi-rate info) corr nrms
                                (and (plusp (reed:vd-short-blocks d)) (reed:vd-short-blocks d)))
                        (and corr nrms (> corr 0.99999d0) (< nrms 1d-4))))
                  (ok (format nil "~a: reference decode is present" name) nil))))
        (error (e) (ok (format nil "~a: ~a" name e) nil)))))
  (format t "~&== the paths those fixtures actually took~%")
  (ok (format nil "long blocks decoded: ~d" (getf totals :long)) (plusp (getf totals :long)))
  (ok (format nil "short blocks decoded: ~d — block switching is exercised" (getf totals :short))
      (plusp (getf totals :short)))
  (ok (format nil "hybrid windows: ~d — a long block lapped against a short one" (getf totals :hybrid))
      (plusp (getf totals :hybrid)))
  (ok (format nil "blocks with channel coupling: ~d" (getf totals :coupled))
      (plusp (getf totals :coupled)))
  (ok (format nil "channels with no floor at all: ~d — the `unused' path" (getf totals :unused))
      (plusp (getf totals :unused))))

;;; ---- what it turns away -------------------------------------------------------------------------

(format t "~&== refusals~%")
(flet ((refuses (name thunk)
         (handler-case (progn (funcall thunk) (ok name nil))
           (reed:vorbis-error (e) (ok (format nil "~a — ~a" name e) t))
           (error (e) (ok (format nil "~a (wrong condition: ~a)" name e) nil)))))
  (refuses "a stream that is not Vorbis is named, not guessed at"
           (lambda () (reed:parse-identification
                       (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))))
  (let ((pk (copy-seq (subseq (slurp (first (directory "corpus/vorbis_sine440.ogg"))) 0 0))))
    (declare (ignore pk)))
  (refuses "a truncated identification header is refused, not half-read"
           (lambda ()
             (let ((v (make-array 8 :element-type '(unsigned-byte 8))))
               (setf (aref v 0) 1)
               (loop for c across "vorbis" for i from 1 do (setf (aref v i) (char-code c)))
               (reed:parse-identification v)))))

(format t "~&~:[VORBIS OK~;VORBIS: ~:*~d FAILED~]~%" (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
