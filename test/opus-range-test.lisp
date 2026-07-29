;;;; Standalone unit test for reed's Opus range decoder against a byte stream
;;;; produced by libopus's ec_enc. Generate the fixtures with a libopus-linked
;;;; harness (ec_enc a mixed program, dumping ectest.bin + ectest.program.txt)
;;;; and point $REED_ECTEST_DIR at the directory holding them.
(let ((base (merge-pathnames "../src/" (or *load-pathname*
                                           *default-pathname-defaults*))))
  (load (merge-pathnames "common/packages.lisp" base))
  (load (merge-pathnames "common/bitreader.lisp" base))
  (load (merge-pathnames "opus/range.lisp" base)))

(in-package #:reed)

(defun read-file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defparameter *ectest-dir*
  (or #+sbcl (sb-ext:posix-getenv "REED_ECTEST_DIR")
      #-sbcl nil
      "opus-build/"))

(let* ((buf (read-file-octets (merge-pathnames "ectest.bin" *ectest-dir*)))
       (icdf (make-array 6 :element-type '(unsigned-byte 8)
                           :initial-contents '(60 40 25 12 4 0)))
       (d (ec-dec-init buf :storage (length buf)))
       (fails 0) (n 0))
  (with-open-file (in (merge-pathnames "ectest.program.txt" *ectest-dir*))
    (loop for line = (read-line in nil) while line do
      (incf n)
      (let* ((toks (with-input-from-string (s line)
                     (loop for x = (read s nil) while x collect x)))
             (op (first toks)))
        (ecase op
          (E (destructuring-bind (fl fh ft) (rest toks)
               (let ((s (ec-decode d ft)))
                 (unless (and (<= fl s) (< s fh))
                   (incf fails) (when (< fails 6) (format t "E fail line ~d: s=~d not in [~d,~d) ft=~d~%" n s fl fh ft)))
                 (ec-dec-update d fl fh ft))))
          (B (destructuring-bind (v v1 bits) (rest toks)
               (let ((s (ec-decode-bin d bits)))
                 (unless (= s v) (incf fails) (when (< fails 6) (format t "B fail line ~d: s=~d v=~d~%" n s v)))
                 (ec-dec-update d v v1 (ash 1 bits)))))
          (L (destructuring-bind (val logp) (rest toks)
               (let ((r (ec-dec-bit-logp d logp)))
                 (unless (= r val) (incf fails) (when (< fails 6) (format t "L fail line ~d: r=~d val=~d~%" n r val))))))
          (I (destructuring-bind (sym) (rest toks)
               (let ((r (ec-dec-icdf d icdf 6)))
                 (unless (= r sym) (incf fails) (when (< fails 6) (format t "I fail line ~d: r=~d sym=~d~%" n r sym))))))
          (U (destructuring-bind (v ft) (rest toks)
               (let ((r (ec-dec-uint d ft)))
                 (unless (= r v) (incf fails) (when (< fails 6) (format t "U fail line ~d: r=~d v=~d ft=~d~%" n r v ft))))))
          (R (destructuring-bind (v bits) (rest toks)
               (let ((r (ec-dec-bits d bits)))
                 (unless (= r v) (incf fails) (when (< fails 6) (format t "R fail line ~d: r=~d v=~d bits=~d~%" n r v bits))))))))))
  (format t "~%Replayed ~d ops, ~d failures~%" n fails)
  (format t "reed final range = ~d (rng register)~%" (ec-dec-rng d))
  (format t "expected         = 81391560~%")
  (if (and (zerop fails) (= (ec-dec-rng d) 81391560))
      (format t "RANGE DECODER: PASS (bit-exact + final range matches)~%")
      (format t "RANGE DECODER: FAIL~%")))
