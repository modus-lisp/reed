;;;; Decode an opus_demo .bit stream (any mode) with reed, verify the per-packet
;;;; encoder final range, and dump 16-bit interleaved PCM for opus_compare.
(ql:quickload "reed" :verbose nil)
(in-package #:reed)

(defun be32 (v p) (logior (ash (aref v p) 24) (ash (aref v (+ p 1)) 16)
                          (ash (aref v (+ p 2)) 8) (aref v (+ p 3))))

(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun decode-bit-file (bitpath outpath channels)
  (let* ((buf (read-octets bitpath))
         (len (length buf)) (p 0)
         (state (make-opus-decoder :channels channels))
         (npk 0) (nmatch 0) (samples '()) (nframes 0) (first-bad nil))
    (loop while (< (+ p 8) len) do
      (let ((plen (be32 buf p)) (frng (be32 buf (+ p 4))))
        (incf p 8)
        (when (> (+ p plen) len) (return))
        (incf npk)
        (let ((pcm (decode-opus-packet state buf :start p :end (+ p plen))))
          (push (pcm-samples pcm) samples)
          (incf nframes (pcm-frame-count pcm))
          (if (= (opus-decoder-state-last-final-range state) frng)
              (incf nmatch)
              (when (null first-bad) (setf first-bad npk))))
        (incf p plen)))
    (with-open-file (o outpath :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create)
      (dolist (s (nreverse samples))
        (loop for f across s do
          (let* ((x (* f 32768d0))
                 (i (round (max -32768d0 (min 32767d0 x)))))
            (write-byte (logand i #xff) o) (write-byte (logand (ash i -8) #xff) o)))))
    (format t "~a: ~d packets, final-range ~d/~d~@[ (first mismatch #~d)~], ~d frames~%"
            (file-namestring bitpath) npk nmatch npk (and (< nmatch npk) first-bad) nframes)
    (values npk nmatch)))

(let ((args (last sb-ext:*posix-argv* 3)))
  (decode-bit-file (first args) (second args) (parse-integer (third args))))
