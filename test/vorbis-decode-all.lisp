;;;; test/vorbis-decode-all.lisp — decode every Vorbis fixture and write a .wav beside it.
;;;;   sbcl --non-interactive --load test/vorbis-decode-all.lisp
(require :asdf)
(asdf:load-asd (merge-pathnames "reed.asd" (or *load-truename* *default-pathname-defaults*)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :reed))

(let ((files (sort (directory "corpus/vorbis_*.ogg") #'string< :key #'namestring)))
  (when (null files) (format t "~&no corpus/vorbis_*.ogg fixtures~%"))
  (dolist (f files)
    (let ((name (pathname-name f)))
      (handler-case
          (let* ((t0 (get-internal-real-time))
                 (pcm (reed:decode-vorbis-file f))
                 (secs (/ (- (get-internal-real-time) t0)
                          internal-time-units-per-second)))
            (reed:write-wav-file pcm (format nil "test/out-~a.wav" name))
            (format t "~&~24a ~2dch ~6d Hz ~8d frames  ~,2fs~%"
                    name (reed:pcm-channels pcm) (reed:pcm-sample-rate pcm)
                    (reed:pcm-frame-count pcm) (float secs)))
        (error (e) (format t "~&~24a FAILED: ~a~%" name e))))))
