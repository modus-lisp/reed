;;;; Decode every aac-corpus/*.aac with reed -> aac-corpus/<name>_reed.wav
(require :asdf)
(asdf:load-system "reed")
(dolist (p (directory "aac-corpus/*.aac"))
  (let ((name (pathname-name p)))
    (handler-case
        (let ((pcm (reed:decode-aac-file p)))
          (reed:write-wav pcm (format nil "aac-corpus/~a_reed.wav" name))
          (format t "~&OK   ~a  ch=~a rate=~a frames=~a~%"
                  name (reed:pcm-channels pcm) (reed:pcm-sample-rate pcm)
                  (reed:pcm-frame-count pcm)))
      (error (e) (format t "~&FAIL ~a  ~a~%" name e)))))
(sb-ext:exit)
