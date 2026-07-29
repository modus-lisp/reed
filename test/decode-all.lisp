(ql:quickload :reed :silent t)
(in-package :reed)
(dolist (mp3 (directory "corpus/*.mp3"))
  (let ((name (pathname-name mp3)))
    (handler-case
        (let ((pcm (decode-mp3-file mp3)))
          (write-wav pcm (format nil "test/out-~a.wav" name))
          (format t "~a ch=~a rate=~a frames=~a~%" name
                  (pcm-channels pcm) (pcm-sample-rate pcm) (pcm-frame-count pcm)))
      (error (e) (format t "~a FAILED: ~a~%" name e)))))
