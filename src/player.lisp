;;;; src/player.lisp — playing a file, as opposed to converting one.
;;;;
;;;; A decoder answers "what were the samples"; a player answers "what are the next 20 ms".
;;;; The difference is not cosmetic.  Something consuming audio in real time — a mixer, an RTP
;;;; sender, a sound device — asks for a small fixed frame on a clock and cannot wait while a
;;;; four-minute file decodes, and it wants those frames at ITS rate and channel count, not the
;;;; file's.  So a SOURCE here is the simplest thing that shape allows: a thunk of no arguments
;;;; returning the next frame of mono samples, or NIL when it has nothing more.
;;;;
;;;; That is deliberately the same contract webrtc-media's :source expects, and the same one a
;;;; mixer wants from each of its inputs, so the same object can be handed to either without an
;;;; adapter.  Nothing in here knows about RTP, a device, or a screen.

(in-package #:reed)

(defun make-buffer-source (samples &key (frame-samples 160) loop)
  "A SOURCE over an in-memory mono vector: successive FRAME-SAMPLES slices, then NIL (or wrap,
with LOOP).  The short final frame is returned as-is rather than padded — the consumer knows
what silence is, and padding here would hide a source that ended early."
  (let ((pos 0) (n (length samples)))
    (lambda ()
      (when (and loop (>= pos n)) (setf pos 0))
      (when (< pos n)
        (let ((end (min n (+ pos frame-samples))))
          (prog1 (subseq samples pos end) (setf pos end)))))))

(defstruct (player (:constructor %make-player))
  "A decoder, a resampler and the samples between them.  Kept as a struct rather than a closure
because a player is a thing with a position, and PLAYER-FINISHED-P / PLAYER-FRAMES-EMITTED are
what a caller asks when the sound stops and it wants to know whether that was the file ending
or the pipeline stalling."
  decoder resampler
  (rate 8000 :type fixnum)
  (frame-samples 160 :type fixnum)
  (gain 1.0d0 :type double-float)
  (pending (make-pcm16 0))              ; resampled mono samples not yet handed out
  (fill 0 :type fixnum)
  (source-rate 0 :type fixnum)
  (source-channels 0 :type fixnum)
  (frames 0 :type fixnum)
  (eof nil)
  (flushed nil))

(defun %player-push (p samples)
  (let ((need (+ (player-fill p) (length samples))))
    (when (< (length (player-pending p)) need)
      (let ((new (make-pcm16 (max need (* 2 (length (player-pending p)))))))
        (replace new (player-pending p) :end2 (player-fill p))
        (setf (player-pending p) new)))
    (replace (player-pending p) samples :start1 (player-fill p))
    (setf (player-fill p) need)))

(defun %player-fill-to (p want)
  "Decode until at least WANT resampled samples are pending, or the file ends."
  (loop while (and (< (player-fill p) want) (not (player-flushed p)))
        do (let ((raw (unless (player-eof p) (decode-next-frame (player-decoder p)))))
             (cond
               (raw
                ;; The rate and channel count are only knowable once a frame header has been
                ;; read, so the resampler cannot exist before the first frame is decoded.
                (unless (player-resampler p)
                  (setf (player-source-rate p) (decoder-sample-rate (player-decoder p))
                        (player-source-channels p) (max 1 (decoder-channels (player-decoder p)))
                        (player-resampler p) (make-resampler (player-source-rate p)
                                                             (player-rate p))))
                (%player-push p (resample (player-resampler p)
                                          (downmix raw (player-source-channels p)))))
               (t
                (setf (player-eof p) t)
                ;; the tail the resampler was holding back for want of input on the right
                (when (player-resampler p)
                  (%player-push p (resample (player-resampler p) (make-pcm16 0) :final t)))
                (setf (player-flushed p) t))))))

(defun player-next-frame (p)
  "The next FRAME-SAMPLES mono samples at the player's rate, or NIL at end of file."
  (%player-fill-to p (player-frame-samples p))
  (when (plusp (player-fill p))
    (let* ((n (min (player-frame-samples p) (player-fill p)))
           (out (subseq (player-pending p) 0 n)))
      (replace (player-pending p) (player-pending p) :start2 n :end2 (player-fill p))
      (decf (player-fill p) n)
      (incf (player-frames p))
      (unless (= 1.0d0 (player-gain p)) (apply-gain out (player-gain p)))
      out)))

(defun player-finished-p (p) (and (player-flushed p) (zerop (player-fill p))))

(defun make-mp3-player (path &key (rate 8000) (frame-samples 160) (gain 1.0d0))
  "Open the MP3 at PATH as a PLAYER delivering FRAME-SAMPLES mono samples at RATE.

Decoding is INCREMENTAL — one MPEG frame at a time, as frames are asked for — so opening a long
file is cheap and a real-time consumer never waits on the whole decode.  The resampling is
stateful across those frames, so the granule boundaries do not tick."
  (let ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                 (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
                   (read-sequence b s) b))))
    (%make-player :decoder (make-decoder bytes) :rate rate
                  :frame-samples frame-samples :gain (float gain 1d0))))

(defun make-mp3-source (path &key (rate 8000) (frame-samples 160) (gain 1.0d0))
  "MAKE-MP3-PLAYER as a bare SOURCE thunk, for a consumer that only wants the next frame."
  (let ((p (make-mp3-player path :rate rate :frame-samples frame-samples :gain gain)))
    (lambda () (player-next-frame p))))

(defun drain-source (source &key (limit 100000))
  "Pull SOURCE dry and return everything it produced as one vector.  For tests and for writing a
file — a real-time consumer must never do this."
  (let ((parts '()) (total 0))
    (loop for f = (funcall source)
          while (and f (< total limit))
          do (push f parts) (incf total (length f)))
    (let ((out (make-pcm16 total)) (at 0))
      (dolist (f (nreverse parts) out)
        (replace out f :start1 at) (incf at (length f))))))
