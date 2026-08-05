;;;; inspect/dsp-gate.lisp — the arithmetic between a decoder and a device.
;;;;
;;;; reed's codec gates ask whether the samples are right.  These ask whether what happens to
;;;; them afterwards is right, which is a different and much easier thing to get subtly wrong:
;;;; a resampler that drops samples still produces audio, at the correct rate, of the correct
;;;; length, that sounds roughly like the input.  It is only wrong in the frequency domain, and
;;;; only where the source had content above the new Nyquist — which is exactly where music has
;;;; content and test tones do not.
;;;;
;;;; So the central check here feeds a 6 kHz tone into a 44100 -> 8000 conversion and insists the
;;;; 2 kHz alias it would fold down to is NOT there.  Linear interpolation passes every other
;;;; test in this file and fails that one by about 40 dB.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/dsp-gate.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :reed)))

(defpackage #:reed-dsp-gate (:use #:cl)) (in-package #:reed-dsp-gate)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defun goertzel (pcm hz rate)
  (let* ((n (length pcm)) (k (round (/ (* n hz) rate))) (w (/ (* 2d0 pi k) n))
         (coeff (* 2d0 (cos w))) (s0 0d0) (s1 0d0) (s2 0d0))
    (dotimes (i n) (setf s0 (+ (aref pcm i) (* coeff s1) (- s2)) s2 s1 s1 s0))
    (/ (sqrt (max 0d0 (+ (* s1 s1) (* s2 s2) (- (* s1 s2 coeff))))) n)))

(defun tone (hz rate secs &key (amp 8000))
  (let* ((n (round (* rate secs))) (v (reed:make-pcm16 n)))
    (dotimes (i n v)
      (setf (aref v i) (round (* amp (sin (/ (* 2 pi hz i) rate))))))))

(defun corpus (name)
  (merge-pathnames name (merge-pathnames "corpus/" (asdf:system-source-directory :reed))))

(defun drain-player (p)
  "Every sample a player will give, joined."
  (let ((frames '()))
    (loop for f = (reed:player-next-frame p) while f do (push f frames))
    (let* ((frames (nreverse frames))
           (all (reed:make-pcm16 (reduce #'+ frames :key #'length)))
           (at 0))
      (dolist (f frames all) (replace all f :start1 at) (incf at (length f))))))

(defun rms (v &optional (start 0) (end (length v)))
  (if (<= end start) 0d0
      (let ((s 0d0))
        (loop for i from start below end do (incf s (expt (float (aref v i) 1d0) 2)))
        (sqrt (/ s (- end start))))))

(defun envelope (v &optional (window 800))
  "Loudness over time: the RMS of each WINDOW samples."
  (let ((n (floor (length v) window)))
    (make-array n :initial-contents
                (loop for i below n collect (rms v (* i window) (* (1+ i) window))))))

(defun envelope-align (part whole &optional (window 800))
  "Where in WHOLE does PART begin, in samples, judged by loudness over time.
Sample-exact comparison is not available across a seek — the resampler's phase and the bit
reservoir both restart — but the shape of the music does not move, so the best-matching offset
answers the question the test is actually asking: did the seek land where it was asked to?"
  (let* ((ep (envelope part window)) (ew (envelope whole window))
         (span (min (length ep) 12))       ; ~1.2 s of envelope is plenty to place it
         (best 0) (best-err nil))
    (loop for off from 0 to (max 0 (- (length ew) span))
          for err = (loop for i below span
                          sum (expt (- (aref ep i) (aref ew (+ off i))) 2))
          when (or (null best-err) (< err best-err))
            do (setf best-err err best off))
    (* best window)))

;;; ---- 1. downmix ------------------------------------------------------------
(format t "~&=== downmix ===~%")
(let ((stereo (make-array 8 :element-type '(signed-byte 16)
                            :initial-contents '(1000 1000 500 500 -2000 -2000 0 0))))
  (check "identical channels keep their level, they do not double"
         (coerce (reed:downmix stereo 2) 'list) '(1000 500 -2000 0)))
(let ((opposed (make-array 4 :element-type '(signed-byte 16)
                             :initial-contents '(1000 -1000 800 -800))))
  (check "opposed channels cancel" (coerce (reed:downmix opposed 2) 'list) '(0 0)))
(let ((mono (reed:make-pcm16 4)))
  (check-that "mono passes through untouched" (eq mono (reed:downmix mono 1))))

;;; ---- 2. gain and the mixer -------------------------------------------------
(format t "~&~%=== gain and the mixer ===~%")
(let ((v (make-array 3 :element-type '(signed-byte 16) :initial-contents '(100 -200 300))))
  (check "gain scales" (coerce (reed:apply-gain (copy-seq v) 2.0d0) 'list) '(200 -400 600))
  (check "gain clips rather than wrapping"
         (coerce (reed:apply-gain (copy-seq v) 1000.0d0) 'list) '(32767 -32768 32767)))
(let ((a (make-array 3 :element-type '(signed-byte 16) :initial-contents '(100 200 300)))
      (b (make-array 3 :element-type '(signed-byte 16) :initial-contents '(10 20 30))))
  (check "two sources sum" (coerce (reed:mix (list a b)) 'list) '(110 220 330))
  (check "with per-source gain" (coerce (reed:mix (list a b) :gains '(2.0d0 0.5d0)) 'list)
         '(205 410 615)))
(let ((a (make-array 2 :element-type '(signed-byte 16) :initial-contents '(30000 -30000)))
      (b (make-array 2 :element-type '(signed-byte 16) :initial-contents '(30000 -30000))))
  (check "a loud mix clips, it does not wrap" (coerce (reed:mix (list a b)) 'list)
         '(32767 -32768)))
;; two sources that cancel must come out quiet — which they only do if the clip happens once,
;; at the end, rather than per source on the way in
(let ((a (make-array 2 :element-type '(signed-byte 16) :initial-contents '(30000 30000)))
      (b (make-array 2 :element-type '(signed-byte 16) :initial-contents '(-30000 -30000))))
  (check "and sources that cancel come out silent, not clipped twice"
         (coerce (reed:mix (list a b)) 'list) '(0 0)))
(let ((long (make-array 4 :element-type '(signed-byte 16) :initial-contents '(1 2 3 4)))
      (short (make-array 2 :element-type '(signed-byte 16) :initial-contents '(10 20))))
  (check "a source that ended early does not truncate the others"
         (coerce (reed:mix (list long short)) 'list) '(11 22 3 4)))

;;; ---- 3. rate conversion ----------------------------------------------------
(format t "~&~%=== rate conversion, 44100 -> 8000 ===~%")
;; Measure on an INTERIOR slice of exactly 800 samples.  Two reasons, both about the measurement
;; rather than the filter: 800 samples at 8 kHz puts every frequency here on an exact DFT bin
;; (measuring 1 kHz over the full 7988-sample output loses 2/pi to scalloping and looks like a
;; 3.9 dB gain error that is not there), and skipping the first 1000 samples steps past the
;; kernel's start-up transient.
(defun band (out hz) (goertzel (subseq out 1000 1800) hz 8000))

(let* ((rs (reed:make-resampler 44100 8000))
       (in (tone 1000d0 44100 1.0))
       (out (reed:resample rs in :final t)))
  (check-that "the output length follows the rate ratio"
              (< (abs (- (length out) 8000)) 40)
              (format nil "~d samples for 1.0 s at 8 kHz" (length out)))
  ;; A sine of amplitude A reads as A/2 in this normalization, so 8000 in should read 4000.
  (let ((at1k (band out 1000d0)))
    (check-that "a 1 kHz tone survives at full level"
                (> at1k 3900d0)
                (format nil "1 kHz reads ~,0f, want ~~4000 for an 8000-amplitude input" at1k))))

;; The gain, checked without a window in the way at all: a constant in must be the same constant
;; out, which is true exactly when the kernel sums to 1.  If the filter had a gain error, this is
;; where it shows up unambiguously.
(let* ((dc (make-array 44100 :element-type '(signed-byte 16) :initial-element 5000))
       (out (reed:resample (reed:make-resampler 44100 8000) dc :final t))
       (interior (subseq out 200 (- (length out) 200))))
  (check-that "unity gain — a constant comes through unchanged"
              (every (lambda (s) (< (abs (- s 5000)) 5)) interior)
              (format nil "interior ranges ~d..~d for 5000 in"
                      (reduce #'min interior) (reduce #'max interior))))

;;; THE one that matters.  6 kHz is above the 4 kHz Nyquist of an 8 kHz stream, so it cannot be
;;; represented and must be FILTERED OUT.  Interpolating without filtering instead folds it down
;;; to |8000-6000| = 2000 Hz — a loud tone that was never in the source.  Every other check in
;;; this file passes either way.
(let* ((rs (reed:make-resampler 44100 8000))
       (out (reed:resample rs (tone 6000d0 44100 1.0) :final t))
       (alias (band out 2000d0))
       (passband (band (reed:resample (reed:make-resampler 44100 8000)
                                      (tone 1000d0 44100 1.0) :final t)
                       1000d0)))
  (check-that "content above the new Nyquist is filtered, not folded down as an alias"
              (< alias (* 0.01d0 passband))
              (format nil "2 kHz alias ~,1f vs 1 kHz passband ~,0f (~,1f dB down)"
                      alias passband (* 20 (log (/ (max 1d-6 alias) passband) 10)))))

;;; ---- 4. streaming has no seams --------------------------------------------
;;; A decoder hands over 1152 samples at a time.  If the resampler forgets its position and its
;;; history between calls, every boundary is a discontinuity: an audible tick at 38 Hz.  The
;;; property that rules that out is that the pieces equal the whole, exactly.
(format t "~&~%=== streaming ===~%")
(let* ((in (tone 700d0 44100 0.5))
       (whole (reed:resample (reed:make-resampler 44100 8000) in :final t))
       (rs (reed:make-resampler 44100 8000))
       (parts '()))
  ;; deliberately ragged chunks, none of them a multiple of anything
  (let ((pos 0) (sizes '(1152 37 4096 1 999 1152 8192 13)))
    (loop while (< pos (length in))
          do (let* ((n (min (- (length in) pos) (or (pop sizes) 1152)))
                    (chunk (subseq in pos (+ pos n))))
               (push (reed:resample rs chunk) parts)
               (incf pos n)
               (unless sizes (setf sizes '(1152 37 4096 1 999 1152 8192 13))))))
  (push (reed:resample rs (reed:make-pcm16 0) :final t) parts)
  (let* ((pieces (nreverse parts))
         (total (reduce #'+ pieces :key #'length))
         (joined (reed:make-pcm16 total)) (at 0))
    (dolist (p pieces) (replace joined p :start1 at) (incf at (length p)))
    (check "the pieces are the same length as the whole" (length joined) (length whole))
    (check-that "and sample-identical to it — no seam at any boundary"
                (equalp joined whole)
                (format nil "~d samples across ~d ragged chunks" total (length pieces)))))

;;; ---- 5. the player ---------------------------------------------------------
(format t "~&~%=== the mp3 player ===~%")
(let ((path (corpus "music_cbr128.mp3")))
  (if (not (probe-file path))
      (format t "  SKIP no corpus at ~a~%" path)
      (let* ((p (reed:make-mp3-player path :rate 8000 :frame-samples 160))
             (frames '()))
        (loop for f = (reed:player-next-frame p) while f do (push f frames))
        (let* ((frames (nreverse frames))
               (all (reed:make-pcm16 (reduce #'+ frames :key #'length)))
               (at 0))
          (dolist (f frames) (replace all f :start1 at) (incf at (length f)))
          (check "the file was 44.1 kHz" (reed:player-source-rate p) 44100)
          (check "and stereo" (reed:player-source-channels p) 2)
          (check-that "every frame but the last is exactly 160 samples"
                      (every (lambda (f) (= 160 (length f))) (butlast frames))
                      (format nil "~d frames" (length frames)))
          (check-that "the player reports itself finished" (reed:player-finished-p p))
          (check-that "about 10 seconds of 8 kHz audio came out"
                      (< 78000 (length all) 82000)
                      (format nil "~d samples = ~,2f s" (length all) (/ (length all) 8000.0)))
          (check-that "and it is music, not silence"
                      (> (let ((s 0d0))
                           (dotimes (i (length all) (sqrt (/ s (length all))))
                             (incf s (expt (float (aref all i) 1d0) 2))))
                         200d0)
                      "rms above the noise floor")
          ;; Incremental decoding must not change the answer: pulling the file 20 ms at a time
          ;; through a stateful resampler has to equal decoding it whole and converting once.
          ;; This is the property that says the player is a scheduling change, not a quality one.
          (let* ((whole (reed:decode-mp3-file path))
                 (converted (reed:resample-pcm whole 8000)))
            (check-that "frame-at-a-time equals decode-it-all-then-convert"
                        (equalp all (reed:pcm-samples converted))
                        (format nil "~d vs ~d samples" (length all)
                                (length (reed:pcm-samples converted)))))))))

;;; a mono file exercises the downmix-is-a-no-op path through the same machinery
(let ((path (corpus "music_mono128.mp3")))
  (when (probe-file path)
    (let* ((src (reed:make-mp3-source path :rate 8000 :frame-samples 160))
           (all (reed:drain-source src)))
      (check-that "a mono file plays too" (< 78000 (length all) 82000)
                  (format nil "~d samples" (length all))))))

;;; gain rides on the player, so a mixer can set a source's level without touching its samples
(let ((path (corpus "music_cbr128.mp3")))
  (when (probe-file path)
    (let ((loud (reed:drain-source (reed:make-mp3-source path :frame-samples 160) :limit 1600))
          (quiet (reed:drain-source (reed:make-mp3-source path :frame-samples 160 :gain 0.25d0)
                                    :limit 1600)))
      (check-that "player gain attenuates"
                  (let ((a 0) (b 0))
                    (dotimes (i (min (length loud) (length quiet)))
                      (incf a (abs (aref loud i))) (incf b (abs (aref quiet i))))
                    (and (plusp a) (< (* 0.2 a) (* 4 b) (* 1.05 a))))
                  "0.25 gain is about a quarter the level"))))

;;; ---- 5b. opening in memory, and seeking ------------------------------------
;;;
;;; Scrubbing a long show is why these exist: re-reading a 180 MB file to move the playhead is
;;; not a seek, it is a reload.  So a player can be opened over bytes already in hand, at a byte
;;; offset.  MP3 carries no index — the offset is an estimate and the decoder resyncs to the
;;; next frame — so what is checked here is that the audio LANDS WHERE ASKED, not that it is
;;; sample-exact.  It cannot be: the resampler's phase and the bit reservoir both start over.
(format t "~&~%=== opening in memory, and seeking ===~%")
(let ((path (corpus "music_cbr128.mp3")))
  (if (not (probe-file path))
      (format t "  SKIP no corpus at ~a~%" path)
      (let* ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
                        (read-sequence b s) b)))
             (whole (drain-player (reed:make-mp3-player path :rate 8000 :frame-samples 160)))
             (in-mem (drain-player (reed:make-mp3-player bytes :rate 8000 :frame-samples 160))))
        (check-that "bytes in memory decode exactly like the file they came from"
                    (equalp whole in-mem)
                    (format nil "~d samples" (length in-mem)))
        ;; Halfway through the file is halfway through the audio only because this one is CBR,
        ;; which is why it is the file this check uses.
        (let* ((half (drain-player (reed:make-mp3-player bytes :rate 8000 :frame-samples 160
                                                               :start (floor (length bytes) 2))))
               (expected (floor (length whole) 2)))
          (check-that "seeking to the middle returns about half the audio"
                      (< (* 0.45 (length whole)) (length half) (* 0.55 (length whole)))
                      (format nil "~d of ~d samples" (length half) (length whole)))
          (check-that "and it is music, not the silence a failed resync would leave"
                      (> (rms half) 200d0) (format nil "rms ~,1f" (rms half)))
          ;; Where did it land?  An ignored :START would align at zero and a botched one
          ;; anywhere; only a seek that worked aligns near the midpoint.
          (let ((at (envelope-align half whole)))
            (check-that "and it starts where it was asked to, not at the beginning"
                        (< (abs (- at expected)) 8000)   ; within a second, at 8 kHz
                        (format nil "landed at ~,2f s, asked for ~,2f s"
                                (/ at 8000.0) (/ expected 8000.0))))))))

;;; ---- 5c. a hand-assembled PCM still writes a playable WAV -------------------
(format t "~&~%=== the WAV writer ===~%")
(let* ((n 400)
       (samples (make-array n :element-type '(signed-byte 16))))
  (dotimes (i n) (setf (aref samples i) (round (* 8000 (sin (/ (* 2 pi i) 32))))))
  ;; FRAME-COUNT left at its default is the shape a caller who assembled the samples themselves
  ;; produces.  The header was written from it verbatim, so such a file said zero frames: it
  ;; opened, and played nothing, which reads as a broken decoder rather than a missing argument.
  (let* ((pcm (reed:make-pcm :samples samples :channels 2 :sample-rate 8000 :format :pcm16))
         (wav (reed:pcm->wav-octets pcm))
         (data (+ (aref wav 40) (ash (aref wav 41) 8) (ash (aref wav 42) 16) (ash (aref wav 43) 24))))
    (check "the data chunk holds every sample handed in" data (* n 2))
    (check "and the file is header plus data" (length wav) (+ 44 (* n 2))))
  ;; An explicit FRAME-COUNT still wins — it is how a caller says "only this much of the vector".
  (let* ((pcm (reed:make-pcm :samples samples :channels 2 :sample-rate 8000 :format :pcm16
                             :frame-count 50))
         (wav (reed:pcm->wav-octets pcm))
         (data (+ (aref wav 40) (ash (aref wav 41) 8) (ash (aref wav 42) 16) (ash (aref wav 43) 24))))
    (check "an explicit frame count is still authoritative" data (* 50 2 2))))

;;; ---- 6. a buffer source ----------------------------------------------------
(format t "~&~%=== a buffer source ===~%")
(let* ((v (make-array 350 :element-type '(signed-byte 16) :initial-element 7))
       (s (reed:make-buffer-source v :frame-samples 160))
       (sizes (loop for f = (funcall s) while f collect (length f))))
  (check "slices into frames, short tail last" sizes '(160 160 30))
  (check-that "and then reports end of stream" (null (funcall s))))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
