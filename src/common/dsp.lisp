;;;; src/common/dsp.lisp — the arithmetic between a decoder and a device.
;;;;
;;;; Every codec in reed answers the same question: what were the samples?  None of them answer
;;;; the next one, which is what anything actually playing audio has to ask — these samples are
;;;; 44.1 kHz stereo and I need 8 kHz mono, and there are three of them at once and one output.
;;;; Rate conversion, downmixing, gain and summing are not codec work, but they are the work
;;;; that sits directly on top of it, and without them a decoder can convert files and nothing
;;;; else.
;;;;
;;;; Two things here are load-bearing and easy to get wrong:
;;;;
;;;; DECIMATION NEEDS A FILTER.  Dropping or interpolating samples to get from 44100 to 8000 Hz
;;;; folds everything above 4 kHz back down into the audible band — cymbals become a warble, and
;;;; the result is not "lower quality", it is wrong in a way that gets worse the better the
;;;; source was.  RESAMPLE convolves with a windowed sinc whose cutoff follows the lower of the
;;;; two rates, which is the filter and the interpolation in one pass.
;;;;
;;;; STREAMING MUST BE STATEFUL.  A decoder hands over its output a granule at a time.  Resample
;;;; each granule as if it were a whole file and every boundary gets a discontinuity — an audible
;;;; tick every 26 ms, which is a buzz at 38 Hz.  RESAMPLER keeps the input history its kernel
;;;; reaches back into and a fractional read position that carries across calls, so a stream
;;;; resampled in pieces is sample-identical to the same stream resampled whole (dsp gate).

(in-package #:reed)

;;; ---- sample plumbing -------------------------------------------------------
(deftype pcm16-vector () '(simple-array (signed-byte 16) (*)))

(defun make-pcm16 (n) (make-array n :element-type '(signed-byte 16) :initial-element 0))

(declaim (inline clamp16))
(defun clamp16 (x)
  "Saturate to the 16-bit range.  Summed audio WILL exceed it; wrapping turns a loud passage
into a burst of noise, so clipping is the merciful failure and the one everything expects."
  (cond ((> x 32767) 32767) ((< x -32768) -32768) (t (round x))))

(defun downmix (samples channels)
  "Interleaved CHANNELS-wide SAMPLES -> mono, by averaging.  Averaging rather than summing:
two channels carrying the same content should not come out twice as loud, which is what a
listener means by 'the same audio in mono'."
  (if (= channels 1)
      samples
      (let* ((frames (floor (length samples) channels))
             (out (make-pcm16 frames)))
        (dotimes (i frames out)
          (let ((sum 0))
            (dotimes (c channels) (incf sum (aref samples (+ (* i channels) c))))
            (setf (aref out i) (clamp16 (/ sum channels))))))))

(defun apply-gain (samples gain &optional (out samples))
  "Scale SAMPLES by GAIN into OUT (in place by default), clipping."
  (dotimes (i (length samples) out)
    (setf (aref out i) (clamp16 (* gain (aref samples i)))))
  out)

(defun mix (buffers &key gains (length nil))
  "Sum mono BUFFERS into one, with an optional per-buffer GAIN, clipping the result.

The mixer proper.  Buffers may differ in length — a source that ended mid-frame contributes what
it has and silence after, rather than truncating everyone else to match it."
  (let* ((n (or length (reduce #'max buffers :key #'length :initial-value 0)))
         ;; The accumulator is WIDER than the output on purpose.  Summing into a 16-bit buffer
         ;; overflows on the way in, which makes "clip once at the end" unreachable — the damage
         ;; is already done by the second source.  This is the whole reason the two are separate.
         (acc (make-array n :element-type 'fixnum :initial-element 0))
         (out (make-pcm16 n)))
    (loop for b in buffers
          for g = (if gains (pop gains) 1.0d0)
          do (dotimes (i (min n (length b)))
               (incf (aref acc i) (round (* g (aref b i))))))
    ;; clip once at the end, not per-source: two sources that individually peak and cancel each
    ;; other should come out quiet, not clipped twice and then cancelled.
    (dotimes (i n out) (setf (aref out i) (clamp16 (aref acc i))))))

;;; ---- rate conversion -------------------------------------------------------
;;;
;;; A windowed-sinc (Blackman) kernel, tabulated once and read with linear interpolation.  The
;;; direct form calls SIN a few hundred times per output sample; the table makes it a lookup, at
;;; a resolution fine enough that the interpolation error is far below the 16-bit floor.

(defconstant +kernel-zeros+ 12
  "Zero crossings of the sinc on each side.  More is a sharper filter and more taps; 12 puts the
transition band comfortably inside what an 8 kHz channel throws away anyway.")

(defconstant +kernel-density+ 64 "Table samples per unit of the sinc argument.")

(defstruct (resampler (:constructor %make-resampler))
  (in-rate 44100 :type fixnum)
  (out-rate 8000 :type fixnum)
  (step 1d0 :type double-float)        ; input samples advanced per output sample
  (cutoff 1d0 :type double-float)      ; filter cutoff, as a fraction of the input Nyquist
  (half 12 :type fixnum)               ; kernel half-width, in input samples
  (kernel #() :type simple-vector)
  (buf (make-pcm16 0))                 ; input history + pending input
  (fill 0 :type fixnum)                ; valid samples in BUF
  (base 0 :type fixnum)                ; stream index of BUF[0]
  (pos 0d0 :type double-float)         ; absolute fractional read position, in input samples
  (drained nil))

(defun %build-kernel (cutoff half)
  (let* ((n (1+ (* half +kernel-density+)))
         (k (make-array n)))
    (dotimes (i n k)
      (let* ((x (/ (float i 1d0) +kernel-density+))       ; distance in input samples
             (a (* pi cutoff x))
             (sinc (if (zerop i) 1d0 (/ (sin a) a)))
             ;; Blackman, evaluated over [0,1] of the half-width
             (u (/ x half))
             (w (if (> u 1d0) 0d0
                    (+ 0.42d0 (* 0.5d0 (cos (* pi u))) (* 0.08d0 (cos (* 2 pi u)))))))
        (setf (aref k i) (* cutoff sinc w))))))

(defun make-resampler (in-rate out-rate)
  "A stateful rate converter from IN-RATE to OUT-RATE.  Feed it with RESAMPLE; the state it keeps
is what makes a stream converted in pieces identical to the same stream converted whole."
  (let* ((cutoff (min 1d0 (/ (float out-rate 1d0) in-rate)))
         ;; Downsampling widens the kernel in input samples by exactly the factor it narrows the
         ;; cutoff by — the same number of zero crossings, spread over more input.
         (half (max 4 (ceiling (/ +kernel-zeros+ cutoff)))))
    (%make-resampler :in-rate in-rate :out-rate out-rate
                     :step (/ (float in-rate 1d0) out-rate)
                     :cutoff cutoff :half half
                     :kernel (%build-kernel cutoff half)
                     :buf (make-pcm16 (* 4 half))
                     :pos (float half 1d0))))   ; start where the kernel is fully inside the buffer

(declaim (inline %tap))
(defun %tap (kernel half d)
  "Kernel weight at distance D (in input samples), by table lookup + lerp."
  (let ((x (* (abs d) +kernel-density+)))
    (if (>= x (* half +kernel-density+))
        0d0
        (let* ((i (floor x)) (f (- x i)))
          (+ (* (- 1d0 f) (the double-float (aref kernel i)))
             (* f (the double-float (aref kernel (1+ i)))))))))

(defun %ensure-room (rs n)
  (let ((buf (resampler-buf rs)))
    (when (< (length buf) (+ (resampler-fill rs) n))
      (let ((new (make-pcm16 (max (* 2 (length buf)) (+ (resampler-fill rs) n)))))
        (replace new buf :end2 (resampler-fill rs))
        (setf (resampler-buf rs) new)))))

(defun %discard-consumed (rs)
  "Drop input the kernel can no longer reach — everything before POS - HALF."
  (let* ((keep-from (max 0 (- (floor (resampler-pos rs)) (resampler-half rs) 1)))
         (drop (- keep-from (resampler-base rs))))
    (when (plusp drop)
      (let ((buf (resampler-buf rs)))
        (replace buf buf :start2 drop :end2 (resampler-fill rs))
        (decf (resampler-fill rs) drop)
        (incf (resampler-base rs) drop)))))

(defun resample (rs samples &key final)
  "Feed mono SAMPLES to RS and return every output sample now producible (a fresh vector).

FINAL t flushes the tail: without it the last half-kernel of input is held back, because a
correct output sample needs input on BOTH sides of it and the rest has not arrived yet.  A
player calls this with FINAL t once the decoder is done, and gets the last few milliseconds."
  (%ensure-room rs (length samples))
  (replace (resampler-buf rs) samples :start1 (resampler-fill rs))
  (incf (resampler-fill rs) (length samples))
  (let* ((buf (resampler-buf rs))
         (base (resampler-base rs))
         (half (resampler-half rs))
         (kernel (resampler-kernel rs))
         (step (resampler-step rs))
         (end (+ base (resampler-fill rs)))                  ; stream index one past the last input
         ;; the last position whose kernel is fully covered by input we hold
         (limit (if final (float end 1d0) (- end half)))
         (out '())
         (n 0))
    (loop while (< (resampler-pos rs) limit)
          do (let* ((p (resampler-pos rs))
                    (lo (max base (ceiling (- p half))))
                    (hi (min (1- end) (floor (+ p half))))
                    (acc 0d0))
               (loop for i from lo to hi
                     do (incf acc (* (aref buf (- i base)) (%tap kernel half (- i p)))))
               (push (clamp16 acc) out)
               (incf n)
               (incf (resampler-pos rs) step)))
    (%discard-consumed rs)
    (let ((v (make-pcm16 n)))
      (loop for s in out for i downfrom (1- n) do (setf (aref v i) s))
      v)))

(defun resample-pcm (pcm rate &key (channels 1))
  "One-shot: convert PCM to RATE and CHANNELS (mono by default).  Convenience for whole buffers;
streaming callers want MAKE-RESAMPLER + RESAMPLE so the pieces join without a click."
  (let* ((mono (if (= channels 1) (downmix (pcm-samples pcm) (pcm-channels pcm)) (pcm-samples pcm)))
         (rs (make-resampler (pcm-sample-rate pcm) rate))
         (body (resample rs mono))
         (tail (resample rs (make-pcm16 0) :final t))
         (all (make-pcm16 (+ (length body) (length tail)))))
    (replace all body)
    (replace all tail :start1 (length body))
    (make-pcm :samples all :channels channels :sample-rate rate
              :format :pcm16 :frame-count (floor (length all) channels))))
