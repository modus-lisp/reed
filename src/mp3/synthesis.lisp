;;;; src/synthesis.lisp — 32-band polyphase synthesis filterbank.
(in-package #:reed)

(declaim (type (simple-array double-float (64 32)) +synth-nwin+))
(defparameter +synth-nwin+
  (let ((a (make-array '(64 32) :element-type 'double-float)))
    (dotimes (i 64 a)
      (dotimes (j 32)
        (setf (aref a i j)
              (cos (* (coerce (* (+ 16 i) (+ (* 2 j) 1)) 'double-float)
                      (/ +pi+ 64.0d0))))))))

(defun subband-synthesis (is vvec ch out)
  "Polyphase synthesis of channel CH: transform the 576 subband lines IS into
576 time-domain samples in OUT, updating the persistent FIFO VVEC[ch][1024]."
  (declare (type samples is out) (type (simple-array double-float (2 1024)) vvec)
           (type fixnum ch) (optimize (speed 3) (safety 1)))
  (let ((s-vec (make-array 32 :element-type 'double-float))
        (u-vec (make-array 512 :element-type 'double-float))
        (nwin +synth-nwin+)
        (dtbl +synth-window+))
    (declare (dynamic-extent s-vec u-vec)
             (type (simple-array double-float (512)) dtbl))
    (dotimes (ss 18)
      ;; shift the V FIFO up by 64
      (loop for i of-type fixnum from 1023 downto 64
            do (setf (aref vvec ch i) (aref vvec ch (- i 64))))
      ;; next 32 subband samples
      (dotimes (i 32) (setf (aref s-vec i) (aref is (+ (* i 18) ss))))
      ;; matrix the 32 inputs into the 64 new V entries
      (dotimes (i 64)
        (let ((sum 0.0d0))
          (declare (type double-float sum))
          (dotimes (j 32) (incf sum (* (aref nwin i j) (aref s-vec j))))
          (setf (aref vvec ch i) sum)))
      ;; build the 512-entry U vector
      (dotimes (i 8)
        (dotimes (j 32)
          (setf (aref u-vec (+ (ash i 6) j)) (aref vvec ch (+ (ash i 7) j))
                (aref u-vec (+ (ash i 6) j 32)) (aref vvec ch (+ (ash i 7) j 96)))))
      ;; window
      (dotimes (i 512) (setf (aref u-vec i) (* (aref u-vec i) (aref dtbl i))))
      ;; 32 output samples for this sub-block
      (dotimes (i 32)
        (let ((sum 0.0d0))
          (declare (type double-float sum))
          (dotimes (j 16) (incf sum (aref u-vec (+ (ash j 5) i))))
          (setf (aref out (+ (* 32 ss) i)) sum))))))
