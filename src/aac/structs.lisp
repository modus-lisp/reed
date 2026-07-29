;;;; src/aac/structs.lisp --- persistent decoder state for AAC channels.
(in-package #:reed)

(defstruct (tns (:constructor make-tns))
  "Temporal Noise Shaping data for one channel (per window / filter)."
  (n-filt    (make-array 8 :element-type 'fixnum :initial-element 0))
  (length    (make-array '(8 4) :element-type 'fixnum :initial-element 0))
  (order     (make-array '(8 4) :element-type 'fixnum :initial-element 0))
  (direction (make-array '(8 4) :element-type 'fixnum :initial-element 0))
  (coef      (make-array '(8 4 20) :element-type 'double-float :initial-element 0d0))
  (present   nil))

(defstruct (aac-chan (:constructor make-aac-chan))
  "One decoded audio channel; overlap buffer persists across frames."
  (coeffs (make-array 1024 :element-type 'double-float :initial-element 0d0))
  (saved  (make-array 1024 :element-type 'double-float :initial-element 0d0))
  (output (make-array 1024 :element-type 'double-float :initial-element 0d0))
  ;; ics_info (refreshed each frame)
  (wseq 0 :type fixnum) (wseq-prev 0 :type fixnum)
  (kbd 0 :type fixnum) (kbd-prev 0 :type fixnum)
  (max-sfb 0 :type fixnum)
  (num-windows 1 :type fixnum)
  (num-groups 1 :type fixnum)
  (group-len (make-array 8 :element-type 'fixnum :initial-element 1))
  (swb nil) (num-swb 0 :type fixnum) (tns-max-bands 0 :type fixnum)
  ;; per (group,sfb) data
  (band-type (make-array 512 :element-type 'fixnum :initial-element 0))
  (sfmult    (make-array 512 :element-type 'double-float :initial-element 0d0))
  (tns (make-tns)))
