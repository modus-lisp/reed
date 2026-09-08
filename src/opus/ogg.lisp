;;;; src/opus/ogg.lisp — Ogg (.opus) container demux (RFC 3533 + RFC 7845).
;;;;
;;;; Parse OggS pages (capture pattern, segment/lacing table, packet reassembly
;;;; across page boundaries), read the OpusHead identification header (channel
;;;; count, pre-skip, input sample rate, output gain, channel mapping family),
;;;; skip OpusTags, then feed each audio packet to the Opus decoder.  Pre-skip
;;;; (encoder delay) is trimmed from the start and the OpusHead output gain is
;;;; applied.  Channel mapping family 0 (mono/stereo) is supported; families
;;;; 1/255 (multichannel/surround) are not yet handled (noted, deferred).
(in-package #:reed)

(defun %u16le (v p) (logior (aref v p) (ash (aref v (+ p 1)) 8)))
(defun %u32le (v p) (logior (aref v p) (ash (aref v (+ p 1)) 8)
                            (ash (aref v (+ p 2)) 16) (ash (aref v (+ p 3)) 24)))
(defun %u64le (v p)
  (let ((x 0)) (dotimes (i 8) (setf x (logior x (ash (aref v (+ p i)) (* 8 i))))) x))
(defun %s16le (v p) (let ((x (%u16le v p))) (if (>= x #x8000) (- x #x10000) x)))

(defstruct ogg-page continued bos eos granule serial seqno packets)

(defun %parse-ogg-pages (octets)
  "Walk OCTETS, returning a list of OGG-PAGE in stream order.  Each page's
`packets` is a list of (octet-vector . completep); an incomplete final packet
(lacing value 255 at page end) continues into the next page."
  (let ((pos 0) (n (length octets)) (pages '()))
    (loop while (<= (+ pos 27) n) do
      (unless (and (= (aref octets pos) #x4f) (= (aref octets (+ pos 1)) #x67)
                   (= (aref octets (+ pos 2)) #x67) (= (aref octets (+ pos 3)) #x53))
        (return))                                     ; not an OggS page: stop
      (let* ((htype (aref octets (+ pos 5)))
             (granule (%u64le octets (+ pos 6)))
             (serial (%u32le octets (+ pos 14)))
             (seqno (%u32le octets (+ pos 18)))
             (nsegs (aref octets (+ pos 26)))
             (seg-tbl (+ pos 27))
             (data (+ seg-tbl nsegs))
             (packets '()) (cur '()) (curlen 0))
        ;; A TRUNCATED FILE IS A NORMAL THING TO BE HANDED.  The lacing table says how long the
        ;; page is; if the file ends before that, the page is not there and neither is anything
        ;; after it.  Walking on regardless makes GATHER-SEGMENTS index past the buffer, which
        ;; surfaces as a SUBSEQ bounds error from somewhere three layers down rather than as
        ;; "this file is cut short".
        (when (> (+ data (loop for i below nsegs sum (aref octets (+ seg-tbl i)))) n)
          (return))
        (dotimes (i nsegs)
          (let ((lv (aref octets (+ seg-tbl i))))
            (push (cons data lv) cur) (incf data lv) (incf curlen lv)
            (when (< lv 255)                          ; packet terminates here
              (push (cons (%gather-segments octets (nreverse cur)) t) packets)
              (setf cur '() curlen 0))))
        (when cur                                     ; trailing continued packet
          (push (cons (%gather-segments octets (nreverse cur)) nil) packets))
        (push (make-ogg-page :continued (logtest htype 1) :bos (logtest htype 2)
                             :eos (logtest htype 4) :granule granule :serial serial
                             :seqno seqno :packets (nreverse packets))
              pages)
        (setf pos data)))
    (nreverse pages)))

(defun %gather-segments (octets segs)
  "Concatenate the byte ranges named by SEGS (each (offset . len)) into one vector."
  (let* ((tot (reduce #'+ segs :key #'cdr))
         (out (make-array tot :element-type '(unsigned-byte 8)))
         (o 0))
    (dolist (s segs)
      (replace out octets :start1 o :start2 (car s) :end2 (+ (car s) (cdr s)))
      (incf o (cdr s)))
    out))

(defun %reassemble-opus-packets (pages)
  "Join packet fragments across page boundaries (a page-final continued
fragment + the next page's leading fragment).  Returns (values header-packet
list-of-audio-packets last-granule)."
  (let ((all '()) (pending nil) (serial nil) (last-granule 0))
    (dolist (pg pages)
      ;; lock onto the first logical bitstream we see (family-0 mono/stereo)
      (when (null serial) (setf serial (ogg-page-serial pg)))
      (when (= (ogg-page-serial pg) serial)
        (setf last-granule (ogg-page-granule pg))
        (let ((ps (ogg-page-packets pg)))
          (loop for (pk . completep) in ps for first = t then nil do
            (let ((frag (if (and first pending) (%concat-octets pending pk) pk)))
              (when (and first pending) (setf pending nil))
              (if completep (push frag all) (setf pending frag)))))))
    (let ((all (nreverse all)))
      (values (first all) (cddr all) last-granule))))  ; [0]=OpusHead [1]=OpusTags

(defun %concat-octets (a b)
  (let ((out (make-array (+ (length a) (length b)) :element-type '(unsigned-byte 8))))
    (replace out a) (replace out b :start1 (length a)) out))

(defstruct opus-head (version 1) (channels 2) (pre-skip 0) (input-rate 48000)
  (output-gain 0) (mapping-family 0))

(defun %parse-opus-head (pk)
  (unless (and (>= (length pk) 19)
               (loop for i below 8 for c across "OpusHead" always (= (aref pk i) (char-code c))))
    (%opus-err "not an OpusHead identification header"))
  (make-opus-head :version (aref pk 8) :channels (aref pk 9)
                  :pre-skip (%u16le pk 10) :input-rate (%u32le pk 12)
                  :output-gain (%s16le pk 16) :mapping-family (aref pk 18)))

(defun decode-opus-ogg (octets &key channels)
  "Demux an Ogg-encapsulated Opus stream (OCTETS) and decode it to a 48 kHz
float32 PCM struct, applying OpusHead pre-skip trimming, end-granule trimming
and the output gain (RFC 7845)."
  (multiple-value-bind (head-pk audio last-granule) (%reassemble-opus-packets
                                                     (%parse-ogg-pages octets))
    (let* ((head (%parse-opus-head head-pk))
           (nchan (or channels (max 1 (opus-head-channels head)))))
      (when (> (opus-head-mapping-family head) 0)
        (%opus-err "Ogg Opus channel mapping family ~d (multichannel/surround) not supported"
                   (opus-head-mapping-family head)))
      (let* ((state (make-opus-decoder :channels nchan))
             (parts (mapcar (lambda (p) (decode-opus-packet state p)) audio))
             (total (reduce #'+ parts :key #'pcm-frame-count :initial-value 0))
             (buf (make-array (* nchan total) :element-type 'single-float :initial-element 0f0))
             (off 0))
        (dolist (p parts)
          (let ((s (pcm-samples p))) (replace buf s :start1 off) (incf off (length s))))
        ;; pre-skip (encoder delay) off the front; end-granule trims the tail
        (let* ((pre (opus-head-pre-skip head))
               (want (max 0 (- last-granule pre)))
               (avail (max 0 (- total pre)))
               (nout (min avail want))
               (gain (opus-head-output-gain head)))
          (when (zerop last-granule) (setf nout avail))   ; no granule info: keep all
          (let ((out (make-array (* nchan nout) :element-type 'single-float :initial-element 0f0)))
            (replace out buf :start2 (* nchan pre) :end2 (* nchan (+ pre nout)))
            (unless (zerop gain)
              (let ((g (float (expt 10d0 (/ gain 5120d0)) 1f0)))  ; 10^(gain/256/20)
                (dotimes (i (length out)) (setf (aref out i) (* (aref out i) g)))))
            (make-pcm :samples out :channels nchan :sample-rate 48000
                      :format :float32 :frame-count nout)))))))
