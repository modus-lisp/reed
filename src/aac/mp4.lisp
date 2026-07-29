;;;; src/aac/mp4.lisp --- a minimal ISO base media file format (MP4/M4A)
;;;; demuxer, just enough to pull AAC-LC access units and their
;;;; AudioSpecificConfig and feed the shared decoder core.
(in-package #:reed)

(declaim (inline %be32 %be64))
(defun %be32 (b p) (logior (ash (aref b p) 24) (ash (aref b (+ p 1)) 16)
                           (ash (aref b (+ p 2)) 8) (aref b (+ p 3))))
(defun %be64 (b p) (logior (ash (%be32 b p) 32) (%be32 b (+ p 4))))

(defun aac-mp4-p (bytes)
  "Heuristic: an ISO-BMFF file has an 'ftyp' box at the start."
  (and (>= (length bytes) 12)
       (= (aref bytes 4) (char-code #\f)) (= (aref bytes 5) (char-code #\t))
       (= (aref bytes 6) (char-code #\y)) (= (aref bytes 7) (char-code #\p))))

(defun %box-name= (bytes p s)
  (and (< (+ p 3) (length bytes))
       (loop for i below 4 always (= (aref bytes (+ p i)) (char-code (char s i))))))

(defun %find-box (bytes start end name)
  "Return (values content-start content-end) of the first child box NAME within
[START,END), or NIL.  Handles 32- and 64-bit box sizes."
  (let ((p start))
    (loop while (< (+ p 8) end) do
      (let* ((size (%be32 bytes p)) (hdr 8) (bend nil))
        (cond ((= size 1) (setf size (%be64 bytes (+ p 8)) hdr 16))
              ((= size 0) (setf size (- end p))))
        (setf bend (+ p size))
        (when (%box-name= bytes (+ p 4) name)
          (return-from %find-box (values (+ p hdr) (min bend end))))
        (when (<= size 0) (return))
        (setf p bend)))
    nil))

(defun %find-path (bytes start end names)
  "Descend a path of nested box NAMES; return (values cs ce) of the last."
  (let ((s start) (e end))
    (dolist (nm names (values s e))
      (multiple-value-bind (cs ce) (%find-box bytes s e nm)
        (unless cs (return-from %find-path nil))
        (setf s cs e ce)))))

;;; ---- AudioSpecificConfig (from the esds DecoderSpecificInfo) -------------

(defun aac-parse-asc (bytes p end)
  "Parse an AudioSpecificConfig at bit offset 0 of BYTES[P,END).  Returns
(values aot sr-index channel-config)."
  (let ((br (make-bitreader bytes :start p :end end)))
    (let* ((aot (read-bits br 5))
           (sri (read-bits br 4)))
      (when (= aot 31) (setf aot (+ 32 (read-bits br 6))))
      (when (= sri 15) (read-bits br 24))     ; explicit rate (rare); index stays 15
      (values aot sri (read-bits br 4)))))

(defun %find-esds-asc (bytes start end)
  "Locate the esds box under stsd/mp4a and return (values aot sri chan)."
  (multiple-value-bind (ds de) (%find-path bytes start end '("stsd"))
    (unless ds (return-from %find-esds-asc nil))
    ;; stsd: 4 version/flags + 4 entry-count, then a sample entry box (mp4a)
    (multiple-value-bind (ms me) (%find-box bytes (+ ds 8) de "mp4a")
      (unless ms (return-from %find-esds-asc nil))
      ;; mp4a is an AudioSampleEntry: 8 reserved +2 dref +2*3 version/etc +
      ;; 2 channelcount +2 samplesize +2*2 +4 samplerate = 28 bytes, then esds
      (multiple-value-bind (es ee) (%find-box bytes (+ ms 28) me "esds")
        (unless es (return-from %find-esds-asc nil))
        ;; esds: 4 version/flags, then an ES_Descriptor.  Walk the MPEG-4
        ;; descriptor tags to the DecoderSpecificInfo (tag 0x05).
        (let ((q (+ es 4)))
          (labels ((desc-len () (let ((v 0))
                                  (loop for i below 4
                                        for b = (aref bytes q)
                                        do (incf q) (setf v (logior (ash v 7) (logand b #x7f)))
                                        while (logtest b #x80))
                                  v)))
            ;; ES_Descriptor (0x03)
            (unless (= (aref bytes q) 3) (return-from %find-esds-asc nil))
            (incf q) (desc-len) (incf q 3)      ; ES_ID(2) + flags(1)
            ;; DecoderConfigDescriptor (0x04)
            (unless (= (aref bytes q) 4) (return-from %find-esds-asc nil))
            (incf q) (desc-len) (incf q 13)     ; objtype+stream+bufsize+bitrates
            ;; DecoderSpecificInfo (0x05) = the AudioSpecificConfig
            (unless (= (aref bytes q) 5) (return-from %find-esds-asc nil))
            (incf q)
            (let ((n (desc-len)))
              (aac-parse-asc bytes q (+ q n)))))))))

;;; ---- sample table (stsz / stsc / stco|co64) -----------------------------

(defun %read-u32-table (bytes cs)
  "Read a FullBox u32-array table (version/flags + count + entries)."
  (let* ((count (%be32 bytes (+ cs 4)))
         (v (make-array count :element-type '(unsigned-byte 32))))
    (dotimes (i count v) (setf (aref v i) (%be32 bytes (+ cs 8 (* i 4)))))))

(defun aac-mp4-access-units (bytes start end)
  "Extract the ordered list of AAC access units (byte vectors) from a trak's
stbl at [START,END) using stsz + stsc + stco/co64."
  (multiple-value-bind (zs ze) (%find-box bytes start end "stsz")
    (declare (ignore ze))
    (multiple-value-bind (cs ce) (%find-box bytes start end "stsc")
      (declare (ignore ce))
      (let* ((co64 nil)
             (offs (multiple-value-bind (os oe) (%find-box bytes start end "stco")
                     (declare (ignore oe))
                     (if os (%read-u32-table bytes os)
                         (multiple-value-bind (o8 o8e) (%find-box bytes start end "co64")
                           (declare (ignore o8e))
                           (setf co64 t)
                           (let* ((cnt (%be32 bytes (+ o8 4)))
                                  (v (make-array cnt :element-type '(unsigned-byte 64))))
                             (dotimes (i cnt v) (setf (aref v i) (%be64 bytes (+ o8 8 (* i 8)))))))))))
        (declare (ignore co64))
        (unless (and zs cs offs) (return-from aac-mp4-access-units nil))
        ;; stsz: version/flags, sample_size (if !=0 uniform), sample_count
        (let* ((uniform (%be32 bytes (+ zs 4)))
               (nsamp (%be32 bytes (+ zs 8)))
               (sizes (make-array nsamp :element-type '(unsigned-byte 32))))
          (if (/= uniform 0)
              (dotimes (i nsamp) (setf (aref sizes i) uniform))
              (dotimes (i nsamp) (setf (aref sizes i) (%be32 bytes (+ zs 12 (* i 4))))))
          ;; stsc: entries of (first_chunk, samples_per_chunk, sample_desc_idx)
          (let* ((nsc (%be32 bytes (+ cs 4)))
                 (nchunks (length offs))
                 (aus '())
                 (sample 0))
            ;; expand stsc into per-chunk samples_per_chunk
            (dotimes (chunk nchunks)
              (let ((spc 1))
                ;; find the stsc entry governing this chunk (1-based first_chunk)
                (dotimes (e nsc)
                  (let ((first (%be32 bytes (+ cs 8 (* e 12)))))
                    (when (<= first (1+ chunk))
                      (setf spc (%be32 bytes (+ cs 8 (* e 12) 4))))))
                (let ((off (aref offs chunk)))
                  (dotimes (k spc)
                    (when (< sample nsamp)
                      (let* ((sz (aref sizes sample))
                             (au (make-array sz :element-type '(unsigned-byte 8))))
                        (replace au bytes :start2 off :end2 (+ off sz))
                        (push au aus)
                        (incf off sz) (incf sample)))))))
            (nreverse aus)))))))

(defun decode-m4a (bytes &key (format :pcm16))
  "Decode an MP4/M4A file (ISO-BMFF) carrying AAC-LC into a PCM struct."
  (let ((bytes (coerce bytes '(simple-array (unsigned-byte 8) (*))))
        (len (length bytes)))
    (multiple-value-bind (ss se) (%find-path bytes 0 len '("moov" "trak" "mdia" "minf" "stbl"))
      (unless ss (error 'aac-error :message "no stbl box (not an AAC MP4?)"))
      (multiple-value-bind (aot sri chan) (%find-esds-asc bytes ss se)
        (unless aot (error 'aac-error :message "no esds AudioSpecificConfig"))
        (unless (or (= aot 2) (= aot 1) (= aot 3) (= aot 4))
          (error 'aac-error :message (format nil "unsupported AAC object type ~a (LC only)" aot)))
        (let ((aus (aac-mp4-access-units bytes ss se)))
          (unless aus (error 'aac-error :message "no access units in stbl"))
          (aac-decode-access-units aus sri (aac-config-channels chan)
                                   (aref +aac-sample-rates+ sri) :format format))))))
