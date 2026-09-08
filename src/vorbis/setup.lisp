;;;; src/vorbis/setup.lisp — the three headers, which are the whole of the decoder's configuration.
;;;;
;;;; Vorbis puts NO probability model in the audio packets.  Every codebook, every floor shape, every
;;;; residue partitioning is in the setup header, once, at the front of the stream — which is why a
;;;; Vorbis file cannot be decoded from the middle without it, and why the header is often larger
;;;; than a second of audio.  The audio packets then say only "mode 3", and the mode says which
;;;; mapping, which says which floor and residue, which say which codebooks.
;;;;
;;;; The three headers are identification (what the stream is), comment (metadata, skipped here) and
;;;; setup (everything above).  Each begins with a packet type byte and the string "vorbis".

(in-package #:reed)

(defstruct (vorbis-info (:conc-name vi-))
  (channels 0 :type fixnum)
  (rate 0 :type fixnum)
  (bitrate-nominal 0 :type fixnum)
  (blocksize-0 0 :type fixnum)
  (blocksize-1 0 :type fixnum))

(defstruct (vorbis-mapping (:conc-name vm-))
  (submaps 1 :type fixnum)
  (coupling #() :type simple-vector)    ; vector of (magnitude . angle) channel pairs
  (mux #() :type (simple-array fixnum (*)))
  (submap-floor #() :type (simple-array fixnum (*)))
  (submap-residue #() :type (simple-array fixnum (*))))

(defstruct (vorbis-mode (:conc-name vmo-))
  (blockflag nil)
  (mapping 0 :type fixnum))

(defstruct (vorbis-setup (:conc-name vs-))
  (info nil :type (or null vorbis-info))
  (codebooks #() :type simple-vector)
  (floors #() :type simple-vector)
  (residues #() :type simple-vector)
  (mappings #() :type simple-vector)
  (modes #() :type simple-vector))

(defun %check-header (v type)
  "Every header packet is a type byte then the six characters `vorbis'."
  (let ((got (vb v 8)))
    (unless (= got type)
      (vorbis-error "expected header packet type ~d, found ~d" type got)))
  (let ((magic (map 'string (lambda (i) (declare (ignore i)) (code-char (vb v 8))) '(0 1 2 3 4 5))))
    (unless (string= magic "vorbis")
      (vorbis-error "header packet is not marked `vorbis' but ~s" magic))))

(defun parse-identification (packet)
  "The identification header (§4.2.2): what the stream is, and whether we can decode it at all."
  (let ((v (make-vbits packet)))
    (%check-header v 1)
    (let* ((version (vb v 32))
           (channels (vb v 8))
           (rate (vb v 32))
           (bitrate-max (vb-signed v 32))
           (bitrate-nom (vb-signed v 32))
           (bitrate-min (vb-signed v 32))
           (bs0 (ash 1 (vb v 4)))
           (bs1 (ash 1 (vb v 4)))
           (framing (vb1 v)))
      (declare (ignore bitrate-max bitrate-min))
      (unless (zerop version) (vorbis-error "Vorbis version ~d is not Vorbis I" version))
      (when (zerop channels) (vorbis-error "a stream with no channels"))
      (when (zerop rate) (vorbis-error "a stream with no sample rate"))
      (unless (and (<= 64 bs0 8192) (<= 64 bs1 8192) (<= bs0 bs1))
        (vorbis-error "block sizes ~d and ~d are outside what Vorbis I allows" bs0 bs1))
      (when (zerop framing) (vorbis-error "identification header has no framing bit"))
      (when (vb-eop v) (vorbis-error "identification header is truncated"))
      (make-vorbis-info :channels channels :rate rate :bitrate-nominal bitrate-nom
                        :blocksize-0 bs0 :blocksize-1 bs1))))

(defun parse-setup (packet info)
  "The setup header (§4.2.4): codebooks, floors, residues, mappings and modes, in that order."
  (declare (type vorbis-info info))
  (let ((v (make-vbits packet)))
    (%check-header v 5)
    (let* ((ncodebooks (1+ (vb v 8)))
           (codebooks (make-array ncodebooks)))
      (dotimes (i ncodebooks) (setf (aref codebooks i) (read-codebook v)))
      ;; the time-domain transform list is a placeholder the format never used
      (let ((ntimes (1+ (vb v 6))))
        (dotimes (i ntimes)
          (let ((tt (vb v 16)))
            (unless (zerop tt)
              (vorbis-error "time domain transform ~d is not zero" tt)))))
      (let* ((nfloors (1+ (vb v 6)))
             (floors (make-array nfloors)))
        (dotimes (i nfloors) (setf (aref floors i) (read-floor v ncodebooks)))
        (let* ((nresidues (1+ (vb v 6)))
               (residues (make-array nresidues)))
          (dotimes (i nresidues)
            (setf (aref residues i) (read-residue v ncodebooks codebooks)))
          (let* ((nmappings (1+ (vb v 6)))
                 (mappings (make-array nmappings))
                 (channels (vi-channels info)))
            (dotimes (i nmappings)
              (let ((type (vb v 16)))
                (unless (zerop type)
                  (vorbis-error "mapping type ~d is not defined by Vorbis I" type)))
              (let* ((submaps (if (plusp (vb1 v)) (1+ (vb v 4)) 1))
                     (coupling
                       (if (plusp (vb1 v))
                           (let* ((steps (1+ (vb v 8)))
                                  (out (make-array steps)))
                             (dotimes (j steps out)
                               (let ((mag (vb v (ilog (1- channels))))
                                     (ang (vb v (ilog (1- channels)))))
                                 (when (or (= mag ang) (>= mag channels) (>= ang channels))
                                   (vorbis-error
                                    "coupling step ~d pairs channels ~d and ~d, which is not a pair"
                                    j mag ang))
                                 (setf (aref out j) (cons mag ang)))))
                           #()))
                     (reserved (vb v 2)))
                (unless (zerop reserved)
                  (vorbis-error "mapping reserved field is ~d, not zero" reserved))
                (let ((mux (make-array channels :element-type 'fixnum :initial-element 0)))
                  (when (> submaps 1)
                    (dotimes (j channels)
                      (setf (aref mux j) (vb v 4))
                      (when (> (aref mux j) (1- submaps))
                        (vorbis-error "channel ~d is muxed to submap ~d, past the last one"
                                      j (aref mux j)))))
                  (let ((sf (make-array submaps :element-type 'fixnum :initial-element 0))
                        (sr (make-array submaps :element-type 'fixnum :initial-element 0)))
                    (dotimes (j submaps)
                      (vb v 8)                       ; the unused time configuration placeholder
                      (setf (aref sf j) (vb v 8))
                      (unless (< (aref sf j) nfloors)
                        (vorbis-error "submap ~d names floor ~d, past the last one" j (aref sf j)))
                      (setf (aref sr j) (vb v 8))
                      (unless (< (aref sr j) nresidues)
                        (vorbis-error "submap ~d names residue ~d, past the last one"
                                      j (aref sr j))))
                    (setf (aref mappings i)
                          (make-vorbis-mapping :submaps submaps :coupling coupling :mux mux
                                               :submap-floor sf :submap-residue sr))))))
            (let* ((nmodes (1+ (vb v 6)))
                   (modes (make-array nmodes)))
              (dotimes (i nmodes)
                (let* ((blockflag (plusp (vb1 v)))
                       (windowtype (vb v 16))
                       (transformtype (vb v 16))
                       (mapping (vb v 8)))
                  (unless (and (zerop windowtype) (zerop transformtype))
                    (vorbis-error "mode ~d asks for window type ~d and transform type ~d; Vorbis I has only zero"
                                  i windowtype transformtype))
                  (unless (< mapping nmappings)
                    (vorbis-error "mode ~d names mapping ~d, past the last one" i mapping))
                  (setf (aref modes i)
                        (make-vorbis-mode :blockflag blockflag :mapping mapping))))
              (when (zerop (vb1 v)) (vorbis-error "setup header has no framing bit"))
              (when (vb-eop v) (vorbis-error "setup header is truncated"))
              (make-vorbis-setup :info info :codebooks codebooks :floors floors
                                 :residues residues :mappings mappings :modes modes))))))))
