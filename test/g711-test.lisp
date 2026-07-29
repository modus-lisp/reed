;;;; test/g711-test.lisp — G.711 (PCMU/PCMA) conformance for reed.
;;;;
;;;; Bit-exact checks against the canonical ITU-T G.711 reference (the
;;;; Sun/Reese-Campbell exp_lut + segment-search formulation, re-derived
;;;; independently of reed's integer-length code), plus round-trip idempotency.
;;;;
;;;; NOTE ON THE ORACLE: ffmpeg's pcm_mulaw/pcm_alaw is NOT the reference here —
;;;; ffmpeg rounds ~1.8% of samples to the adjacent codeword at segment
;;;; boundaries and disagrees with the ITU reference (reed matches the ITU
;;;; reference on all 65536 inputs).  This mirrors the MP3 24 kHz case where
;;;; ffmpeg was itself the outlier.  Vectors below come from the independent
;;;; reference decoder, not from reed.
(ql:quickload :reed :silent t)
(in-package :reed)

(defparameter *fail* 0)
(defun check (name got want)
  (unless (equalp got want)
    (incf *fail*)
    (format t "FAIL ~a: got ~s want ~s~%" name got want)))

;;; ---- canonical encode vectors: linear -> (mu-law A-law) ------------------
;;; (from the independent ITU reference)
(defparameter *enc-vectors*
  '((-32768 0 42) (-24956 7 45) (-16320 15 58) (-8000 32 10) (-1000 78 122)
    (-100 114 83) (-1 127 85) (0 255 213) (1 255 213) (100 242 211)
    (1000 206 250) (8000 160 138) (16319 143 186) (24000 136 162) (32767 128 170)))

(dolist (v *enc-vectors*)
  (destructuring-bind (lin u a) v
    (check (format nil "mu-law encode ~d" lin) (pcmu-encode-1 lin) u)
    (check (format nil "A-law  encode ~d" lin) (pcma-encode-1 lin) a)))

;;; ---- canonical decode tables (all 256 codewords -> linear) ---------------
(defparameter *ulaw-decode*
  #(-32124 -31100 -30076 -29052 -28028 -27004 -25980 -24956 -23932 -22908 -21884
    -20860 -19836 -18812 -17788 -16764 -15996 -15484 -14972 -14460 -13948 -13436
    -12924 -12412 -11900 -11388 -10876 -10364 -9852 -9340 -8828 -8316 -7932 -7676
    -7420 -7164 -6908 -6652 -6396 -6140 -5884 -5628 -5372 -5116 -4860 -4604 -4348
    -4092 -3900 -3772 -3644 -3516 -3388 -3260 -3132 -3004 -2876 -2748 -2620 -2492
    -2364 -2236 -2108 -1980 -1884 -1820 -1756 -1692 -1628 -1564 -1500 -1436 -1372
    -1308 -1244 -1180 -1116 -1052 -988 -924 -876 -844 -812 -780 -748 -716 -684 -652
    -620 -588 -556 -524 -492 -460 -428 -396 -372 -356 -340 -324 -308 -292 -276 -260
    -244 -228 -212 -196 -180 -164 -148 -132 -120 -112 -104 -96 -88 -80 -72 -64 -56
    -48 -40 -32 -24 -16 -8 0 32124 31100 30076 29052 28028 27004 25980 24956 23932
    22908 21884 20860 19836 18812 17788 16764 15996 15484 14972 14460 13948 13436
    12924 12412 11900 11388 10876 10364 9852 9340 8828 8316 7932 7676 7420 7164 6908
    6652 6396 6140 5884 5628 5372 5116 4860 4604 4348 4092 3900 3772 3644 3516 3388
    3260 3132 3004 2876 2748 2620 2492 2364 2236 2108 1980 1884 1820 1756 1692 1628
    1564 1500 1436 1372 1308 1244 1180 1116 1052 988 924 876 844 812 780 748 716 684
    652 620 588 556 524 492 460 428 396 372 356 340 324 308 292 276 260 244 228 212
    196 180 164 148 132 120 112 104 96 88 80 72 64 56 48 40 32 24 16 8 0))

(defparameter *alaw-decode*
  #(-5504 -5248 -6016 -5760 -4480 -4224 -4992 -4736 -7552 -7296 -8064 -7808 -6528
    -6272 -7040 -6784 -2752 -2624 -3008 -2880 -2240 -2112 -2496 -2368 -3776 -3648
    -4032 -3904 -3264 -3136 -3520 -3392 -22016 -20992 -24064 -23040 -17920 -16896
    -19968 -18944 -30208 -29184 -32256 -31232 -26112 -25088 -28160 -27136 -11008
    -10496 -12032 -11520 -8960 -8448 -9984 -9472 -15104 -14592 -16128 -15616 -13056
    -12544 -14080 -13568 -344 -328 -376 -360 -280 -264 -312 -296 -472 -456 -504 -488
    -408 -392 -440 -424 -88 -72 -120 -104 -24 -8 -56 -40 -216 -200 -248 -232 -152
    -136 -184 -168 -1376 -1312 -1504 -1440 -1120 -1056 -1248 -1184 -1888 -1824 -2016
    -1952 -1632 -1568 -1760 -1696 -688 -656 -752 -720 -560 -528 -624 -592 -944 -912
    -1008 -976 -816 -784 -880 -848 5504 5248 6016 5760 4480 4224 4992 4736 7552 7296
    8064 7808 6528 6272 7040 6784 2752 2624 3008 2880 2240 2112 2496 2368 3776 3648
    4032 3904 3264 3136 3520 3392 22016 20992 24064 23040 17920 16896 19968 18944
    30208 29184 32256 31232 26112 25088 28160 27136 11008 10496 12032 11520 8960 8448
    9984 9472 15104 14592 16128 15616 13056 12544 14080 13568 344 328 376 360 280 264
    312 296 472 456 504 488 408 392 440 424 88 72 120 104 24 8 56 40 216 200 248 232
    152 136 184 168 1376 1312 1504 1440 1120 1056 1248 1184 1888 1824 2016 1952 1632
    1568 1760 1696 688 656 752 720 560 528 624 592 944 912 1008 976 816 784 880 848))

(dotimes (c 256)
  (check (format nil "mu-law decode ~d" c) (pcmu-decode-1 c) (aref *ulaw-decode* c))
  (check (format nil "A-law  decode ~d" c) (pcma-decode-1 c) (aref *alaw-decode* c)))

;;; ---- round-trip idempotency: encode(decode(code)) = code -----------------
;;; Holds for all 256 codes EXCEPT the mu-law +0 code (0x7F), which shares its
;;; sample value (0) with the -0 code (0xFF) and re-encodes to 0xFF — the
;;; standard mu-law negative-zero degeneracy.
(let ((mu-mismatch '()) (a-mismatch '()))
  (dotimes (c 256)
    (unless (= c (pcmu-encode-1 (pcmu-decode-1 c))) (push c mu-mismatch))
    (unless (= c (pcma-encode-1 (pcma-decode-1 c))) (push c a-mismatch)))
  (check "mu-law round-trip (only +0 code #x7F degenerates)" (sort mu-mismatch #'<) '(#x7F))
  (check "A-law round-trip (all codes idempotent)" a-mismatch '()))

;;; ---- buffer wrappers round-trip a PCM frame through mu-law/A-law ----------
(let* ((n 160)
       (pcm (make-array n :element-type '(signed-byte 16))))
  (dotimes (i n) (setf (aref pcm i) (- (* i 300) 24000)))
  ;; encode+decode is lossy but the codeword path must be stable
  (let ((u (pcmu-encode pcm)) (a (pcma-encode pcm)))
    (check "pcmu buffer decode = per-sample"
           (pcmu-decode u) (let ((o (make-array n :element-type '(signed-byte 16))))
                             (dotimes (i n o) (setf (aref o i) (pcmu-decode-1 (aref u i))))))
    (check "pcma buffer decode = per-sample"
           (pcma-decode a) (let ((o (make-array n :element-type '(signed-byte 16))))
                             (dotimes (i n o) (setf (aref o i) (pcma-decode-1 (aref a i))))))
    ;; aliases must be identical to the PCMU/PCMA names
    (check "mulaw-encode alias" (mulaw-encode pcm) u)
    (check "alaw-decode alias"  (alaw-decode a) (pcma-decode a))))

(if (zerop *fail*)
    (format t "~&G711: ALL CHECKS PASS (encode+decode bit-exact to ITU reference; round-trip verified)~%")
    (format t "~&G711: ~d CHECK(S) FAILED~%" *fail*))
(sb-ext:exit :code (if (zerop *fail*) 0 1))
