;;;; threads.lisp — decoding on more than one thread.
;;;;
;;;; Some decoders build their tables on first use and keep them in globals: the Opus MDCT's FFT
;;;; twiddles and trig tables, Vorbis's IMDCT tables and FFT plans, AAC's Huffman decoders, x^(4/3)
;;;; table, windows and IMDCT matrices.  On a Lisp whose threads share memory that is fine.  On
;;;; modus they share NOTHING: a thread's own objects may not be stored into a global, so decoding
;;;; on any thread but the one that loaded reed is refused at the first table it builds.
;;;; WITH-THREAD-STATE gives the body its own set -- every one of those specials bound afresh, which
;;;; on modus is storage of the thread's own -- so a decode worker can.  They are built lazily, as
;;;; before, so a thread pays only for the codecs it uses.

(in-package #:reed)

(defmacro with-thread-state (() &body body)
  "BODY with reed's lazily built decoder tables private to this thread.  Costs one rebuild of
   whatever tables the thread's codecs use; a decode on the loading thread does not need it."
  `(let ((*mdct-twiddle-cache* (make-hash-table))
         (*mdct-trig-cache* (make-hash-table))
         (*vorbis-imdct-tables* (make-hash-table))
         (*fft-plans* (make-hash-table))
         (*imdct-plans* (make-hash-table))
         (*aac-spec-decoders* nil) (*aac-sf-decoder* nil) (*aac-pow43* nil)
         (*aac-windows-ready* nil)
         (*win-sine-long* nil) (*win-kbd-long* nil) (*win-sine-short* nil) (*win-kbd-short* nil)
         (*imdct-long* nil) (*imdct-short* nil))
     ,@body))

(export 'with-thread-state)
