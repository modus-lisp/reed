;;;; src/opus/tables.lisp — CELT static tables (RFC 6716 / libopus mode 48000,960).
;;;; The mode-derived arrays (eBands, logN, allocVectors, pulse cache) are
;;;; auto-extracted from libopus by opus-build/dumptables.c; the small model
;;;; tables are transcribed from celt/quant_bands.c, celt/celt.c, celt/rate.c.
(in-package #:reed)

(defparameter +celt-nbEBands+ 21)
(defparameter +celt-effEBands+ 21)
;; +celt-overlap+ (= 120) is defined as a constant in mdct.lisp (compiled next).
(defparameter +celt-shortMdctSize+ 120)
(defparameter +celt-maxLM+ 3)
(defparameter +celt-nbAllocVectors+ 11)
(defparameter +celt-mdct-n+ 1920)
(defparameter +celt-ebands+ (make-array 23 :element-type '(signed-byte 16) :initial-contents '(0 1 2 3 4 5 6 7 8 10 12 14 16 20 24 28 34 40 48 60 78 100 6913 )))
(defparameter +celt-logn+ (make-array 21 :element-type '(signed-byte 16) :initial-contents '(0 0 0 0 0 0 0 0 8 8 8 8 16 16 16 21 21 24 29 34 36 )))
(defparameter +celt-alloc-vectors+ (make-array 231 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 90 80 75 69 63 56 49 40 34 29 20 18 10 0 0 0 0 0 0 0 0 110 100 90 84 78 71 65 58 51 45 39 32 26 20 12 0 0 0 0 0 0 118 110 103 93 86 80 75 70 65 59 53 47 40 31 23 15 4 0 0 0 0 126 119 112 104 95 89 83 78 72 66 60 54 47 39 32 25 17 12 1 0 0 134 127 120 114 103 97 91 85 78 72 66 60 54 47 41 35 29 23 16 10 1 144 137 130 124 113 107 101 95 88 82 76 70 64 57 51 45 39 33 26 15 1 152 145 138 132 123 117 111 105 98 92 86 80 74 67 61 55 49 43 36 20 1 162 155 148 142 133 127 121 115 108 102 96 90 84 77 71 65 59 53 46 30 1 172 165 158 152 143 137 131 125 118 112 106 100 94 87 81 75 69 63 56 45 20 200 200 200 200 200 200 200 200 198 193 188 183 178 173 168 163 158 153 148 129 104 )))
(defparameter +celt-cache-index+ (make-array 105 :element-type '(signed-byte 16) :initial-contents '(-1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 41 41 41 82 82 123 164 200 222 0 0 0 0 0 0 0 0 41 41 41 41 123 123 123 164 164 240 266 283 295 41 41 41 41 41 41 41 41 123 123 123 123 240 240 240 266 266 305 318 328 336 123 123 123 123 123 123 123 123 240 240 240 240 305 305 305 318 318 343 351 358 364 240 240 240 240 240 240 240 240 305 305 305 305 343 343 343 351 351 370 376 382 387 )))
(defparameter +celt-cache-bits+ (make-array 392 :element-type '(unsigned-byte 8) :initial-contents '(40 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 40 15 23 28 31 34 36 38 39 41 42 43 44 45 46 47 47 49 50 51 52 53 54 55 55 57 58 59 60 61 62 63 63 65 66 67 68 69 70 71 71 40 20 33 41 48 53 57 61 64 66 69 71 73 75 76 78 80 82 85 87 89 91 92 94 96 98 101 103 105 107 108 110 112 114 117 119 121 123 124 126 128 40 23 39 51 60 67 73 79 83 87 91 94 97 100 102 105 107 111 115 118 121 124 126 129 131 135 139 142 145 148 150 153 155 159 163 166 169 172 174 177 179 35 28 49 65 78 89 99 107 114 120 126 132 136 141 145 149 153 159 165 171 176 180 185 189 192 199 205 211 216 220 225 229 232 239 245 251 21 33 58 79 97 112 125 137 148 157 166 174 182 189 195 201 207 217 227 235 243 251 17 35 63 86 106 123 139 152 165 177 187 197 206 214 222 230 237 250 25 31 55 75 91 105 117 128 138 146 154 161 168 174 180 185 190 200 208 215 222 229 235 240 245 255 16 36 65 89 110 128 144 159 173 185 196 207 217 226 234 242 250 11 41 74 103 128 151 172 191 209 225 241 255 9 43 79 110 138 163 186 207 227 246 12 39 71 99 123 144 164 182 198 214 228 241 253 9 44 81 113 142 168 192 214 235 255 7 49 90 127 160 191 220 247 6 51 95 134 170 203 234 7 47 87 123 155 184 212 237 6 52 97 137 174 208 240 5 57 106 151 192 231 5 59 111 158 202 243 5 55 103 147 187 224 5 60 113 161 206 248 4 65 122 175 224 4 67 127 182 234 )))
(defparameter +celt-cache-caps+ (make-array 168 :element-type '(unsigned-byte 8) :initial-contents '(224 224 224 224 224 224 224 224 160 160 160 160 185 185 185 178 178 168 134 61 37 224 224 224 224 224 224 224 224 240 240 240 240 207 207 207 198 198 183 144 66 40 160 160 160 160 160 160 160 160 185 185 185 185 193 193 193 183 183 172 138 64 38 240 240 240 240 240 240 240 240 207 207 207 207 204 204 204 193 193 180 143 66 40 185 185 185 185 185 185 185 185 193 193 193 193 193 193 193 183 183 172 138 65 39 207 207 207 207 207 207 207 207 204 204 204 204 201 201 201 188 188 176 141 66 40 193 193 193 193 193 193 193 193 193 193 193 193 194 194 194 184 184 173 139 65 39 204 204 204 204 204 204 204 204 201 201 201 201 198 198 198 187 187 175 140 66 40 )))
(defparameter +celt-window+
  ;; CELT overlap window: sin(pi/2 * sin^2(pi/2*(i+0.5)/overlap)); overlap=120
  (let ((w (make-array 120 :element-type 'double-float)))
    (dotimes (i 120 w)
      (let ((s (sin (* 0.5d0 pi (/ (+ i 0.5d0) 120)))))
        (setf (aref w i) (sin (* 0.5d0 pi s s)))))))

;;; ---- coarse-energy Laplace model (quant_bands.c) ------------------------
;; eMeans[21] (Q4->float): mean log-energy per band
(defparameter +celt-emeans+
  (make-array 21 :element-type 'double-float :initial-contents
    '(6.4375d0 6.25d0 5.75d0 5.3125d0 5.0625d0 4.8125d0 4.5d0 4.375d0 4.875d0
      4.6875d0 4.5625d0 4.4375d0 4.875d0 4.625d0 4.3125d0 4.5d0 4.375d0 4.625d0
      4.75d0 4.4375d0 3.75d0)))
;; prediction coefficients (0.9,0.8,0.65,0.5) and beta, per LM
(defparameter +celt-pred-coef+
  (make-array 4 :element-type 'double-float
    :initial-contents (list (/ 29440d0 32768) (/ 26112d0 32768) (/ 21248d0 32768) (/ 16384d0 32768))))
(defparameter +celt-beta-coef+
  (make-array 4 :element-type 'double-float
    :initial-contents (list (/ 30147d0 32768) (/ 22282d0 32768) (/ 12124d0 32768) (/ 6554d0 32768))))
(defparameter +celt-beta-intra+ (/ 4915d0 32768))
;; e_prob_model[LM][intra][42], probability-of-zero + decay pairs (Q8)
(defparameter +celt-e-prob-model+
  #(#(#(72 127 65 129 66 128 65 128 64 128 62 128 64 128 64 128 92 78 92 79 92 78 90 79 116 41 115 40 114 40 132 26 132 26 145 17 161 12 176 10 177 11)
      #(24 179 48 138 54 135 54 132 53 134 56 133 55 132 55 132 61 114 70 96 74 88 75 88 87 74 89 66 91 67 100 59 108 50 120 40 122 37 97 43 78 50))
    #(#(83 78 84 81 88 75 86 74 87 71 90 73 93 74 93 74 109 40 114 36 117 34 117 34 143 17 145 18 146 19 162 12 165 10 178 7 189 6 190 8 177 9)
      #(23 178 54 115 63 102 66 98 69 99 74 89 71 91 73 91 78 89 86 80 92 66 93 64 102 59 103 60 104 60 117 52 123 44 138 35 133 31 97 38 77 45))
    #(#(61 90 93 60 105 42 107 41 110 45 116 38 113 38 112 38 124 26 132 27 136 19 140 20 155 14 159 16 158 18 170 13 177 10 187 8 192 6 175 9 159 10)
      #(21 178 59 110 71 86 75 85 84 83 91 66 88 73 87 72 92 75 98 72 105 58 107 54 115 52 114 55 112 56 129 51 132 40 150 33 140 29 98 35 77 42))
    #(#(42 121 96 66 108 43 111 40 117 44 123 32 120 36 119 33 127 33 134 34 139 21 147 23 152 20 158 25 154 26 166 21 173 16 184 13 184 10 150 13 139 15)
      #(22 178 63 114 74 82 84 83 92 82 103 62 96 72 96 67 101 73 107 72 113 55 118 52 125 52 118 52 117 55 135 49 137 39 157 32 145 29 97 33 77 40))))
(defparameter +celt-small-energy-icdf+
  (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(2 1 0)))

;;; ---- CELT flag icdf / trim tables (celt.h) ------------------------------
(defparameter +celt-trim-icdf+
  (make-array 11 :element-type '(unsigned-byte 8) :initial-contents '(126 124 119 109 87 41 19 9 4 2 0)))
(defparameter +celt-spread-icdf+
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(25 23 2 0)))
(defparameter +celt-tapset-icdf+
  (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(2 1 0)))
;; tf_select_table[4][8] (celt.c): index 4*isTransient + 2*tf_select + flag
(defparameter +celt-tf-select-table+
  #(#(0 -1 0 -1 0 -1 0 -1)
    #(0 -1 0 -2 1 0 1 -1)
    #(0 -2 0 -3 2 0 1 -1)
    #(0 -2 0 -3 3 0 1 -1)))
;; LOG2_FRAC_TABLE[24] (rate.c) — cost of coding uniform in [0,n) in 1/8 bits
(defparameter +celt-log2-frac-table+
  (make-array 24 :element-type '(unsigned-byte 8) :initial-contents
    '(0 8 13 16 19 21 23 24 26 27 28 29 30 31 32 32 33 34 34 35 36 36 37 37)))
;; exp2_table8 (bands.c compute_qn)
(defparameter +celt-exp2-table8+
  (make-array 8 :element-type '(signed-byte 32) :initial-contents
    '(16384 17866 19483 21247 23170 25267 27554 30048)))
;; comb-filter tap gains[3][3] (celt.c)
(defparameter +celt-comb-gains+
  #(#(0.3066406250d0 0.2170410156d0 0.1296386719d0)
    #(0.4638671875d0 0.2680664062d0 0.0d0)
    #(0.7998046875d0 0.1000976562d0 0.0d0)))
;; preemph[0] for 48 kHz de-emphasis
(defparameter +celt-preemph0+ 0.8500061035d0)
