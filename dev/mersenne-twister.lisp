(in-package metabang.math)

#|
tests

better names for constants

Can we speed up integer random by passing in a temporary float?

Problem is that we're using bignums... can we restrict to 24/28-bits?

mt generates integers ~1.8 times faster (somewhat less consing)
ran1 generate floats 1.3 times faster (somewhat less consing)

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'mt-random-number-generator)))
        (loop repeat 10000 do
              (setf x (uniform-random g 0.0 1.0)))
        x))

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'mt-random-number-generator)))
        (loop repeat 10000 do
              (setf x (next-element g)))
        x))

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'mt-random-number-generator-1)))
        (loop repeat 10000 do
              (setf x (next-element g)))
        x))

(let ((g (make-random-number-generator 5489 'mt-random-number-generator)))
  (loop repeat 4 do
        (format t "~&~D" (next-element g))))
        
.139 .145 .155 .146 .181
.145 .155 .146

(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'ran1-random-number-generator)))
        (loop repeat 10000 do
              (setf x (uniform-random g 0.0 1.0)))
        x))


(timeit (:report t)
      (let ((x 0)
            (g (make-random-number-generator 42 'ran1-random-number-generator)))
        (loop repeat 10000 do
              (setf x (next-element g)))
        x))
.146 .126 .136 .129 .106
.146 .136 .129

(pro:with-profiling
  (let ((x 0)
        (g (make-random-number-generator 42 'mt-random-number-generator)))
    (loop repeat 100000 do
          (setf x (next-element g)))
    x))

(pro:with-profiling
  (let ((x 0)
        (g (make-random-number-generator 42 'ran1-random-number-generator)))
    (loop repeat 100000 do
          (setf x (next-element g)))
    x))
|#

;; Period parameters 
(defconstant +N+ 624)
(defconstant +M+ 397)
(defconstant +matrix-a+ #x9908b0df)   ; constant vector a 
(defconstant +upper-mask+ #x80000000) ; most significant w-r bits 
(defconstant +lower-mask+ #x7fffffff) ; least significant r bits 
(defconstant +mag01+ (make-array 2 :initial-contents (list 0 +matrix-a+)))
  
;;; ---------------------------------------------------------------------------

(defclass* mt-random-number-generator (basic-random-number-generator)
  ((mt-array :unbound r)
   (mt-index :unbound r)
   (random-seed :unbound ir))
  :copy-slots
  (:default-initargs 
    :random-seed 5489)
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object mt-random-number-generator) &key)
  (setf (slot-value object 'mt-array)
        (make-array +N+ :initial-element 0)
        (slot-value object 'mt-index)
        (1+ +N+)))

;;; ---------------------------------------------------------------------------

;; initializes mt[N] with a seed
(defun init-genrand (generator seed)
  (with-slots (mt-array mt-index) generator
    (declare (type (simple-array (unsigned-byte 32) (624)) mt-array))
    (setf (svref mt-array 0) (logand seed #xffffffff))
    (do ((i 1 (1+ i)))
        ((>= i +N+))
      ; See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. 
      ; In the previous versions, MSBs of the seed affect   
      ; only MSBs of the array mt[].                        
      ; 2002/01/09 modified by Makoto Matsumoto             
      (setf (svref mt-array i)
            (logand
             (+ 
              (* 1812433253 
                 (logxor (svref mt-array (1- i))
                         (ash (svref mt-array (1- i)) -30)))
              i)
             #xffffffff)))
    (setf mt-index +N+)))

;;; ---------------------------------------------------------------------------

;; initialize by an array with array-length 
;; init_key is the array for initializing keys 
;; key_length is its length 
(defun init-by-array (generator init-key)
  (with-slots (mt-array mt-index random-seed) generator
    (declare (type (simple-array (unsigned-byte 32) (624)) mt-array))
    (init-genrand generator 19650218) 
    (let ((i 1) (j 0) (key-length (array-dimension init-key 0)))
      (loop repeat (if (> +N+ key-length) +N+ key-length) do
            (setf (svref mt-array i)
                  (logand 
                   (+ (logxor (svref mt-array i)
                              (* (logxor (svref mt-array (1- i))
                                         (ash (svref mt-array (1- i)) -30))
                                 1664525))
                      (svref init-key j) j)      ; non-linear
                   #xffffffff))
            (incf i)
            (incf j)
            (when (>= i +N+)
              (setf (svref mt-array 0) (svref mt-array (1- +N+))
                    i 1))
            (when (>= j key-length) (setf j 0)))
      (loop repeat (1- +N+) do
            (setf (svref mt-array i)
                  (logand
                   (- (logxor (svref mt-array i)
                              (* (logxor (svref mt-array (1- i))
                                         (ash (svref mt-array (1- i)) -30))
                                 1566083941))      ; non-linear
                      i)
                   #xffffffff))
            (incf i)
            (when (>= i +N+)
              (setf (svref mt-array 0) (svref mt-array (1- +N+))
                    i 1)))
      
      ; MSB is 1, assuring non-zero initial array
      (setf (svref mt-array 0) #x80000000))))
        
;;; ---------------------------------------------------------------------------

;; generates a random number on [0,0xffffffff]-interval
(defmethod next-element ((generator mt-random-number-generator))
  (declare (optimize speed))
  (with-slots (mt-array mt-index random-seed) generator
    (declare (type (simple-array (unsigned-byte 32) (624)) mt-array))
    (declare (type fixnum mt-index))
    (let ((y 0))
      (declare (type (unsigned-byte 32) y))
      (when (>= mt-index +N+)
        ;; generate N words at one time
        (when (= mt-index (1+ +N+))
          ;; init-genrand() has not been called
          (init-genrand generator random-seed))
        
        (loop for k from 0 to (1- (- +N+ +M+)) do
              (setf y (logior (logand (svref mt-array k) +upper-mask+)
                              (logand (svref mt-array (1+ k)) +lower-mask+)))
              (setf (svref mt-array k)
                    (logxor (svref mt-array (+ k +M+))
                            (ash y -1)
                            (svref +mag01+ (logand y #x1)))))
        
        (loop for k from (- +N+ +M+) to (- +N+ 2) do
              (setf y (logior (logand (svref mt-array k) +upper-mask+)
                              (logand (svref mt-array (1+ k)) +lower-mask+)))
              (setf (svref mt-array k)
                    (logxor (svref mt-array (+ k (- +M+ +N+)))
                            (ash y -1)
                            (svref +mag01+ (logand y #x1)))))
        
        (setf y (logior (logand (svref mt-array (1- +N+)) +upper-mask+)
                        (logand (svref mt-array 0) +lower-mask+))
              (svref mt-array (1- +N+)) 
              (logxor (svref mt-array (1- +M+)) (ash y -1)
                      (svref +mag01+ (logand y #x1))))
        
        (setf mt-index 0))
      
      (setf y (svref mt-array mt-index)
            y (logxor y (ash y -11))
            y (logxor y (logand (ash y 7) #x9d2c5680))
            y (logxor y (logand (ash y 15) #xefc60000))
            y (logxor y (ash y -18)))
      (incf mt-index)
      
      #+Ignore
      (/ y #.(float (expt 2 32))) y)))


#+Test
(let ((g (make-random-number-generator 42 'mt-random-number-generator))
      (init (make-array 4 :initial-contents '(#x123 #x234 #x345 #x456))))
  (init-by-array g init)
  (loop repeat 4 do
        (format t "~&~15D" (next-element g))))