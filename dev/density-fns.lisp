(in-package #:metabang.math)

;;; --*--
;;; ***************************************************************************
;;; Most of the following functions are implemented from Numerical Recipes in C:
;;; The Art of Scientific Computing, by William H Press, Brian P Flannery, Saul
;;; A Teukolsky, and William T Vetterling, published by Cambridge University
;;; Press, copyright 1988.  The code has been extensively tested and the values
;;; compared to tables of statistics or, in some cases, to figures in the
;;; Numerical Recipes book.

(defconstant +log-pi+ (log pi))
(defconstant +sqrt-pi+ (sqrt pi))

(defun gamma-ln (x)
  "Returns the natural logarithm of the Gamma function evaluated at `x.'
Mathematically, the Gamma function is defined to be the integral from 0 to
Infinity of t^x exp\(-t\) dt.  The implementation is copied, with extensions for
the reflection formula, from Numerical Recipes in C, section 6.1.  The argument
`x' must be positive.  Full accuracy is obtained for x>1.  For x<1, the
reflection formula is used.  The computation is done using double-floats, and
the result is a double-float."
  ;; For this implementation, we reverse the NRinC notation:  x <-> xx
  (flet ((do-it (x)
           (declare (optimize (speed 3) (space 1) (safety 0) (debug 0)) 
                    (type double-float x))
           (cond ((<= x 0.0d0) (error "arg to gamma-ln must be positive:  ~s" x))
                 ((> x 1.0d302)
                  (error "Argument too large:  ~e" x))
                 ((= x 0.5d0)
                  ;; special case for this arg, since it is used by the error-function
                  (log +sqrt-pi+))
                 ((< x 1.0d0)
                  ;; Use reflection formula:  Gamma(1-z) = z*pi/(Gamma(1+z)sin(pi*z))
                  (let ((z (- 1.0d0 x)))
                    (declare (type double-float z))
                    (- (+ (log z) +log-pi+) (+ (gamma-ln (+ 1.0d0 z)) (log (sin (* pi z)))))))
                 (t (let* ((xx  (- x 1.0d0))
                           (tmp (+ xx 5.5d0))
                           (ser 1.0d0))
                      (declare (type double-float xx tmp ser))
                      (decf tmp (* (+ xx 0.5d0) (log tmp)))
                      (dolist (coef '(76.18009173d0 -86.50532033d0 24.01409822d0
                                      -1.231739516d0 0.120858003d-2 -0.536382d-5))
                        (declare (type double-float coef))
                        (incf xx 1.0d0)
                        (incf ser (/ coef xx)))
                      (- (log (* 2.50662827465d0 ser)) tmp))))))
    
    (if (typep x 'double-float)
      (do-it x)
      (do-it (coerce x 'double-float)))))

;; ============================================================================

(defun factorial-exact (n)
  "Returns the factorial of `n,' which should be an integer.  The result will
returned as an integer or bignum.  This implementation is exact, but is more
computationally expensive than `factorial,' which is to be preferred."
  (check-type n fixnum)
  (if (< n 0)
      (error "N cannot be negative:  ~d" n)
      (do ((i 1 (1+ i))
	   (o 1 (* i o)))
	  ((> i n)
	   o))))

(defun factorial (n)
  "Returns the factorial of `n,' which should be a non-negative integer.  The
result will returned as a floating-point number, single-float if possible,
otherwise double-float.  If it is returned as a double-float, it won't
necessarily be integral, since the actual computation is

     (exp (gamma-ln (1+ n)))

Implementation is loosely based on Numerical Recipes in C, section 6.1.  On the
TI Explorer, the largest argument that won't cause a floating overflow is 170."
  (check-type n fixnum)
  (let ((cache (load-time-value
		 ;; On the TI, 34! is the largest factorial representable as a single-float
		 (let ((a (make-array 35 :element-type 'single-float)))
		   (dotimes (n 35)
		     (setf (aref a n) (coerce (factorial-exact n) 'single-float)))
		   a))))
    (cond ((< n 0)
	   (error "N cannot be negative:  ~d" n))
	  ((< n (length cache))
	   (aref cache n))
	  (t
	   (exp (gamma-ln (float (1+ n))))))))

#+test
(defun test-gamma-ln-and-factorial ()
  (dotimes (i 100)
    (let ((f1 (coerce (factorial-exact i) 'double-float))
	  (f2 (coerce (factorial i) 'double-float))
	  (f3 (coerce (exp (gamma-ln (1+ i))) 'double-float)))
      (let ((ratio1 (coerce (/ f1 f2) 'single-float))
	    (ratio2 (coerce (/ f1 f3) 'single-float)))
	(unless (= 1.0 ratio1 ratio2)
	  (format t "~2d ~g   ~g~%" i ratio1 ratio2)))))
  (format t "~2%")
  (let ((n (random 100)))
    (time:timeit (:cpu :label (format nil "~d! exact" n)) (factorial-exact n))
    (time:timeit (:cpu :label (format nil "~d!  aref" n)) (factorial 30))
    (time:timeit (:cpu :label (format nil "~d! gamma" n)) (exp (gamma-ln (1+ n))))))

(defun factorial-ln (n)
  "Returns the natural logarithm of n!; `n' should be an integer.  The result
will be a single-precision, floating point number.  The implementation follows
Numerical Recipes in C, section 6.1"
  (check-type n fixnum)
  (let ((cache (load-time-value
		 (let ((a (make-array 101 :element-type 'single-float)))
		   (dotimes (n 101)
		     (setf (aref a n)
			   (coerce (log (coerce (factorial-exact n)
						'double-float))
				   'single-float)))
		   a))))
    (cond ((< n 0)
	   (error "N cannot be negative:  ~d" n))
	  ((<= n 1)
	   0.0)
	  ((< n (length cache))
	   (aref cache n))
	  (t
	   (gamma-ln (float (1+ n)))))))

(defun binomial-coefficient (n k)
  "Returns the binomial coefficient, `n' choose `k,' as an integer.  The result
may not be exactly correct, since the computation is done with logarithms.  The
result is rounded to an integer.  The implementation follows Numerical Recipes
in C, section 6.1"
  (check-type n (integer 0 #.most-positive-fixnum))
  (check-type k (integer 0 #.most-positive-fixnum))
  (assert (<= k n) (k) "k must be less than n: ~d" k)
  (values (round (exp (- (factorial-ln n) (factorial-ln k) (factorial-ln (- n k)))))))

(defun binomial-coefficient-exact (n k)
  "This is an exact but computationally intensive form of the preferred
function, `binomial-coefficient.'"
  ;; This works, but boy, is it slow.
  #+ignore				   
  (cond ((= k 0) 1)
	((= k n) n)
	((= k 1) n)
	((> (* 2 k) n)
	 (binomial-coefficient-exact n (- n k)))
	(t
	 (+ (binomial-coefficient-exact (1- n) (1- k))
	    (binomial-coefficient-exact (1- n) k))))
  ;; This uses bignums, but is actually fairly fast.
  (/ (factorial-exact n)
     (factorial-exact k)
     (factorial-exact (- n k))))

#+test
(defun test-binomial-coefficient ()
  (flet ((=* (level x y)
	   (if (= level 0)
	       (= x y)
	       ;; returns true if fractional error is less than `level'
	       (<= (/ (* 2.0 (abs (- x y))) (+ x y)) level))))
    (flet ((test-range (min max level)
	     (do ((n min (1+ n)))
		 ((> n max))
	       (dotimes (k n)
		 (if (not (=* (float level)
			      (float (binomial-coefficient n k))
			      (float (binomial-coefficient-exact n k))))
		     (format t "at level ~1d, bico(~d,~d) => ~40t~:d~%   but exact is ~40t~:d~2%"
			     (if (zerop level) 0 (round (log level 10)))
			     n k
			     (binomial-coefficient n k)
			     (binomial-coefficient-exact n k)))))))
      (test-range  2  21 0)			; perfect, so far
      (test-range 21  24 1e-6)			; roughly 6 digits of accuracy
      (test-range 24  50 1e-5)			; roughly 5 digits of accuracy
      ;; 4 digits of accuracy for all n above 50 and below floating overflow.
      )))

(defun binomial-probability (p n k)
  "Returns the probability of `k' successes in `n' trials, where at each trial
the probability of success is `p.' This function uses floating-point
approximations, and so is computationally efficient but not necessarily exact."
  (check-type p float)
  (check-type n integer)
  (check-type k integer)
  ;; Rather than signal an error if p is extreme, we might as well try to return
  ;; something sensible.  In fact, the exact function returns the same values.
  (case p
    (0.0 (if (zerop k) 1.0 0.0))
    (1.0 (if (= k n) 1.0 0.0))
    (t   (exp (+ (- (factorial-ln n) (factorial-ln k) (factorial-ln (- n k)))
		 (* k (log p))
		 (* (- n k) (log (- 1.0 p))))))))

(defun binomial-probability-exact (p n k)
  "This is an exact but computationally intensive form of the preferred
function, `binomial-probability.'"
  (check-type p real)
  (check-type n integer)
  (check-type k integer)
  (* (binomial-coefficient-exact n k)
     (expt p k)
     (expt (- 1 p) (- n k))))

#+test
(defun test-binomial-probability ()
  (flet ((compare (p n k)
	   (let* ((exact (float (binomial-probability-exact p n k)))
		  (float (binomial-probability (float p) n k))
		  (delta (abs (- exact float))))
	   (if (> delta 1e-7)
	       (spy p n k exact float delta)))))
    (compare 1/2 30 20)
    (compare 1/2 40 20)
    (compare 1/3 20 10)
    (compare 1/3 30 15)
    (compare 1/10 30 1)))

;;; ============================================================================

(defun beta (z w)
  "Returns the value of the Beta function, defined in terms of the complete
gamma function, G, as: G(z)G(w)/G(z+w).  The implementation follows Numerical
Recipes in C, section 6.1."
  (exp (- (+ (gamma-ln z)
	     (gamma-ln w))
	  (gamma-ln (+ z w)))))

;;; ============================================================================

(defun safe-exp (x)
  "Eliminates floating point underflow for the exponential function.
Instead, it just returns 0.0d0"
  (setf x (coerce x 'double-float))
  (if (< x (log least-positive-double-float))
      0.0d0
      (exp x)))

(defmacro underflow-goes-to-zero (&body body)
  "Protects against floating point underflow errors and sets the value to 0.0 instead."
  #+EXPLORER
  `(ticl:condition-bind ((eh:floating-exponent-underflow-error 
			   #'(lambda (ignore) :use-zero)))
			(progn ,@body))
  #-EXPLORER
  `(handler-case 
       (progn ,@body)
       (#-Lucid floating-point-underflow
	#+Lucid lcl::floating-point-underflow
	(condition)
	(declare (ignore condition))
	(values 0.0d0))))

#| Grabbed this off the net.

(defmacro without-floating-underflow-traps (&body body)
  `(handler-bind ((floating-point-underflow
                   #'(lambda (e)
                       (when (find-restart 'use-value e)
                         ;; Apply coercion rule to get appropriate result type
			 (let* ((zeros (mapcar #'(lambda (v) (float 0 v))))
                                (result (apply #'+ zeros)))
			   (use-value result e))))))
   ,@body))

|#


(defun gamma-incomplete (a x)
  "This is an incomplete gamma function, what Numerical Recipes in C calls
``gammp.'' This function also returns, as the second value, g(a,x).  See the
manual for more information."
  (check-type a float)
  (check-type x float)
  (assert (> a 0.0) (a) "a must be positive:  ~g" a)
  (assert (>= x 0.0) (x) "x must be non-negative:  ~g" x)
  (let ((gln (gamma-ln a)))
    (when (= x 0.0)
      (return-from gamma-incomplete (values 0.0 gln)))
      (if (< x (+ a 1.0))
	  ;; Use series representation.  The following is the code of what
	  ;; Numerical Recipes in C calls ``GSER'
	  (let* ((itmax 100)
		 (eps   3.0e-7)
		 (ap    a)
		 (sum   (/ 1.0 a))
		 (del sum))
	    (declare (type float ap sum del))
	    (dotimes (i itmax)
	      (incf ap 1.0)
	      (setf del (* del (/ x ap)))
	      (incf sum del)
	      (if (< (abs del) (* eps (abs sum)))
		  (let ((result (underflow-goes-to-zero
				 (* sum (safe-exp (- (* a (log x)) x gln))))))
		    (return-from gamma-incomplete (values result gln)))))
	    (error "Series didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a itmax))
	  ;; Use the continued fraction representation.  The following is the
	  ;; code of what Numerical Recipes in C calls ``GCF.'' Their code
	  ;; computes the complement of the desired result, so we subtract from
	  ;; 1.0 at the end.
	  (let ((itmax 100)
		(eps   3.0e-7)
		(gold 0.0) (g 0.0) (fac 1.0) (b1 1.0) (b0 0.0)
		(anf 0.0) (ana 0.0) (an 0.0) (a1 x) (a0 1.0))
	    (declare (type float gold g fac b1 b0 anf ana an a1 a0))
	    (dotimes (i itmax)
	      (setf an  (float (1+ i))
		    ana (- an a)
		    a0  (* fac (+ a1 (* a0 ana)))
		    b0  (* fac (+ b1 (* b0 ana)))
		    anf (* fac an)
		    a1  (+ (* x a0) (* anf a1))
		    b1  (+ (* x b0) (* anf b1)))
	      (unless (zerop a1)
		(setf fac (/ 1.0 a1)
		      g   (* b1 fac))
		(if (< (abs (/ (- g gold) g)) eps)
		    (let ((result (underflow-goes-to-zero
				   (* (safe-exp (- (* a (log x)) x gln)) g))))
		      (return-from
			gamma-incomplete (values (- 1.0 result) gln)))
		    (setf gold g))))
	    (error "Continued Fraction didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a itmax)))))

#+test
(defun test-gamma-incomplete ()
  "This function reproduces figure 6.2.1 given in Numerical Recipes in C,
section 6.2, and acts as a kind of gross check on the `gamma-incomplete'
function.  It also reports particular values, which can be compared to tables."
  (dotimes (a-1 10)
    (dotimes (x-1 8)
      (let* ((a     (float (1+ a-1)))
	     (x     (float (1+ x-1)))
	     (gamma (gamma-incomplete a x)))
	(spy a x gamma))))
  (when (y-or-n-p "plot?")
    (flet ((igf (a)
	     (let ((pts nil))
	       (dotimes (x 16)
		 (push (list x (gamma-incomplete a (float x))) pts))
	       `((:polyline-without-vertices ,(format nil "a = ~s" a) 0) . ,pts))))
      (plotter:plot-stuff
	(list (igf 0.5)
	      (igf 1.0)
	      (igf 3.0)
	      (igf 10.0))
	:title "The incomplete gamma function P(a,x) for a=.5, 1.0, 3.0, and 10.0"))))

;;; ============================================================================

(defun error-function (x)
  "Computes the error function, which is typically used to compute areas under
the Gaussian probability distribution.  See the manual for more information.
Also see the function `gaussian-cdf.'

This implementation follows Numerical Recipes in C, section 6.2"
  (let ((erf (gamma-incomplete .5 (square x))))	; for x>0
    ;; because erf(-x)=-erf(x)
    (if (>= x 0.0) erf (- erf))))

(defun gaussian-cdf (x &optional (mean 0.0) (sd 1.0))
  "Computes the cumulative distribution function for a Gaussian random variable
\(defaults: mean=0.0, s.d.=1.0\) evaluated at `x.' The result is the probability
of getting a random number less than or equal to `x,' from the given Gaussian
distribution."
  (when (= sd 0.0)
    (if *gaussian-cdf-signals-zero-standard-deviation-error*
        (error 'zero-standard-deviation)
      (if (> x mean) (return-from gaussian-cdf  1.0) (return-from gaussian-cdf 0.0))))
  (let ((z (/ (- x mean) sd)))
    (* .5 (+ 1.0 (error-function (/ z (sqrt 2.0)))))))

#+test
(defun test-error-function ()
  ;; just compare to tabulated values in any statistics book
  (dolist (x '(-3.0 -2.0 -1.0 0.0 1.0 2.0 3.0))
    (format t "Gaussian CDF of ~4f is ~f~%" x (gaussian-cdf x))))

(defun error-function-complement (x)
  "This function computes the complement of the error function, ``erfc\(x\),''
defined as 1-erf\(x\).  See the documentation for `error-function' for a more
complete definition and description.  Essentially, this function on z/\sqrt2
returns the two-tailed significance of z in a standard Gaussian distribution.

This function implements the function that Numerical Recipes in C calls erfcc,
see section 6.3; that is, it's the one using the Chebyshev approximation, since
that is the one they call from their statistical functions.  It is quick to
compute and has fractional error everywhere less than 1.2x10^{-7}."
  (check-type x float)
  (let* ((z   (abs x))
	 (y   (/ 1.0 (+ 1.0 (* 0.5 z))))   ; instead of t
	 (ans
          (error-function-complement-short-1 y z)
          #+COMPILER-BUG
          (* y (exp (+ (* (- z) z)
                       -1.26551223
                       (* y
                          (+ 1.00002368
                             (* y
                                (+ 0.37409196
                                   (* y
                                      (+ 0.09678418
                                         (* y
                                            (+ -0.18628806
                                               (* y
                                                  (+ 0.27886807
                                                     (* y
                                                        (+ -1.13520398
                                                           (* y
                                                              (+ 1.48851587
                                                                 (* y
                                                                    (+ -0.82215223
                                                                       (* y 0.17087277))))))))))))))))))))))
    (declare (type float z y ans))
    (if (>= x 0.0)
      ans
      (- 2.0 ans))))

#+CRAP ;Compiler bug here.  (Westy)
(defun error-function-complement-helper (y z)
  (* y (exp (+ (* (- z) z)
               -1.26551223
               (* y 
		  (+ 1.00002368
		     (* y 
			(+ 0.37409196
			   (* y 
			      (+ 0.09678418
				 (* y 
				    (+ -0.18628806
				       (* y 
					  (+ 0.27886807
					     (* y 
						(+ -1.13520398
						   (* y 
						      (+ 1.48851587
							 (* y 
							    (+ -0.82215223
							       (* y 0.17087277)))))))))))))))))))))

(defun error-function-complement-short-1 (y z)
  (* y (exp (+ (* (- z) z)
               -1.26551223
               (* y 
		  (+ 1.00002368
		     (* y 
			(+ 0.37409196
			   (* y 
			      (+ 0.09678418
				 (* y 
				    (+ -0.18628806
                                       (error-function-complement-short-2 y)))))))))))))

(defun error-function-complement-short-2 (y)
  (* y 
     (+ 0.27886807
        (* y 
           (+ -1.13520398
              (* y 
                 (+ 1.48851587
                    (* y 
                       (+ -0.82215223
                          (* y 0.17087277))))))))))


  
#+test
(defun test-error-function-complement ()
  (dotimes (i 100)
    (let* ((abs-x (/ i 20.0))		   ; range is 0 to 5.0
	   (x     (if (zerop (random 2)) abs-x (- abs-x)))
	   (e1    (error-function-complement x))
	   (e2    (- 1.0 (error-function x)))
	   (del   (- e1 e2)))
      (when (> (abs del) 1e-6)		   ; 10^-6 agreement between the two error functions
	(format t "~&on ~7f, difference is ~f" x del)
	(spy e1 e2))))
  #+Explorer
  ;; The following shows erfcc to be about 4 times faster and does a lot less number-consing
  (time:timeit (:cpu :cons :number-cons :label "erf")
    (error-function 1.75))
  #+Explorer
  (time:timeit (:cpu :cons :number-cons :label "erfcc")
    (error-function-complement 1.75)))

(defun gaussian-significance (x tails &optional mean sd)
  "Computes the significance of `x' in a Gaussian distribution with mean=`mean'
\(default 0.0\) and standard deviation=`sd' \(default 1.0\); that is, it returns
the area which farther from the mean than `x' is.

The null hypothesis is roughly that `x' is zero; you must specify your
alternative hypothesis (H1) via the `tails' parameter, which must be :both,
:positive or :negative.  The first corresponds to a two-tailed test: H1 is that
`x' is not zero, but you are not specifying a direction.  If the parameter is
:positive, H1 is that `x' is positive, and similarly for :negative."
  (when mean (setf x (- x mean)))
  (when sd   (setf x (/ x sd)))
  (let ((a (error-function-complement (* (abs x) (/ (sqrt 2.0d0))))))
    ;; A is 2*Integral from x to Infinity of the z distribution
    (ecase tails
      (:both a)
      ;; The TI compiler actually does the right thing with the following code:
      ;; the repeated expressions only appear once in the object code!
      (:positive (if (plusp x)
		     (* .5 a)
		     (- 1.0 (* .5 a))))
      (:negative (if (plusp x)
		     (- 1.0 (* .5 a))
		     (* .5 a))))))

;;; ============================================================================

(defun poisson-cdf (k x)
  "Computes the cumulative distribution function for a Poisson random variable
with mean `x' evaluated at `k.' The result is the probability that the number of
Poisson random events occurring will be between 0 and k-1 inclusive, if the
expected number is `x.' The argument `k' should be an integer, while `x' should
be a float.  The implementation follows Numerical Recipes in C, section 6.2"
  (check-type k integer)
  (check-type x float)
  (- 1.0 (gamma-incomplete (float k x) x)))

(defun chi-square-significance (x dof)
  "Computes the complement of the cumulative distribution function for a
Chi-square random variable with `dof' degrees of freedom evaluated at `x.' The
result is the probability that the observed chi-square for a correct model
should be greater than `x.' The implementation follows Numerical Recipes in C,
section 6.2.  Small values suggest that the null hypothesis should be rejected;
in other words, this computes the significance of `x.'"
  (check-type dof integer)
  (check-type x float)
  (- 1.0 (gamma-incomplete (* 0.5 dof) (* 0.5 x))))

;;; ============================================================================

(defun beta-incomplete (a b x)
   "This function is useful in defining the cumulative distributions for
Student's t and the F distribution.

All arguments must be floating-point numbers; `a' and `b' must be positive and
`x' must be between 0.0 and 1.0, inclusive."
   (check-type a (float (0.0) *))
   (check-type b (float (0.0) *))
   (check-type x (float 0.0 1.0))
   (flet ((betacf (a b x)
	    ;; straight from Numerical Recipes in C, section 6.3
	    (declare (type float a b x))
	    (let ((itmax 100)
		  (eps   3.0e-7)
		  (qap 0.0) (qam 0.0) (qab 0.0) (em  0.0) (tem 0.0) (d 0.0)
		  (bz  0.0) (bm  1.0) (bp  0.0) (bpp 0.0)
		  (az  1.0) (am  1.0) (ap  0.0) (app 0.0) (aold 0.0))
	      (declare (type float qap qam qab em tem d
			     bz bm bp bpp az am ap app aold))
	      (setf qab (+ a b)
		    qap (+ a 1.0)
		    qam (- a 1.0)
		    bz  (- 1.0 (/ (* qab x) qap)))
	      (dotimes (m itmax)
		(setf em   (float (1+ m))
		      tem  (+ em em)
		      d    (/ (* em (- b em) x)
			      (* (+ qam tem) (+ a tem)))
		      ap   (+ az (* d am))
		      bp   (+ bz (* d bm))
		      d    (/ (* (- (+ a em)) (+ qab em) x)
			      (* (+ qap tem) (+ a tem)))
		      app  (+ ap (* d az))
		      bpp  (+ bp (* d bz))
		      aold az
		      am   (/ ap bpp)
		      bm   (/ bp bpp)
		      az   (/ app bpp)
		      bz   1.0)
		(if (< (abs (- az aold)) (* eps (abs az)))
		    (return-from betacf az)))
	      (error "a=~s or b=~s too big, or itmax too small in betacf"
		     a b))))
      (declare (notinline betacf))
      (when (or (< x 0.0) (> x 1.0))
	 (error "x must be between 0.0 and 1.0:  ~f" x))
      ;; bt is the factors in front of the continued fraction
      (let ((bt (if (or (= x 0.0) (= x 1.0))	    
		    0.0
		    (exp (+ (gamma-ln (+ a b))
			    (- (gamma-ln a))
			    (- (gamma-ln b))
			    (* a (log x))
			    (* b (log (- 1.0 x))))))))
	 (if (< x (/ (+ a 1.0) (+ a b 2.0)))
	     ;; use continued fraction directly
	     (/ (* bt (betacf a b x)) a) 
	     ;; use continued fraction after making the symmetry transformation
	     (- 1.0 (/ (* bt (betacf b a (- 1.0 x))) b))))))

#+test
(defun test-beta-incomplete ()
  "This function reproduces figure 6.3.1 from Numerical Recipes in C, section
6.2, and acts as a kind of gross check on the `beta-incomplete' function."
  (flet ((ibf (a b)
	   (let ((pts nil))
	     (dotimes (x 101)
	       (push (list (* .01 x) (beta-incomplete a b (* .01 x))) pts))
	     `((:polyline-without-vertices ,(format nil "(~f,~f)" a b) 0) .
	       ,pts))))
    (plotter:plot-stuff
      (list (ibf 0.5 0.5)
	    (ibf 0.5 5.0)
	    (ibf 1.0 3.0)
	    (ibf 8.0 10.0)
	    (ibf 5.0 0.5)
	    )
      :y-first -0.05
      :y-last   1.05
      :title
      (format nil "The incomplete beta function I_x(a,b)~%~
                   for five different pairs of (a,b):~2%~
                   ~@{(~f,~f) ~}~2%~
                   Notice that the pairs (0.5,5.0) and (5.0,0.5)~%~
                   are related by reflection symmetry around the diagonal."
	      0.5 5.0 1.0 3.0 8.0 10.0 0.5 0.5 5.0 0.5))))

;;; ============================================================================

(defun students-t-significance (t-statistic dof tails)
  "Student's distribution is much like the Gaussian distribution except with
heavier tails, depending on the number of degrees of freedom, `dof.' As `dof'
goes to infinity, Student's distribution approaches the Gaussian.  This function
computes the significance of `t-statistic.' Values range from 0.0 to 1.0: small
values suggest that the null hypothesis---that `t-statistic' is drawn from a t
distribution---should be rejected.  The `t-statistic' parameter should be a
float, while `dof' should be an integer.

The null hypothesis is roughly that `t-statistic' is zero; you must specify your
alternative hypothesis (H1) via the `tails' parameter, which must be :both,
:positive or :negative.  The first corresponds to a two-tailed test: H1 is that
`t-statistic' is not zero, but you are not specifying a direction.  If the
parameter is :positive, H1 is that `t-statistic' is positive, and similarly for
:negative.

This implementation follows Numerical Recipes in C, section 6.3."
  (declare (inline square))
  (check-type t-statistic float)
  (check-type dof integer)
  (check-type tails (member :both :positive :negative))
  (setf dof (float dof t-statistic))
  (let ((a (beta-incomplete (* 0.5 dof) 0.5 (/ dof (+ dof (square t-statistic))))))
    ;; A is 2*Integral from (abs t-statistic) to Infinity of t-distribution
    (case tails
      (:both a)
      ;; The TI compiler actually does the right thing with the following code:
      ;; the repeated expressions only appear once in the object code!
      (:positive (if (plusp t-statistic)
		     (* .5 a)
		     (- 1.0 (* .5 a))))
      (:negative (if (plusp t-statistic)
		     (- 1.0 (* .5 a))
		     (* .5 a))))))



(defun f-significance
       (f-statistic numerator-dof denominator-dof &optional one-tailed-p)
  "This function occurs in the statistical test of whether two observed samples
have the same variance.  A certain statistic, F, essentially the ratio of the
observed dispersion of the first sample to that of the second one, is
calculated.  This function computes the tail areas of the null hypothesis: that
the variances of the numerator and denominator are equal.  It can be used for
either a one-tailed or two-tailed test.  The default is two-tailed, but
one-tailed can be computed by setting the optional argument `one-tailed-p' to
true.

For a two-tailed test, this function computes the probability that F would be as
different from 1.0 \(larger or smaller\) as it is, if the null hypothesis is
true.

For a one-tailed test, this function computes the probability that F would be as
LARGE as it is if the first sample's underlying distribution actually has
SMALLER variance that the second's, where `numerator-dof' and `denominator-dof'
is the number of degrees of freedom in the numerator sample and the denominator
sample.  In other words, this computes the significance level at which the
hypothesis ``the numerator sample has smaller variance than the denominator
sample'' can be rejected.

A small numerical value implies a very significant rejection.

The `f-statistic' must be a non-negative floating-point number.  The degrees of
freedom arguments must be positive integers.  The `one-tailed-p' argument is
treated as a boolean.

This implementation follows Numerical Recipes in C, section 6.3 and the `ftest'
function in section 13.4.  Some of the documentation is also drawn from the
section 6.3, since I couldn't improve on their explanation."
  (check-type f-statistic (float 0.0 *))
  (check-type numerator-dof (integer 1 *))
  (check-type denominator-dof (integer 1 *))
  (let ((tail-area (beta-incomplete
		     (* 0.5 denominator-dof)
		     (* 0.5 numerator-dof)
		     (float (/ denominator-dof
			       (+ denominator-dof
				  (* numerator-dof f-statistic)))))))
    (if one-tailed-p
	tail-area
	;; Because of symmetry, we can double the area, but if H0 is strongly
	;; viable, the tails can get confused, so we ensure that the area is
	;; less than 1.0
	(progn (setf tail-area (* 2.0 tail-area))
	       (if (> tail-area 1.0)
		   (- 2.0 tail-area)
		   tail-area)))))

;;; ============================================================================

(defun binomial-cdf (p n k)
  "Suppose an event occurs with probability `p' per trial.  This function
computes the probability of `k' or more events occurring in `n' trials.  Note
that this is the complement of the usual definition of cdf.  This function
approximates the actual computation using the incomplete beta function, but is
preferable for large `n' \(greater than a dozen or so\) because it avoids
summing many tiny floating-point numbers.

The implementation follows Numerical Recipes in C, section 6.3."
  (assert (<= k n) () "Can't have more events than trials, so k <= n")
  (if (zerop k)
      1.0
      (beta-incomplete (float k) (float (1+ (- n k))) (float p))))

(defun binomial-cdf-exact (p n k)
  "This is an exact but computationally intensive form of the preferred
function, `binomial-cdf.'"
  (let ((sum 0))
    (do ((i k (1+ i)))
	((> i n))
      (incf sum (binomial-probability-exact p n i)))
    sum))

#+test
(defun test-binomial-cdf ()
  (flet ((compare (p n k)
	   (let* ((exact (coerce (binomial-cdf-exact p n k) 'double-float))
		  (float (binomial-cdf (coerce p 'double-float) n k))
		  (delta (abs (- exact float))))
	     (if (> delta 1e-9)
		 (spy p n k exact float delta)))))
    (compare 1/2 30 20)
    (compare 1/2 40 20)
    (compare 1/3 20 10)
    (compare 1/3 30 15)
    (compare 1/10 30 25))
  ;; The following shows the exact computation to be much slower and do a lot
  ;; more number-consing
  (time:timeit (:cpu :cons :number-cons :label "float")
    (binomial-cdf 0.5d0 50 30))
  (time:timeit (:cpu :cons :number-cons :label "exact")
    (binomial-cdf-exact 1/2 50 30)))

;;; ***************************************************************************
;;; EOF


