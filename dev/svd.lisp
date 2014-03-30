;;;; -*- Mode:Common-Lisp; Package:eksl-utilities; Base:10 -*-
;;;; *-* File: eksl-math:svd.lisp *-*
;;;; *-* Last-edit: Tuesday, August 24, 2004 11:30:30; Edited-By: Gary *-* 
;;;; *-* Machine: billy-pilgrim *-*
;;;; *-* Software: Macintosh Common Lisp, Version 5.1b2 *-*
;;;; *-* Lisp: Macintosh Common Lisp, Version 5.1b2 *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                      Singular Value Decomposition                      *
;;;; *                           Double Precision                             *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; SVD is the preferred method for inverting nearly singular matrices.  The
;;;; following routines are adapted from Numerical Recipes in C.
;;;; **************************************************************************
;;;
;;; Written by: Numerical Recipes In C
;;;             Translated to Lisp by J. Ross Beveridge
;;;             Modified and added to eksl-math by Scott D. Anderson 
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #:metabang.math)

;;; --*--
;;; ***************************************************************************

(defmacro aref1 (a i)
  `(aref ,a (- ,i 1)))

(defmacro aref11 (a i j)
  `(aref ,a (- ,i 1) (- ,j 1)))

#+test
(defun make-random-array (rows cols &optional (type 'single-float) (max 1000))
  "Returns an array of the correct size and element type, filled with random
positive floats less than `max.'"
  (let ((a   (make-array (list rows cols) :element-type type))
	(max (coerce max type)))
    (dotimes (i (array-total-size a))
      (setf (row-major-aref a i) (random max)))))

;;;--------------------------------------------------------------------------------
;;;                                       Double Precision
;;;                               SINGULAR VALUE DECOMPOSITION (SVD)
;;;
;;; SVD is the preferred method for inverting nearly singular matrixes.  The
;;; following routines are taken from 'Numerical Recipes in C'.
;;;
;;;--------------------------------------------------------------------------------

(defmacro sign-df (a b)
  `(if (>= ,b 0.0d0) (abs ,a) (- (abs ,a))))

(defun pythag-df (a b)
  "Computes square root of a*a + b*b without destructive overflow or underflow."
  (let ((at (abs a)) (bt (abs b)) (ct 0.0d0))
    (declare (double-float at bt ct))
    (cond ((> at bt)
	   (setf ct (/ bt at))
	   (* at (sqrt (+ 1.0d0 (* ct ct)))))
	  ((and (>= bt at) (not (= bt 0.0d0)))
	   (setf ct (/ at bt))
	   (* bt (sqrt (+ 1.0d0 (* ct ct)))))
	  (t 0.0d0))))

(defun svbksb-df (u w v m n b x &optional 
		  (tmp (make-array n :element-type 'double-float)))
  "Solves A X = B for a vector `X,' where A is specified by the mxn array U, `n'
vector W, and nxn matrix V as returned by svdcmp.  `m' and `n' are the
dimensions of `A,' and will be equal for square matrices.  `B' is the 1xm input
vector for the right-hand side.  `X' is the 1xn output solution vector.  All
arrays are of double-floats.  No input quantities are destroyed, so the routine
may be called sequentially with different B's.  See the discussion in Numerical
Recipes in C, section 2.6.

This routine assumes that near zero singular values have already been zeroed.
It returns no values, storing the result in `X.' It does use some auxiliary
storage, which can be passed in as `tmp,' a double-float array of length `n,' if
you want to avoid consing."
  (let ((s 0.0d0))
    (do ((j 1 (1+ j)))((> j n))			; calculate u^t b
      (setf s 0.0d0)
      (when (not (= (aref1 w j) 0.0d0))		; nonzero result only if w(j) is non zero
	(do ((i 1 (1+ i)))((> i m)) (incf s (* (aref11 u i j) (aref1 b i))))
	(setf s (/ s (aref1 w j))))		; this is the divide by w(j)
      (setf (aref1 tmp j) s))
    (do ((j 1 (1+ j)))((> j n))			; matrix multiply by v to get answer
      (setf s 0.0d0)
      (do ((jj 1 (1+ jj)))((> jj n)) (incf s (* (aref11 v j jj) (aref1 tmp jj))))
      (setf (aref1 x j) s))
    (values)))

(defun svdcmp-df (a m n w v &optional
		  (rv1 (make-array n :element-type 'double-float)))
  "Given an `m'x`n' matrix `A,' this routine computes its singular value
decomposition, A = U W V^T.  The matrix U replaces `A' on output.  The diagonal
matrix of singular values W is output as a vector `W' of length `n.' The matrix
`V' -- not the transpose V^T -- is output as an `n'x`n' matrix `V.' The row
dimension `m' must be greater or equal to `n'; if it is smaller, then `A' should
be filled up to square with zero rows.  See the discussion in Numerical Recipes
in C, section 2.6.

This routine returns no values, storing the results in `A,' `W,' and `V.' It
does use some auxiliary storage, which can be passed in as `rv1,' a double-float
array of length `n,' if you want to avoid consing."
  (let ((flag 0)(i 0)(l 0)(nm 0)
	(c 0.0d0)(f 0.0d0)(h 0.0d0)(s 0.0d0)(x 0.0d0)(y 0.0d0)(z 0.0d0)
	(anorm 0.0d0)(g 0.0d0)(scale 0.0d0))
    (declare (fixnum flag i l nm))
    (if (< m n) (error "SVDCMP: You must augment A with extra zero rows"))
    ;; Householder reduction to bidiagonal form
    (do ((i 1 (1+ i)))((> i n))
      (setf l (+ i 1))
      (setf (aref1 rv1 i) (* scale g))
      (setf g 0.0d0 s 0.0d0 scale 0.0d0)
      (when (<= i m)
	(do ((k i (1+ k)))((> k m)) (incf scale (abs (aref11 a k i))))
	(when (not (= scale 0.0d0))
	  (do ((k i (1+ k)))((> k m))
	    (setf (aref11 a k i) (/ (aref11 a k i) scale))
	    (incf s (* (aref11 a k i) (aref11 a k i))))
	  (setf f (aref11 a i i))
	  (setf g (- (sign-df (sqrt s) f)))
	  (setf h (- (* f g) s))
	  (setf (aref11 a i i) (- f g))
	  (when (not (= i n))
	    (do ((j l (1+ j)))((> j n))
	      (setf s 0.0d0)
	      (do ((k i (1+ k)))((> k m)) (incf s (* (aref11 a k i)
						     (aref11 a k j))))
	      (setf f (/ s h))
	      (do ((k i (1+ k)))((> k m)) (incf (aref11 a k j) (* f
								  (aref11 a k i))))))
	  (do ((k i (1+ k)))((> k m)) (setf (aref11 a k i) (* (aref11 a
								      k i) scale)))))
      (setf (aref1 w i) (* scale g))
      (setf g 0.0d0 s 0.0d0 scale 0.0d0)
      (when (and (<= i m) (not (= i n)))
	(do ((k l (1+ k)))((> k n)) (incf scale (abs (aref11 a i k))))
	(when (not (= scale 0.0d0))
	  (do ((k l (1+ k)))((> k n))
	    (setf (aref11 a i k) (/ (aref11 a i k) scale))
	    (incf s (* (aref11 a i k)(aref11 a i k))))
	  (setf f (aref11 a i l))
	  (setf g (- (sign-df (sqrt s) f)))
	  (setf h (- (* f g) s))
	  (setf (aref11 a i l) (- f g))
	  (do ((k l (1+ k)))((> k n)) (setf (aref1 rv1 k) (/ (aref11 a i k) h)))
	  (when (not (= i m))
	    (do ((j l (1+ j)))((> j m))
	      (setf s 0.0d0)
	      (do ((k l (1+ k)))((> k n)) (incf s (* (aref11 a j k)
						     (aref11 a i k))))
	      (do ((k l (1+ k)))((> k n)) (incf (aref11 a j k) (* s
								  (aref1 rv1 k))))))
	  (do ((k l (1+ k)))((> k n)) (setf (aref11 a i k) (* (aref11 a
								      i k) scale)))))
      (setf anorm (max anorm (+ (abs (aref1 w i)) (abs (aref1 rv1 i))))))
    ;;  accumulation of right hand transformations
    (do ((i n (1- i)))((< i 1))
      (when (< i n)
	(when (not (= g 0.0d0))
	  (do ((j l (1+ j)))((> j n))		; double division to avoid possible underflow
	    (setf (aref11 v j i) (/ (/ (aref11 a i j)(aref11 a i l)) g)))
	  (do ((j l (1+ j)))((> j n)) 
	    (setf s 0.0d0)
	    (do ((k l (1+ k)))((> k n)) (incf s (* (aref11 a i k) (aref11 v k j))))
	    (do ((k l (1+ k)))((> k n)) (incf (aref11 v k j) (* s
								(aref11 v k i))))))
	(do ((j l (1+ j)))((> j n)) (setf (aref11 v i j) 0.0d0 (aref11
								 v j i) 0.0d0)))
      (setf (aref11 v i i) 1.0d0)
      (setf g (aref1 rv1 i))
      (setf l i))
    ;;  Accumulation of left hand transformations
    (do ((i n (1- i)))((< i 1))
      (setf l (1+ i))
      (setf g (aref1 w i))
      (when (< i n)
	(do ((j l (1+ j)))((> j n)) (setf (aref11 a i j) 0.0d0)))
      (if (not (= g 0.0d0))
	  (progn
	    (setf g (/ 1.0d0 g))
	    (when (not (= i n))
	      (do ((j l (1+ j)))((> j n))
		(setf s 0.0d0)
		(do ((k l (1+ k)))((> k m)) (incf s (* (aref11 a k i)
						       (aref11 a k j))))
		(setf f (* (/ s (aref11 a i i)) g))
		(do ((k i (1+ k)))((> k m)) (incf (aref11 a k j) (* f
								    (aref11 a k i))))))
	    (do ((j i (1+ j)))((> j m)) (setf (aref11 a j i) (* (aref11 a j i) g))))
	  (progn				; else
	    (do ((j i (1+ j)))((> j m)) (setf (aref11 a j i) 0.0d0))))
      (incf (aref11 a i i)))
    ;;  Diagonalization of the Bidiagonal form
    (do ((k n (1- k)))((< k 1))			;  loop over singular values
      (do ((its 1 (1+ its)))((> its 30))	;  loop over allowed iterations
	(setf flag 1)
	;;  these c hacking turkeys are using forced exit to set the value of l
	(setf l k)
	(loop
	  (if (< l 1) (return))
	  (setf nm (- l 1))			; note that rv1's first element is always zero
	  (when (= (+ (abs (aref1 rv1 l)) anorm) anorm)
	    (setf flag 0)
	    (return))
	  (when (= (+ (abs (aref1 w nm)) anorm) anorm)
	    (return))
	  (decf l))
	(when (not (= flag 0))
	  (setf c 0.0d0)			; cancellation of first element of rv1, if l > 1
	  (setf s 1.0d0)
	  (do ((i l (1+ i)))((> i k))
	    (setf f (* s (aref1 rv1 i)))
	    (when (not (= (+ (abs f) anorm) anorm))
	      (setf g (aref1 w i))
	      (setf h (pythag-df f g))
	      (setf (aref1 w i) h)
	      (setf h (/ 1.0d0 h))
	      (setf c (* g h))
	      (setf s (- (* f h)))
	      (do ((j 1 (1+ j)))((> j m))
		(setf y (aref11 a j nm))
		(setf z (aref11 a j i))
		(setf (aref11 a j nm) (+ (* y c) (* z s)))
		(setf (aref11 a j  i) (- (* z c) (* y s)))))))
	(setf z (aref1 w k))
	(when (= l k)				;  convergence
	  (when (< z 0.0d0)			; singular value is made nonnegative
	    (setf (aref1 w k) (- z))
	    (do ((j 1 (1+ j)))((> j n)) (setf (aref11 v j k) (- (aref11 v j k)))))
	  (return))
	(when (= its 30) (error "no convergence in 30 svdcmp iterations"))
	(setf x (aref1 w l))			;  shiftr from bottom 2-by-2 minor
	(setf nm (- k 1))
	(setf y (aref1 w nm))
	(setf g (aref1 rv1 nm))
	(setf h (aref1 rv1 k))
	(setf f (/ (+ (* (- y z)(+ y z)) (* (- g h) (+ g h))) (* 2.0d0 h y)))
	(setf g (pythag-df f 1.0d0))
	(setf f (/ (+ (* (- x z)(+ x z)) (* h (- (/ y (+ f (sign-df g f))) h))) x))
						; next qr transformation
	(setf c 1.0d0 s 1.0d0)
	(do ((j l (1+ j)))((> j nm))
	  (setf i (1+ j))
	  (setf g (aref1 rv1 i))
	  (setf y (aref1 w i))
	  (setf h (* s g))
	  (setf g (* c g))
	  (setf z (pythag-df f h))
	  (setf (aref1 rv1 j) z)
	  (setf c (/ f z))
	  (setf s (/ h z))
	  (setf f (+ (* x c)(* g s)))
	  (setf g (- (* g c)(* x s)))
	  (setf h (* y s))
	  (setf y (* y c))
	  (do ((jj 1 (1+ jj)))((> jj n))
	    (setf x (aref11 v jj j))
	    (setf z (aref11 v jj i))
	    (setf (aref11 v jj j) (+ (* x c)(* z s)))
	    (setf (aref11 v jj i) (- (* z c)(* x s))))
	  (setf z (pythag-df f h))
	  (setf (aref1 w j) z)			; rotation can be arbitrary if z=0
	  (when (not (= z 0.0d0))   
	    (setf z (/ 1.0d0 z))
	    (setf c (* f z))
	    (setf s (* h z)))
	  (setf f (+ (* c g)(* s y)))
	  (setf x (- (* c y)(* s g)))
	  (do ((jj 1 (1+ jj)))((> jj m))
	    (setf y (aref11 a jj j))
	    (setf z (aref11 a jj i))
	    (setf (aref11 a jj j) (+ (* y c)(* z s)))
	    (setf (aref11 a jj i) (- (* z c)(* y s)))))
	(setf (aref1 rv1 l) 0.0d0)
	(setf (aref1 rv1 k) f)
	(setf (aref1 w k) x))))
  (values))

(defun svzero-df (w n threshold &optional (report? t))
  "If the relative magnitude of an element in `w' compared to the largest
element is less than `threshold,' then zero that element.  If `report?' is true,
the indices of zeroed elements are printed.  Returns a list of the indices of
zeroed elements.  This routine uses double-floats."
  (let ((max (reduce #'max w)))
    (let ((zeroed-elts nil))
      (dotimes (i n)
	(when (< (/ (aref w i) max) threshold)
	  (push i zeroed-elts)
	  (setf (aref w i) 0.0d0)))
      (setf zeroed-elts (nreverse zeroed-elts))
      (when (and report? zeroed-elts)
	(format t "~&Zeroed variables: ~{ ~d~}.~%" zeroed-elts))
      zeroed-elts)))

(defun svd-inverse-slow-df (u w v &optional
			    (a-1 (make-array (list (length w) (length w))
					     :element-type 'double-float)))
  "Computes the inverse of a matrix that has been decomposed into `u,' `w' and
`v' by singular value decomposition.  It assumes the ``small'' elements of `w'
have already been zeroed.  It computes the inverse by constructing a diagonal
matrix `w2' from `w' (which is just a vector of the diagonal elements, and then
explicitly multiplying u^t w2 and v.  Note that if you are computing the inverse
merely to solve one or more systems of equations, you are better off using the
decomposition and backsubstitution routines directly."
  (let* ((n  (length w))
	 (w2 (make-array (list n n) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i n)
      (let ((x (aref w i)))
	(if (zerop x)
	    (setf (aref w2 i i) 0.0d0)
	    (setf (aref w2 i i) (/ x)))))
    (multiply-matrices
      (multiply-matrices v w2)
      (transpose-matrix u)
      a-1)))

#+test
(defun test-svd-inverse-slow-df (rank &optional (tol 0.001d0))
  "Tests that svdcmp-df works correctly by decomposing A into U,W, and V,
computing the inverse, and multiplying it by A to see if the product is the
identity matrix.  Since the decomposition has already been tested, this function
mostly test the zeroing and matrix operations.  The input is a random square
matrix, with `rank.'"
  (let ((a    (make-array (list rank rank) :element-type 'double-float))
	(u    (make-array (list rank rank) :element-type 'double-float))
	(v    (make-array (list rank rank) :element-type 'double-float))
	(w    (make-array rank :element-type 'double-float))
	(a-1  (make-array (list rank rank) :element-type 'double-float))
	(prod (make-array (list rank rank) :element-type 'double-float)))
    (format t "~&")
    (loop (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((x (random 1000.0d0)))
		(setf (aref a i j) x)
		(setf (aref u i j) x))))
	  (svdcmp-df u rank rank w v)
	  (svzero-df w rank 0.000000d0)
	  (svd-inverse-slow-df u w v a-1)
	  (multiply-matrices a-1 a prod)
	  (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((true (if (= i j) 1.0 0.0))
		    (test (aref prod i j)))
		(unless (< (abs (- true test)) tol)
		  (format t "disagree on elt (~d,~d):  true is ~f, svd => ~f, diff = ~f~%"
			  i j true test (abs (- true test)))))))
	  (format t "done~2%"))))

(defun svd-inverse-fast-df (u w v &optional
			    (a-1 (make-array (list (length w) (length w))
					     :element-type 'double-float))
			    (tmp (make-array (length w) :element-type 'double-float)))
  "Computes the inverse of a matrix that has been decomposed into `u,' `w' and
`v' by singular value decomposition.  It assumes the ``small'' elements of `w'
have already been zeroed.  It computes the inverse by taking advantage of the
known zeros in the full 2-dimensional `w' matrix.  It uses the backsubstitution
algorithm, only with the B vectors fixed at the columns of the identity matrix,
which lets us take advantage of its zeros.  It's about twice as fast as the slow
version and conses a lot less.  Note that if you are computing the inverse
merely to solve one or more systems of equations, you are better off using the
decomposition and backsubstitution routines directly."
  (let ((n (length w)))
    (do ((col 1 (1+ col))) ((> col n))
      (let ((s 0.0d0))
	(do ((j 1 (1+ j)))((> j n))		; calculate u^t b(col)
	  (setf s 0.0d0)
	  (unless (= (aref1 w j) 0.0d0)		; nonzero result only if w(j) is non zero
	    (incf s (aref11 u col j))		;  and for the elt matching `col'
	    (setf s (/ s (aref1 w j))))		; this is the divide by w(j)
	  (setf (aref1 tmp j) s))
	(do ((j 1 (1+ j)))((> j n))		; matrix multiply by v to get answer
	  (setf s 0.0d0)
	  (do ((jj 1 (1+ jj)))((> jj n)) (incf s (* (aref11 v j jj) (aref1 tmp jj))))
	  (setf (aref11 a-1 j col) s))))))

#+test
(defun test-svd-inverse-fast-df (rank &optional (tol 0.000001d0))
  "Tests that svdcmp-df works correctly by decomposing A into U,W, and V,
computing the inverse, and multiplying it by A to see if the product is the
identity matrix.  Since the decomposition has already been tested, this function
tests the inverse-finding function.  The input is a random square matrix, with
`rank.'"
  (let ((a    (make-array (list rank rank) :element-type 'double-float))
	(u    (make-array (list rank rank) :element-type 'double-float))
	(v    (make-array (list rank rank) :element-type 'double-float))
	(w    (make-array rank :element-type 'double-float))
	(tmp  (make-array rank :element-type 'double-float))
	(a-1  (make-array (list rank rank) :element-type 'double-float))
	(prod (make-array (list rank rank) :element-type 'double-float)))
    (format t "~&")
    (loop (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((x #+i (if (= i j) 1.0d0 0.0d0) (random 1000.0d0)))
		(setf (aref a i j) x)
		(setf (aref u i j) x))))
	  (svdcmp-df u rank rank w v)
	  (svzero-df w rank 1d-5 2)
	  (svd-inverse-fast-df u w v a-1 tmp)
	  (multiply-matrices a-1 a prod)
	  (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((true (if (= i j) 1.0d0 0.0d0))
		    (test (aref prod i j)))
		(unless (< (abs (- true test)) tol)
		  (format t "disagree on elt (~d,~d):  true is ~f, svd => ~f, diff = ~f~%"
			  i j true test (abs (- true test)))))))
	  (format t "done~2%"))))


;;;--------------------------------------------------------------------------------
;;;                                       Single Precision
;;;                               SINGULAR VALUE DECOMPOSITION (SVD)
;;;
;;; SVD is the preferred method for inverting nearly singular matrixes.  The
;;; following routines are taken from 'Numerical Recipes in C'.
;;;
;;;--------------------------------------------------------------------------------

(defmacro sign-sf (a b)
  `(if (>= ,b 0.0) (abs ,a) (- (abs ,a))))

(defun pythag-sf (a b)
  "Computes square root of a*a + b*b without destructive overflow or underflow."
  (let ((at (abs a)) (bt (abs b)) (ct 0.0))
    (declare (single-float at bt ct))
    (cond ((> at bt)
	   (setf ct (/ bt at))
	   (* at (sqrt (+ 1.0 (* ct ct)))))
	  ((and (>= bt at) (not (= bt 0.0)))
	   (setf ct (/ at bt))
	   (* bt (sqrt (+ 1.0 (* ct ct)))))
	  (t 0.0))))

(defun svbksb-sf (u w v m n b x &optional
		  (tmp (make-array n :element-type 'single-float)))
  "Solves A X = B for a vector `X,' where A is specified by the mxn array U, `n'
vector W, and nxn matrix V as returned by svdcmp.  `m' and `n' are the
dimensions of `A,' and will be equal for square matrices.  `B' is the 1xm input
vector for the right-hand side.  `X' is the 1xn output solution vector.  All
arrays are of single-floats.  No input quantities are destroyed, so the routine
may be called sequentially with different B's.  See the discussion in Numerical
Recipes in C, section 2.6.

This routine assumes that near zero singular values have already been zeroed.
It returns no values, storing the result in `X.' It does use some auxiliary
storage, which can be passed in as `tmp,' a single-float array of length `n,' if
you want to avoid consing."
  (let ((s 0.0))
    (do ((j 1 (1+ j)))((> j n))		   ; calculate u^t b
      (setf s 0.0)
      (when (not (= (aref1 w j) 0.0))	   ; nonzero result only if w(j) is non zero
	(do ((i 1 (1+ i)))((> i m)) (incf s (* (aref11 u i j) (aref1 b i))))
	(setf s (/ s (aref1 w j))))	   ; this is the divide by w(j)
      (setf (aref1 tmp j) s))
    (do ((j 1 (1+ j)))((> j n))			; matrix multiply by v to get answer
      (setf s 0.0)
      (do ((jj 1 (1+ jj)))((> jj n)) (incf s (* (aref11 v j jj) (aref1 tmp jj))))
      (setf (aref1 x j) s)))
  (values))

(defun svdcmp-sf (a m n w v &optional
		  (rv1 (make-array n :element-type 'single-float)))
  "Given an `m'x`n' matrix `A,' this routine computes its singular value
decomposition, A = U W V^T.  The matrix U replaces `A' on output.  The diagonal
matrix of singular values W is output as a vector `W' of length `n.' The matrix
`V' -- not the transpose V^T -- is output as an `n'x`n' matrix `V.' The row
dimension `m' must be greater or equal to `n'; if it is smaller, then `A' should
be filled up to square with zero rows.  See the discussion in Numerical Recipes
in C, section 2.6.

This routine returns no values, storing the results in `A,' `W,' and `V.' It
does use some auxiliary storage, which can be passed in as `rv1,' a single-float
array of length `n,' if you want to avoid consing.  All input arrays should be
of single-floats."
  (let ((flag 0)(i 0)(l 0)(nm 0)
	(c 0.0)(f 0.0)(h 0.0)(s 0.0)(x 0.0)(y 0.0)(z 0.0)
	(anorm 0.0)(g 0.0)(scale 0.0))
    (declare (fixnum flag i l nm))
    (if (< m n) (error "svdcmp: you must augment a with extra zero rows"))
    ;; householder reduction to bidiagonal form
    (do ((i 1 (1+ i)))((> i n))
      (setf l (+ i 1))
      (setf (aref1 rv1 i) (* scale g))
      (setf g 0.0 s 0.0 scale 0.0)
      (when (<= i m)
	(do ((k i (1+ k)))((> k m)) (incf scale (abs (aref11 a k i))))
	(when (not (= scale 0.0))
	  (do ((k i (1+ k)))((> k m))
	    (setf (aref11 a k i) (/ (aref11 a k i) scale))
	    (incf s (* (aref11 a k i) (aref11 a k i))))
	  (setf f (aref11 a i i))
	  (setf g (- (sign-sf (sqrt s) f)))
	  (setf h (- (* f g) s))
	  (setf (aref11 a i i) (- f g))
	  (when (not (= i n))
	    (do ((j l (1+ j)))((> j n))
	      (setf s 0.0)
	      (do ((k i (1+ k)))((> k m)) (incf s (* (aref11 a k i)
						     (aref11 a k j))))
	      (setf f (/ s h))
	      (do ((k i (1+ k)))((> k m)) (incf (aref11 a k j) (* f
								  (aref11 a k i))))))
	  (do ((k i (1+ k)))((> k m)) (setf (aref11 a k i) (* (aref11 a
								      k i) scale)))))
      (setf (aref1 w i) (* scale g))
      (setf g 0.0 s 0.0 scale 0.0)
      (when (and (<= i m) (not (= i n)))
	(do ((k l (1+ k)))((> k n)) (incf scale (abs (aref11 a i k))))
	(when (not (= scale 0.0))
	  (do ((k l (1+ k)))((> k n))
	    (setf (aref11 a i k) (/ (aref11 a i k) scale))
	    (incf s (* (aref11 a i k)(aref11 a i k))))
	  (setf f (aref11 a i l))
	  (setf g (- (sign-sf (sqrt s) f)))
	  (setf h (- (* f g) s))
	  (setf (aref11 a i l) (- f g))
	  (do ((k l (1+ k)))((> k n)) (setf (aref1 rv1 k) (/ (aref11 a i k) h)))
	  (when (not (= i m))
	    (do ((j l (1+ j)))((> j m))
	      (setf s 0.0)
	      (do ((k l (1+ k)))((> k n)) (incf s (* (aref11 a j k)
						     (aref11 a i k))))
	      (do ((k l (1+ k)))((> k n)) (incf (aref11 a j k) (* s
								  (aref1 rv1 k))))))
	  (do ((k l (1+ k)))((> k n)) (setf (aref11 a i k) (* (aref11 a
								      i k) scale)))))
      (setf anorm (max anorm (+ (abs (aref1 w i)) (abs (aref1 rv1 i))))))
    ;;  accumulation of right hand transformations
    (do ((i n (1- i)))((< i 1))
      (when (< i n)
	(when (not (= g 0.0))
	  (do ((j l (1+ j)))((> j n))	   ; double division to avoid possible underflow
	    (setf (aref11 v j i) (/ (/ (aref11 a i j)(aref11 a i l)) g)))
	  (do ((j l (1+ j)))((> j n)) 
	    (setf s 0.0)
	    (do ((k l (1+ k)))((> k n)) (incf s (* (aref11 a i k) (aref11 v k j))))
	    (do ((k l (1+ k)))((> k n)) (incf (aref11 v k j) (* s
								(aref11 v k i))))))
	(do ((j l (1+ j)))((> j n)) (setf (aref11 v i j) 0.0 (aref11 v j i) 0.0)))
      (setf (aref11 v i i) 1.0)
      (setf g (aref1 rv1 i))
      (setf l i))
    ;;  accumulation of left hand transformations
    (do ((i n (1- i)))((< i 1))
      (setf l (1+ i))
      (setf g (aref1 w i))
      (when (< i n)
	(do ((j l (1+ j)))((> j n)) (setf (aref11 a i j) 0.0)))
      (if (not (= g 0.0))
	  (progn
	    (setf g (/ 1.0 g))
	    (when (not (= i n))
	      (do ((j l (1+ j)))((> j n))
		(setf s 0.0)
		(do ((k l (1+ k)))((> k m)) (incf s (* (aref11 a k i)
						       (aref11 a k j))))
		(setf f (* (/ s (aref11 a i i)) g))
		(do ((k i (1+ k)))((> k m)) (incf (aref11 a k j) (* f
								    (aref11 a k i))))))
	    (do ((j i (1+ j)))((> j m)) (setf (aref11 a j i) (* (aref11 a j i) g))))
	  (progn			   ; else
	    (do ((j i (1+ j)))((> j m)) (setf (aref11 a j i) 0.0))))
      (incf (aref11 a i i)))
    ;;  diagonalization of the bidiagonal form
    (do ((k n (1- k)))((< k 1))		   ;  loop over singular values
      (do ((its 1 (1+ its)))((> its 30))   ;  loop over allowed iterations
	(setf flag 1)
	;; these c hacking turkeys are using forced exit to set the value of l
	(setf l k)
	(loop
	  (if (< l 1) (return))
	  (setf nm (- l 1))			; note that rv1's first element is always zero
	  (when (= (+ (abs (aref1 rv1 l)) anorm) anorm)
	    (setf flag 0)
	    (return))
	  (when (= (+ (abs (aref1 w nm)) anorm) anorm)
	    (return))
	  (decf l))
	(when (not (= flag 0))
	  (setf c 0.0)			   ;  cancellation of first element of rv1, if l > 1
	  (setf s 1.0)
	  (do ((i l (1+ i)))((> i k))
	    (setf f (* s (aref1 rv1 i)))
	    (when (not (= (+ (abs f) anorm) anorm))
	      (setf g (aref1 w i))
	      (setf h (pythag-sf f g))
	      (setf (aref1 w i) h)
	      (setf h (/ 1.0 h))
	      (setf c (* g h))
	      (setf s (- (* f h)))
	      (do ((j 1 (1+ j)))((> j m))
		(setf y (aref11 a j nm))
		(setf z (aref11 a j i))
		(setf (aref11 a j nm) (+ (* y c) (* z s)))
		(setf (aref11 a j  i) (- (* z c) (* y s)))))))
	(setf z (aref1 w k))
	(when (= l k)			   ;  convergence
	  (when (< z 0.0)		   ;  singular value is made nonnegative
	    (setf (aref1 w k) (- z))
	    (do ((j 1 (1+ j)))((> j n)) (setf (aref11 v j k) (- (aref11 v j k)))))
	  (return))
	(when (= its 30) (error "no convergence in 30 svdcmp iterations"))
	(setf x (aref1 w l))		   ;  shiftr from bottom 2-by-2 minor
	(setf nm (- k 1))
	(setf y (aref1 w nm))
	(setf g (aref1 rv1 nm))
	(setf h (aref1 rv1 k))
	(setf f (/ (+ (* (- y z)(+ y z)) (* (- g h) (+ g h))) (* 2.0 h y)))
	(setf g (pythag-sf f 1.0))
	(setf f (/ (+ (* (- x z)(+ x z)) (* h (- (/ y (+ f (sign-sf g f))) h))) x))
					   ; next qr transformation
	(setf c 1.0 s 1.0)
	(do ((j l (1+ j)))((> j nm))
	  (setf i (1+ j))
	  (setf g (aref1 rv1 i))
	  (setf y (aref1 w i))
	  (setf h (* s g))
	  (setf g (* c g))
	  (setf z (pythag-sf f h))
	  (setf (aref1 rv1 j) z)
	  (setf c (/ f z))
	  (setf s (/ h z))
	  (setf f (+ (* x c)(* g s)))
	  (setf g (- (* g c)(* x s)))
	  (setf h (* y s))
	  (setf y (* y c))
	  (do ((jj 1 (1+ jj)))((> jj n))
	    (setf x (aref11 v jj j))
	    (setf z (aref11 v jj i))
	    (setf (aref11 v jj j) (+ (* x c)(* z s)))
	    (setf (aref11 v jj i) (- (* z c)(* x s))))
	  (setf z (pythag-sf f h))
	  (setf (aref1 w j) z)		   ;  rotation can be arbitrary if z=0
	  (when (not (= z 0.0))   
	    (setf z (/ 1.0 z))
	    (setf c (* f z))
	    (setf s (* h z)))
	  (setf f (+ (* c g)(* s y)))
	  (setf x (- (* c y)(* s g)))
	  (do ((jj 1 (1+ jj)))((> jj m))
	    (setf y (aref11 a jj j))
	    (setf z (aref11 a jj i))
	    (setf (aref11 a jj j) (+ (* y c)(* z s)))
	    (setf (aref11 a jj i) (- (* z c)(* y s)))))
	(setf (aref1 rv1 l) 0.0)
	(setf (aref1 rv1 k) f)
	(setf (aref1 w k) x))))
  (values))

#+test
(defun test-svdcmp-sf (rows cols &optional (tol 0.001))
  "Tests that svdcmp-sf works correctly by decomposing A into U,W, and V, and
then multiplying those to see if the product is equal to A.  The input is a
random matrix, with dimensions `rows' and `cols.'"
  (let ((m rows)
	(n cols))
    (let ((a  (make-array (list m n) :element-type 'single-float))
	  (u  (make-array (list m n) :element-type 'single-float))
	  (aa (make-array (list m n) :element-type 'single-float))
	  (v  (make-array (list n n) :element-type 'single-float))
	  (w  (make-array n :element-type 'single-float))
	  (w2 (make-array (list n n) :element-type 'single-float :initial-element 0.0))
	  (rv (make-array n :element-type 'single-float)))
      (format t "~&")
      (loop (dotimes (i (* n m)) 
	      (let ((x (random 1000.0)))
		(setf (row-major-aref a i) x)
		(setf (row-major-aref u i) x)))
	    (svdcmp-sf u m n w v rv)
	    (dotimes (i n) (setf (aref w2 i i) (aref w i)))
	    (multiply-matrices
	      (multiply-matrices u w2)
	      (transpose-matrix v)
	      aa)
	    (dotimes (i m)
	      (dotimes (j n)
		(let ((true (aref a i j))
		      (test (aref aa i j)))
		  (unless (< (abs (- true test)) tol)
		    (format t "disagree on elt (~d,~d):  true is ~f, svd => ~f, diff = ~f~%"
			    i j true test (abs (- true test)))))))
	    (format t "done~2%")))))

(defun svzero-sf (w n threshold &optional (report? t))
  "If the relative magnitude of an element in `w' compared to the largest
element is less than `threshold,' then zero that element.  If `report?' is true,
the indices of zeroed elements are printed.  Returns a list of indices of the
zeroed elements.  This routine uses single-floats."
  (let ((max (reduce #'max w)))
    (let ((zeroed-elts nil))
      (dotimes (i n)
	(when (< (/ (aref w i) max) threshold)
	  (push i zeroed-elts)
	  (setf (aref w i) 0.0)))
      (setf zeroed-elts (nreverse zeroed-elts))
      (when (and report? zeroed-elts)
	(format t "~&Zeroed variables: ~{ ~d~}.~%" zeroed-elts))
      zeroed-elts)))


(defun svd-inverse-slow-sf (u w v &optional
			      (a-1 (make-array (list (length w) (length w))
					       :element-type 'single-float)))
  "Computes the inverse of a matrix that has been decomposed into `u,' `w' and
`v' by singular value decomposition.  It assumes the ``small'' elements of `w'
have already been zeroed.  It computes the inverse by constructing a diagonal
matrix `w2' from `w' (which is just a vector of the diagonal elements, and then
explicitly multiplying u^t w2 and v.  Note that if you are computing the inverse
merely to solve one or more systems of equations, you are better off using the
decomposition and backsubstitution routines directly."
  (let* ((n  (length w))
	 (w2 (make-array (list n n) :element-type 'single-float :initial-element 0.0)))
    (dotimes (i n)
      (let ((x (aref w i)))
	(if (zerop x)
          (setf (aref w2 i i) 0.0)
          (setf (aref w2 i i) (/ x)))))
    (multiply-matrices
     (multiply-matrices v w2)
     (transpose-matrix u)
     a-1)))

#+test
(defun test-svd-inverse-slow-sf (rank &optional (tol 0.001))
  "Tests that svdcmp-sf works correctly by decomposing A into U,W, and V,
computing the inverse, and multiplying it by A to see if the product is the
identity matrix.  Since the decomposition has already been tested, this function
mostly test the zeroing and matrix operations.  The input is a random square
matrix, with `rank.'"
  (let ((a    (make-array (list rank rank) :element-type 'single-float))
	(u    (make-array (list rank rank) :element-type 'single-float))
	(v    (make-array (list rank rank) :element-type 'single-float))
	(w    (make-array rank :element-type 'single-float))
	(a-1  (make-array (list rank rank) :element-type 'single-float))
	(prod (make-array (list rank rank) :element-type 'single-float)))
    (format t "~&")
    (loop (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((x (random 1000.0)))
		(setf (aref a i j) x)
		(setf (aref u i j) x))))
	  (svdcmp-sf u rank rank w v)
	  (svzero-sf w rank 0.0001)
	  (svd-inverse-slow-sf u w v a-1)
	  (multiply-matrices a-1 a prod)
	  (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((true (if (= i j) 1.0 0.0))
		    (test (aref prod i j)))
		(unless (< (abs (- true test)) tol)
		  (format t "disagree on elt (~d,~d):  true is ~f, svd => ~f, diff = ~f~%"
			  i j true test (abs (- true test)))))))
	  (format t "done~2%"))))

(defun svd-inverse-fast-sf (u w v &optional
			    (a-1 (make-array (list (length w) (length w))
					     :element-type 'single-float))
			    (tmp (make-array (length w) :element-type 'single-float)))
  "Computes the inverse of a matrix that has been decomposed into `u,' `w' and
`v' by singular value decomposition.  It assumes the ``small'' elements of `w'
have already been zeroed.  It computes the inverse by taking advantage of the
known zeros in the full 2-dimensional `w' matrix.  It uses the backsubstitution
algorithm, only with the B vectors fixed at the columns of the identity matrix,
which lets us take advantage of its zeros.  It's about twice as fast as the slow
version and conses a lot less.  Note that if you are computing the inverse
merely to solve one or more systems of equations, you are better off using the
decomposition and backsubstitution routines directly."
  (let ((n (length w)))
    (do ((col 1 (1+ col))) ((> col n))
      (let ((s 0.0))
	(do ((j 1 (1+ j)))((> j n))		; calculate u^t b(col)
	  (setf s 0.0)
	  (unless (= (aref1 w j) 0.0)		; nonzero result only if w(j) is non zero
	    (incf s (aref11 u col j))		;  and for the elt matching `col'
	    (setf s (/ s (aref1 w j))))		; this is the divide by w(j)
	  (setf (aref1 tmp j) s))
	(do ((j 1 (1+ j)))((> j n))		; matrix multiply by v to get answer
	  (setf s 0.0)
	  (do ((jj 1 (1+ jj)))((> jj n)) (incf s (* (aref11 v j jj) (aref1 tmp jj))))
	  (setf (aref11 a-1 j col) s))))))

#+test
(defun test-svd-inverse-fast-sf (rank &optional (tol 0.001))
  "Tests that svdcmp-sf works correctly by decomposing A into U,W, and V,
computing the inverse, and multiplying it by A to see if the product is the
identity matrix.  Since the decomposition has already been tested, this function
tests the inverse-finding function.  The input is a random square matrix, with
`rank.'"
  (let ((a    (make-array (list rank rank) :element-type 'single-float))
	(u    (make-array (list rank rank) :element-type 'single-float))
	(v    (make-array (list rank rank) :element-type 'single-float))
	(w    (make-array rank :element-type 'single-float))
	(tmp  (make-array rank :element-type 'single-float))
	(a-1  (make-array (list rank rank) :element-type 'single-float))
	(prod (make-array (list rank rank) :element-type 'single-float)))
    (format t "~&")
    (loop (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((x #+i (if (= i j) 1.0 0.0) (random 1000.0)))
		(setf (aref a i j) x)
		(setf (aref u i j) x))))
	  (svdcmp-sf u rank rank w v)
	  (svzero-sf w rank 0.0001 2)
	  (svd-inverse-fast-sf u w v a-1 tmp)
	  (multiply-matrices a-1 a prod)
	  (dotimes (i rank)
	    (dotimes (j rank)
	      (let ((true (if (= i j) 1.0 0.0))
		    (test (aref prod i j)))
		(unless (< (abs (- true test)) tol)
		  (format t "disagree on elt (~d,~d):  true is ~f, svd => ~f, diff = ~f~%"
			  i j true test (abs (- true test)))))))
	  (format t "done~2%"))))

;;;--------------------------------------------------------------------------------
;;;
;;;                                    SOME SIMPLE ROUTINES BUILT ON SVD 
;;;
;;;--------------------------------------------------------------------------------

(defun singular-value-decomposition (matrix)
  "Returns as three values the U W and V of singular value decomposition.  If
you have already consed up these matrices, you should call `svdcmp-sf' or
`svdcmp-df' directly.  The input matrix is preserved."
  (destructuring-bind (m n) (array-dimensions matrix)
    (let ((type (array-element-type matrix)))
      (let ((u (make-array (list m n) :element-type type))
	    (w (make-array n :element-type type))
	    (v (make-array (list n n) :element-type type)))
	(dotimes (i (* m n))
	  (setf (row-major-aref u i)
		(row-major-aref matrix i)))
	(case type
	  (single-float (svdcmp-sf u m n w v))
	  (double-float (svdcmp-df u m n w v))
	  (t            (svdcmp-sf u m n w v)))
	(values u w v)))))

(defun svd-zero (w &optional (threshold 1.0e-6) (report? t))
  "If the relative magnitude of an element in `w' compared to the largest
element is less than `threshold,' then zero that element.  Returns a list of
indices of the zeroed elements.  This function is just a convenient wrapper for
`svzero-sf' and `svzero-df.'"
  (let ((n (length w)))
    (case (array-element-type w)
      (single-float (svzero-sf w n threshold report?))
      (double-float (svzero-df w n threshold report?))
      (t            (svzero-sf w n threshold report?)))))

(defun svd-back-substitute (u w v b)
  "Returns the solution vector to the Ax=b, where A has been decomposed into
`u,' `w' and `v' by `singular-value-decomposition.' This function is just a
minor wrapping of `svbksb-sf' and `svbksb-df.'"
  (destructuring-bind (m n) (array-dimensions u)
    (let ((type (array-element-type b)))
      (let ((x (make-array n :element-type type))) 
	(case type
	  (single-float (svbksb-sf u w v m n b x))
	  (double-float (svbksb-df u w v m n b x))
	  (t            (svbksb-sf u w v m n b x)))
	x))))

(defun svd-solve-linear-system
       (matrix b-vector &optional (report? t) (threshold 1.0e-6))
  "Returns solution of linear system matrix * solution = b-vector.  Employs the
singular value decomposition method.  See the discussion in Numerical Recipes in
C, section 2.6, especially as to the semantics of `threshold.'"
  (multiple-value-bind (u w v) (singular-value-decomposition matrix)
    (svd-zero w threshold report?)
    (svd-back-substitute u w v b-vector)))

#+test
(defun test-svd-solve-linear-system ()
  (let ((*print-array* t))
    ;; the correct answer is (-1 2)
    (spy (svd-solve-linear-system #2a((1 2) (4 5)) #1a(3 6)))
    ;; This is a singular matrix
    (spy (svd-solve-linear-system #2a((1 2) (4 8)) #1a(3 12)))
    ;; This is that singular matrix without the first column
    (spy (svd-solve-linear-system #2a((2) (8)) #1a(3 12)))
    ))

;;; ============================================================================

(defun svdvar (v w &optional cvm)
  "Given `v' and `w' as computed by singular value decomposition, computes the
covariance matrix among the predictors.  Based on Numerical Recipes in C,
section 15.4, algorithm `svdvar.' The covariance matrix is returned.  It can be
supplied as the third argument."
  (let* ((ma   (length w))
	 (type (array-element-type w))
	 (wti  (make-array ma :element-type type)))
    (unless cvm
      (setf cvm (make-array (list ma ma) :element-type type)))
    (dotimes (i ma)
      (setf (aref wti i) 0.0)
      (let ((wi (aref w i)))
	(unless (zerop wi)
	  (setf (aref wti i) (/ (* wi wi))))))
    (dotimes (i ma)
      (dotimes (j (1+ i))
	(let ((sum (loop for k from 0 below ma sum (* (aref v i k)
						      (aref v j k)
						      (aref wti k)))))
	  (setf (aref cvm i j) sum)
	  (setf (aref cvm j i) sum))))
    cvm))

;;; ============================================================================

(defun svd-matrix-inverse (a &optional (singularity-threshold 1.0d-10))
  "Use singular value decomposition to compute the inverse of `A.' If an exact
inverse is not possible, then zero the otherwise infinite inverted singular
value and compute the inverse.  The inverse is returned; `A' is not destroyed.
If you're using this to solve several systems of equations, you're better off
computing the singular value decomposition and using it several times, because
this function computes it anew each time."
  (let* ((u    (copy-array a))
	 (type (array-element-type a))
	 (m    (array-dimension u 0))
	 (n    (array-dimension u 1))
	 (w    (make-array n :element-type type))
	 (rv   (make-array n :element-type type))
	 (v    (make-array (list n n) :element-type type))
	 (a-1  (make-array (array-dimensions a) :element-type type)))
    (ecase type
      (single-float (svdcmp-sf u m n w v rv)
		    (svzero-sf w n singularity-threshold)
		    (svd-inverse-fast-sf u w v a-1 rv))
      (double-float (svdcmp-df u m n w v rv)
		    (svzero-df w n singularity-threshold)
		    (svd-inverse-fast-df u w v a-1 rv)))
    a-1))

