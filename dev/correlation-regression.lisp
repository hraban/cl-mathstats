;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

$Id: correlation-regression.lisp,v 1.2 2005/06/09 20:30:04 gwking Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: EKSL Group

DISCUSSION

|#
(in-package #:metabang.math)

;;; ---------------------------------------------------------------------------
;;; correlation, various types
;;; ---------------------------------------------------------------------------

(define-statistic correlation () () ()
   ((sample1 'sequence)
    (sample2 'sequence))
   (sample1 sample2 &rest args &key start1 end1 start2 end2)
   "Computes the correlation coefficient of two samples, which should be
equal-length sequences of numbers."
   (check-type sample1 sequence)
   (check-type sample2 sequence)
   (let ((n1 (start/end (data-length sample1) start1 end1))
	 (n2 (start/end (data-length sample2) start2 end2)))
     (unless (= n1 n2)
       (error 'unmatched-sequences))
     (correlation-from-summaries
      n1
      (start/end (reduce #'+ sample1) start1 end1)
      (start/end (reduce #'+ sample1 :key #'square) start1 end1)
      (start/end (reduce #'+ sample2) start2 end2)
      (start/end (reduce #'+ sample2 :key #'square) start2 end2)
      (apply #'inner-product sample1 sample2 args))))
       
;;; ---------------------------------------------------------------------------

(defun correlation-from-summaries (n x x2 y y2 xy)
  "Computes the correlation of two variables given summary statistics of the
variables.  All of these arguments are summed over the variable: `x' is the sum
of the x's, `x2' is the sum of the squares of the x's, and `xy' is the sum of
the cross-products, which is also known as the inner product of the variables x
and y.  Of course, `n' is the number of data values in each variable."
  ;; The computing formula comes from DeVore, page 448, equation 12.18.
  (let ((denom (sqrt (* (- (* n x2) (square x))
			(- (* n y2) (square y))))))
    (when (zerop denom)
      (error 'zero-variance))
    (/ (- (* n xy) (* x y)) denom)))

;;; ---------------------------------------------------------------------------

;;
;; Partial correlations, compliments of Lisa Ballesteros
;;

;;; Calculates the partial correlation, given a from and a to variable and a list of 
;;; other variable(s) to hold constant.
;; STATISTICS.LISP ??

(defun partials-from-parents (from to parents-list)
  (labels ((radical (value)
                    (sqrt (- 1 (square value))))
           (parents-partial (from to parents-list)
             (cond ((null parents-list)
		    (correlation from to))
		   ((null (cdr parents-list))
		    (let ((parent (car parents-list)))
		      (/ (- (correlation from to) (* (correlation from parent) (correlation to parent)))
			 (* (radical (correlation from parent)) (radical (correlation to parent))))))
		   (t (/ (- (parents-partial from to (cdr parents-list))
                            (* (parents-partial from (car parents-list) (cdr parents-list))	
                               (parents-partial to (car parents-list) (cdr parents-list))))
                         (* (radical (parents-partial from (car parents-list) (cdr parents-list)))
                            (radical (parents-partial to (car parents-list) (cdr parents-list)))))))))
    (parents-partial from to (reverse parents-list))))

;;; ---------------------------------------------------------------------------

(defun lagged-correlation (sequence1 sequence2 lag)
  "Returns the correlations of `sequence1' with `sequence2' after
shifting `sequence1' by `lag'.  This means that for all n, element n
of `sequence1' is paired with element n+`lag' of `sequence2', where
both of those elements exist."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (cond
   ((zerop lag)
    (correlation sequence1 sequence2))
   ((plusp lag)
    (correlation sequence1 sequence2 :end1 (- (length sequence1) lag)
		 :start2 lag))
   ((minusp lag)
    (correlation sequence1 sequence2 :start1 (abs lag)
		 :end2 (- (length sequence1) (abs lag))))))

;;; ---------------------------------------------------------------------------

(define-statistic cross-correlation () () ()
  ((sequence1 'sequence)
   (sequence2 'sequence))
  (sequence1 sequence2 max-lag &optional (min-lag 0))
  "Returns a list of the correlation coefficients for all lags from
`min-lag' to `max-lag,' inclusive, where the `i'th list element is the
correlation of the first \(length-of-sequence1 - i\) elements of
sequence1 with with the last i elements of sequence2.  Both sequences
should be sequences of numbers and of equal length."
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (loop for lag from min-lag to max-lag
        collect (lagged-correlation sequence1 sequence2 lag)))

;;; ---------------------------------------------------------------------------

(define-statistic autocorrelation () () ()
  ((sample 'sequence))
  (sample max-lag &optional (min-lag 0))
  "Autocorrelation is merely a cross-correlation between a sample and itself.
This function returns a list of correlations, where the i'th element is the
correlation of the sample with the sample starting at `i.'"
  (cross-correlation sample sample max-lag min-lag))

;;; ---------------------------------------------------------------------------
;;; regression, various types
;;; ---------------------------------------------------------------------------

(defun linear-regression-minimal-summaries (n x y x2 y2 xy)
  "Calculates the slope and intercept of the regression line.  This function
differs from `linear-regression-minimal' in that it takes summary statistics:
`x' and `y' are the sums of the independent variable and dependent variables,
respectively; `x2' and `y2' are the sums of the squares of the independent
variable and dependent variables, respectively; and `xy' is the sum of the
products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (declare (ignore y2))
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSXY (- (* n xy) (* x y))))
    (when (zerop NSSX)
      (error 'zero-variance))
    (let* ((slope         (/ NSSXY NSSX))
	   (intercept     (/ (- y (* slope x)) n)))
      (values slope intercept))))

;;; ---------------------------------------------------------------------------

(defun linear-regression-minimal (dv iv)
  "Calculates the slope and intercept of the regression line.  This function
takes two equal-length sequences of raw data.  Note that the dependent variable,
as always, comes first in the argument list.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-minimal-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

;;; ---------------------------------------------------------------------------

(defun linear-regression-brief-summaries (n x y x2 y2 xy)
  "Calculates the main statistics of a linear regression: the slope and
intercept of the line, the coefficient of determination, also known as r-square,
the standard error of the slope, and the p-value for the regression.  This
function differs from `linear-regression-brief' in that it takes summary
variables: `x' and `y' are the sums of the independent variable and dependent
variables, respectively; `x2' and `y2' are the sums of the squares of the
independent variable and dependent variables, respectively; and `xy' is the sum
of the products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSY  (- (* n y2) (* y y)))
	(NSSXY (- (* n xy) (* x y))))
    (when (or (zerop NSSX) (zerop NSSY))
      (error 'zero-variance))
    (let* ((slope         (/ NSSXY NSSX))
	   (intercept     (/ (- y (* slope x)) n))
	   (NSSR          (* slope NSSXY))
	   (NSSE          (- NSSY NSSR))
	   (determination (/ NSSR NSSY))
	   (dof           (- n 2))
	   (std-err-slope (sqrt (/ NSSE (* dof NSSX))))
	   (p-value       (if (zerop NSSE) 0f0
			      (students-t-significance
			       ;; Need a float here.
			       (coerce (/ slope std-err-slope) 'float)
			       dof :both))))
      (values slope intercept determination std-err-slope p-value))))

;;; ---------------------------------------------------------------------------

(defun linear-regression-brief (dv iv)
  "Calculates the main statistics of a linear regression: the slope and
intercept of the line, the coefficient of determination, also known as r-square,
the standard error of the slope, and the p-value for the regression.  This
function takes two equal-length sequences of raw data.  Note that the dependent
variable, as always, comes first in the argument list.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-brief-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

;;; ---------------------------------------------------------------------------

(defun linear-regression-verbose-summaries (n x y x2 y2 xy)
  "Calculates almost every statistic of a linear regression: the slope and
intercept of the line, the standard error on each, the correlation coefficient,
the coefficient of determination, also known as r-square, and an ANOVA table as
described in the manual.

If you don't need all this information, consider using the ``-brief'' or
``-minimal'' functions, which do less computation.

This function differs from `linear-regression-verbose' in that it takes summary
variables: `x' and `y' are the sums of the independent variable and dependent
variables, respectively; `x2' and `y2' are the sums of the squares of the
independent variable and dependent variables, respectively; and `xy' is the sum
of the products of the independent and dependent variables.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (let ((NSSX  (- (* n x2) (* x x)))
	(NSSY  (- (* n y2) (* y y)))
	(NSSXY (- (* n xy) (* x y))))
    (when (or (zerop NSSX) (zerop NSSY))
      (error 'zero-variance))
    (let* ((slope             (/ NSSXY NSSX))
	   (intercept         (/ (- y (* slope x)) n))
	   (NSSR              (* slope NSSXY))
	   (NSSE              (- NSSY NSSR))
	   (determination     (/ NSSR NSSY))
	   (correlation       (sqrt determination))
	   (dof               (- n 2))
	   (std-err-slope     (sqrt (/ NSSE (* dof NSSX))))
	   (std-err-intercept nil)
	   ;; F = (SSR/1)/(SSE/dof) => (SSR*dof)/SSE => (NSSR*dof)/NSSE
	   (F                 (float (/ (* dof NSSR) NSSE)))
	   (p-value           (f-significance F 1 dof t))
	   (ssr               (/ NSSR n))
	   (sse               (/ NSSE n))
	   (anova-table      `((1 ,ssr ,ssr ,F ,p-value)
			       (,dof ,sse ,(/ sse dof))
			       (,(1+ dof) ,(/ NSSY n)))))
      (values slope intercept determination correlation
	      std-err-slope std-err-intercept anova-table))))

;;; ---------------------------------------------------------------------------

(defun linear-regression-verbose (dv iv)
  "Calculates almost every statistic of a linear regression: the slope and
intercept of the line, the standard error on each, the correlation coefficient,
the coefficient of determination, also known as r-square, and an ANOVA table as
described in the manual.

This function takes two equal-length sequences of raw data.  Note that the
dependent variable, as always, comes first in the argument list.  If you don't
need all this information, consider using the ``-brief,'' or ``-minimal''
functions, which do less computation.

You should first look at your data with a scatter plot to see if a linear model
is plausible.  See the manual for a fuller explanation of linear regression
statistics."
  (check-type dv sequence)
  (check-type iv sequence)
  (let ((ny (length dv))
	(nx (length iv)))
    (when (/= nx ny)
      (error 'unmatched-sequences))
    (linear-regression-verbose-summaries
      nx
      (reduce #'+ iv)
      (reduce #'+ dv)
      (reduce #'+ iv :key #'square)
      (reduce #'+ dv :key #'square)
      (inner-product iv dv))))

;;; ---------------------------------------------------------------------------
;;; The following code is due to David Fisher.  It employs the normal equations
;;; method to do the regression.  Numerical Recipes advocates doing regression
;;; by singular value decomposition.  It has several variants, implemented
;;; below.  -- Anderson 12/9/94

(defun multiple-linear-regression-normal (dv &rest ivs)
  "Performs linear regression of the dependent variable, `dv,' on multiple
independent variables, `ivs.' Y on multiple X's, calculating the intercept and
regression coefficient.  Calculates the F statistic, intercept and the
correlation coefficient for Y on X's."
  (let* ((num-x (length ivs))
	 (rows (+ 2 num-x))
	 (cols (length dv))
	 (x-0 (make-list cols :initial-element 1))
	 (item-list ()))
      
    ;;Missing values must be handled with care, if they are not paired
    ;; the regression may not be valid. Advise deleting rows with missing
    ;; values.
    (when (member 'nil dv)
      (error 'missing-data))

    ;;build the array initial-element list
    (push x-0 item-list)
    (dolist (x ivs)
      ;;As above for the independent variables.
      (when (member 'nil x)
	(error 'missing-data))
      (push x item-list))
    (push dv item-list)
      
    ;;make the sum-squares cross-product arrays
    (let* ((z-trans-mat (make-array 
			 (list rows cols) 
			 :initial-contents (nreverse item-list)))
	   (z-mat (transpose-matrix z-trans-mat))
	   (z-trans-z (multiply-matrices z-trans-mat z-mat))
	   (x-dim (+ num-x 1))
	   (x-trans-x (make-array (list x-dim x-dim)))
	   (x-trans-y (make-array (list x-dim 1)))
	   (y-trans-y (make-array '(1 1))))
	 
      ;;X transpose X SSCP X-1..X-n X-1..X-n 
      (dotimes (i (array-dimension x-trans-x 0))
	(dotimes (j (array-dimension x-trans-x 1))
	  (setf (aref x-trans-x i j)  
	    (aref z-trans-z i j))))
	 
      ;; X transpose Y SSCP X-1..X-N Y
      (dotimes (i (array-dimension x-trans-y 0))
	(dotimes (j (array-dimension x-trans-y 1))
	  (setf (aref x-trans-y i j)  
	    (aref z-trans-z  i (+ j x-dim)))))
	 
      ;; Y transpose Y SSCP Y Y
      (setf (aref y-trans-y 0 0 )  
	(aref z-trans-z  x-dim x-dim))
	 
      ;;Calculate the regression statistics.
      (let ((inv-x-t-x (invert-matrix x-trans-x)))
        (unless inv-x-t-x
          (error 'degenerate-data))
        (let* ((b-mat (multiply-matrices 
		       inv-x-t-x 
		       x-trans-y))
	       (intercept (aref b-mat 0 0))
	       (coefficients '())
	       (x-0 (make-array 
		     (list (array-dimension z-mat 0) 1) 
		     :initial-element 1))
	       (x-0-t (transpose-matrix x-0))
	       (y (make-array 
		   (list (array-dimension z-mat 0) 1))))
	  ;;coefficients of regression for eq. Y = b-1X-1 + b-2X-2 +...+b-nX-n + a
	  ;; where a is the intercept of the equation.
	  (dotimes (i (array-dimension b-mat 0))
	    (unless (= i 0)
	      (push (aref b-mat i 0) coefficients)))
	  (setf coefficients (nreverse coefficients))
	  
	  ;;Raw score array for Y 
	  (dotimes (i (array-dimension z-mat 0))
	    (setf (aref y i 0) 
	          (aref z-mat i x-dim)))
	  
	  (let* ((x-0-y (multiply-matrices x-0-t y))
	         (n (aref z-trans-z 0 0))	;number of observations
	         (b-trans (transpose-matrix b-mat))
	         (b-t-x-t-y (multiply-matrices 
			     b-trans 
			     x-trans-y))
	         (e-t-e (- 
			  (aref y-trans-y 0 0) 
			  (aref 
			    b-t-x-t-y 
			    0 0)))		;Sum of squares of the residual
                 
	         ;;This is a miscalculation, mse-reg should be the
	         ;; ss-REG / df-reg, this is ss-RES / a number that
	         ;; is close to df-res i.e. [n - 1], df-res is [n - 1 - df-reg]
	         #+ignore
	         (mse (/ e-t-e (- n 1)))	;mean square error of the regression
                 
	         (sum-sq-y-dev (- (aref y-trans-y 0 0) 
				  (/ 
				   (square (aref x-0-y 0 0))
				   n)))	;Sum of squared deviations Y
	         (sum-sq-x-dev nil)
	         (s-b (scalar-matrix-multiply (/ e-t-e (- n num-x 1)) inv-x-t-x)) ;error of the b's
	         (var-y (/ 
		         sum-sq-y-dev  
		         (- n 1)))	;variance of y
	         (s-y (sqrt var-y))	;standard deviation of y
	         (s-x nil)
	         (r-list nil)
	         (t-bs nil)
	         (ss-reg-list nil)
	         (ss-percent-list nil)
	         (s-bs nil)
	         (sum-y-sq (square (aref x-trans-y 0 0))) ;sum of Y squared
	         (sum-y-sq-n (/ sum-y-sq  n)) ;sum of Y squared divided by n
					;sum of squares of the regression
	         (ss-reg (- (aref b-t-x-t-y 0 0) sum-y-sq-n ))
					;sum of squares of the residual
	         (ss-res (- sum-sq-y-dev ss-reg))
					;f-statistic for the regression
	         (mse-reg (/ ss-reg num-x))
	         (mse-res (/ ss-res (- cols num-x 1)))
	         (f (/ mse-reg (if (zerop mse-res)
                                 (error "Mean Squared Error of Residual is 0")
                                 mse-res)))
	         (betas nil)
					;R squared, proportion of the variance
					; accounted for by the independent variables.
	         (r-square (/ ss-reg sum-sq-y-dev )))

            ;; when sum of squares of the residual we probably have colinear data
            (when (minusp ss-res)
              (error 'degenerate-data))
	    
	       
	  ;;list the errors of the b's
	  (dotimes (i x-dim)
	    (push (sqrt (aref s-b i i)) s-bs))
	  (setf s-bs (nreverse s-bs))
	       
	  ;;list the sums of squared deviations for each X
	  (dotimes (i x-dim)
	    (unless (= i 0)
	      (push (- (aref x-trans-x i i)
		       (/ (square (aref x-trans-x 0 i)) n))  
		    sum-sq-x-dev )))
	  (setf sum-sq-x-dev (nreverse sum-sq-x-dev))
	       
	  ;;list the standard deviations for each X
	  (dolist (dev sum-sq-x-dev)
	    (push (sqrt (/ dev (- n 1))) s-x ))
	  (setf s-x (nreverse s-x))
	       
	  ;;Calculate and list the standardized coefficients {Betas}
	  (do ((sx s-x (cdr sx))
	       (b coefficients (cdr b)))
	      ((null sx))
	    (push (* (car b) (/ (car sx) s-y)) betas))
	  (setf betas (nreverse betas))
	       
	  ;;list the correlation coefficients for each X and Y
	  (dolist (x ivs)
	    (push (r-score x dv) r-list))
	  
	  ;;list the correlation coefficients for each X and X
	  (dolist (x ivs)
	    (dolist (x-1 ivs)
	      (push (r-score x x-1) r-list)))
	  (setf r-list (nreverse r-list))
	  
	  ;;calculate and list the t-scores for the b's 
	  ;; t > 2 significant at 0.005 level.
	  (dotimes (i num-x 1)
	    (push  (/ (nth i coefficients) (nth (+ i 1) s-bs)) t-bs))
	  (setf t-bs (nreverse t-bs))
	       
	  ;;calculate and list the portion of the sum of squares of the regression due 
	  ;; to each X.
	       
	  '(let ((sum-y 0f0) (sum-z 0f0) (sum-z2 0f0) (sum-zy 0f0) (sum-y2 0f0))
	    (apply #'mapc
		   #'(lambda (y &rest xs)
		       (let ((z (loop for x in xs for c in coefficients
				      sum (* x c) into sum finally (return (+ intercept sum)))))
			 (incf sum-z z)
			 (incf sum-zy (* z y))
			 (incf sum-z2 (* z z))
			 (incf sum-y  y)
			 (incf sum-y2 (* y y))))
		   dv
		   ivs)
	    '(spy ss-reg
		 (- sum-z2 (/ (* sum-z sum-z) n))
		 (sum-of-squares dv)
		 (- sum-y2 (/ (* sum-y sum-y) n))
		 ss-res
		 (+ sum-z2 (- (* 2 sum-zy)) sum-y2)
		 ss-reg
		 (- (- sum-y2 (/ (* sum-y sum-y) n))
		    (+ sum-z2 (- (* 2 sum-zy)) sum-y2))
		 ss-reg
		 (- (- (/ (* sum-y sum-y) n))
		    (+ sum-z2 (- (* 2 sum-zy))))
		 ss-reg
		 (+ (- (/ (* sum-y sum-y) n))
		    (- sum-z2)
		    (* 2 sum-zy))
		 ;; the other side of the equation
		 ss-reg
		 (- sum-zy (/ (* sum-y sum-z) n))
	    ))
	    
	  '(spy (aref x-trans-y 0 0) (reduce #'+ dv))
	  (dotimes (i num-x )
	    '(spy (aref x-trans-x 0 (+ i 1)) (reduce #'+ (nth i ivs))
		  (aref x-trans-y (+ i 1) 0) (reduce #'+ (mapcar #'* (nth i ivs) dv))
		  (- (aref x-trans-y (+ i 1) 0)
		     (/ (* (aref x-trans-y 0 0)
			   (aref x-trans-x 0 (+ i 1)))
			n))
		 (* n (covariance dv (nth i ivs))))
	    (push  (* (nth i coefficients) 
		      (- (aref x-trans-y (+ i 1) 0)  
			 ;;either ref (+ i 1) 0
			 ;; or 0 (+ i 1) 
			 ;; access the same cell on the Explorer.  
			 ;; This is bad.
			 (/ (* (aref x-trans-y 0 0)
			       (aref x-trans-x 0 (+ i 1))) n)))
		   ss-reg-list))
	  (setf ss-reg-list  (nreverse  ss-reg-list))
	  '(spyx (reduce #'+ ss-reg-list))
	  '(spy ss-reg (sum-of-squares dv))
	  '(spy (length ss-reg-list) (length ivs))
	       
	  ;;calculate and list the percentages for the SS of regression. R(XnY)^2 = percent variance due to Xn
	  (dolist (r r-list)
	    (push (square r) ss-percent-list))
	  (setf ss-percent-list  (nreverse  ss-percent-list))
	       
	  (values intercept coefficients r-list t-bs betas 
		  r-square f ss-reg-list  ss-percent-list
		  ss-reg ss-res mse-reg mse-res
		  ;; the following is for compatibility with the SVD algorithm
		  nil)))))))

;;; ---------------------------------------------------------------------------

(defun multiple-linear-regression-arrays (dv &rest ivs)
  "This is an internal function for the use of the multiple-linear-regression
functions.  It takes the lists of values given by CLASP and puts them into a
pair of arrays, A and b, suitable for solving the matrix equation Ax=b, to find
the regression equation.  The values are A and b.  The first column of A is the
constant 1, so that an intercept will be included in the regression model."
  (let ((rows (length dv))
	(cols (1+ (length ivs))))
    ;; We will solve for x the linear matrix equation ax=b.
    (let ((a (make-array (list rows cols)))
	  (b (make-array rows)))
      ;; The predictor for the constant is a one-vector
      (dotimes (i rows)
	(setf (aref a i 0) 1))
      ;; Copy over the rest of the data
      (loop for j from 1
	    for iv in ivs do
	    (loop for i from 0
		  for aij in iv do
		  ;; check for missing data
		  (when (null aij) (error 'missing-data))
		  (setf (aref a i j) aij)))
      ;; Copy the dependent variable
      (loop for i from 0
	    for bi in dv do
	    ;; check for missing data
	    (when (null bi) (error 'missing-data))
	    (setf (aref b i) bi))
      (values a b))))

;;; ---------------------------------------------------------------------------

(defun multiple-linear-regression-minimal (dv &rest ivs)
  "Let m be the number of independent variables, `ivs.'  This function returns
a vector of length m which are the coefficients of a linear equation that best
predicts the dependent variable, `dv,' in the least squares sense.

This function returns the minimal information for a least squares regression
model, namely a list of the coefficients of the ivs, with the constant term
first.  Consider using the sibling functions -brief and -verbose if you want
more information."
  (multiple-value-bind (a b) (apply #'multiple-linear-regression-arrays dv ivs)
    ;; If we were bootstrapping this computation, we would want to open it up
    ;; some, pull out the temporary matrices, and reduce the consing by a *lot*.
    ;; In fact, this computation conses more than in needs to, since
    ;; `svd-solve-linear-system' doesn't destroy A, and, in this case, that
    ;; would be okay.  It's not okay in the other versions of MLR.
    (coerce (svd-solve-linear-system a b nil) 'list)))

;;; ---------------------------------------------------------------------------

(defun multiple-linear-regression-brief (dv &rest ivs)
  "Let m be the number of independent variables, `ivs.' This function returns a
vector of length m which are the coefficients of a linear equation that best
predicts the dependent variable, `dv,' in the least squares sense.  It also
returns, as the second value, the sum of squared deviations of the data from the
fitted model, aka SSE, aka chi-square.  The third value is the number of degrees
of freedom for the chi-square, if you want to test the fit.

This function returns an intermediate amount of information.  Consider using the
sibling functions -minimal and -verbose if you want less or more information."
  (multiple-value-bind (a b) (apply #'multiple-linear-regression-arrays dv ivs)
    (let ((x (svd-solve-linear-system a b nil)))
      (destructuring-bind (rows cols) (array-dimensions a)
	(let ((SSE 0f0))
	  (dotimes (i rows)
	    (let* ((model (loop for j from 0 below cols
				sum (* (aref a i j) (aref x j))))
		   (diff  (- model (aref b i))))
	      (incf SSE (* diff diff))))
	  (values (coerce x 'list) SSE (- rows cols)))))))

;;; ---------------------------------------------------------------------------

(defun multiple-linear-regression-verbose (dv &rest ivs)
  "Let m be the number of independent variables, `ivs.' This function returns
fourteen values:
 1. the intercept
 2. a list of coefficients
 3. a list of correlations of each iv to the dv and to each iv
 4. a list of the t-statistic for each coefficient
 5. a list of the standardized coefficients (betas)
 6. the fraction of variance accounted for, aka r-square
 7. the ratio of MSR (see #12) to MSE (see #13), aka F 
 8. a list of the portion of the SSR due to each iv
 9. a list of the fraction of variance accounted for by each iv
10. the sum of squares of the regression, aka SSR
11. the sum of squares of the residuals, aka SSE, aka chi-square
12. the mean squared error of the regression, aka MSR
13. the mean squared error of the residuals, aka MSE
14. a list of indices of ``zeroed'' independent variables

This function returns a lot of information about the regression.  Consider using
the sibling functions -minimal and -brief if you need less information."
  (multiple-value-bind (x y) (apply #'multiple-linear-regression-arrays dv ivs)
    (multiple-value-bind (u w v) (singular-value-decomposition x)
      (destructuring-bind (n k) (array-dimensions x)
	;; n is the number of data, k is the number of coefficients, including intercept
	(let (zeroed		; a list of the zeroed independent variables
	      Bs		; the solution vector
	      betas		; the standardized coefficients
	      Ts		; the t-statistics for the b's.
	      (SST 0f0)		; total sum of squares
	      (SSE 0f0)		; sum squared error of residuals
	      MSE		; mean squared error of residuals
	      (SSR 0f0)		; sum squares due to regression
	      MSR		; mean square regression
	      R2		; R-squared, the fraction variance accounted for
	      F			; F ratio
	      cov		; covariance matrix (among the coefficients)
	      cor		; correlation matrix (among the variables)
	      ssr-portions)	; portion of SSR for each iv
	  (setf zeroed (svd-zero w 1f-6 nil))
	  
	  ;; The solution vector includes all coefficients and the intercept,
	  ;; which is the zeroth element.
	  (setf Bs (svd-back-substitute u w v y))
	  
	  ;; calculate SSE directly by computing predicted dv, subtracting
	  ;; true dv, squaring and summing.  Also compute SST
	  (let ((sum-y 0f0) (sum-y2 0f0) (sum-z 0f0) (sum-z2 0f0))
	    (dotimes (i n)
	      (let* ((data  (aref y i))
		     (model (loop for j from 0 below k
				  sum (* (aref x i j) (aref Bs j))))
		     (error (- model data)))
		(incf SSE (* error error))
		(incf sum-z model)
		(incf sum-z2 (* model model))
		(incf sum-y  data)
		(incf sum-y2 (* data data))))
	    ;; standard computing formula for summed deviations from the mean:
	    ;; \sum_i(x_i-\bar{x})^2 = \sum {x_i^2} - (\sum x_i)^2/n
	    (setf SST (- sum-y2 (/ (* sum-y sum-y) n)))
	    (setf SSR (- sum-z2 (/ (* sum-z sum-z) n)))
	    '(spyx (+ sse ssr) sst)
	    '(let ((z (loop for i from 0 below n collect
			    (loop for j from 0 below k
				  sum (* (aref x i j) (aref Bs j))))))
	       (spy (* n (covariance z dv))))
	    )
	  
	  ;; MSE is SSE/DOF, and there are k estimated parameters
	  (setf MSE (/ SSE (- n k)))
	  
	  ;; By definition, SST = SSR + SSE
	  (setf SSR (- SST SSE)
		MSR (/ SSR (- k 1))
		R2  (/ SSR SST)
		F   (/ MSR MSE))
	  
	  ;; compute covariance matrix.  Empirically, this seems to be equal
	  ;; to (X'X)^{-1} in the normal equations, where X is the data matrix
	  ;; augmented by a column of ones, also called the Design Matrix.
	  ;; This is surprising, because the covariance matrix should be equal
	  ;; to MSE*(X'X)^{-1}.  For example, see Devore, page 511.
	  (setf cov (svdvar v w))
	  
	  ;; The t statistic for each coefficient (B), testing whether it is
	  ;; non-zero, is B/sd-B, where sd-B is the standard deviation of B.
	  ;; Note the multiplication by MSE, when I thought it should be
	  ;; unnecessary.
	  (setf Ts (loop for i from 0 below k
			 for B = (aref Bs i)
			 for sd-B = (sqrt (* MSE (aref cov i i)))
			 collect (/ B sd-B)))
	  
	  (let ((sd-variables (cons (standard-deviation dv)
				    (mapcar #'standard-deviation ivs))))
	    ;; the sd of the variables is used a lot below
	    
	    ;; The standardized coefficient (beta) is B_i sd(X_i)/sd(Y)
	    (setf betas (loop for i from 1 below k	; skip intercept
			      for B = (aref Bs i)
			      collect (* B (/ (nth i sd-variables) (car sd-variables)))))
	    
	    ;; This is useful for other calculation
	    (setf cor (correlation-matrix dv ivs))
	    
	    ;; Portion of SSR is B_i*N*COV(X_i,Y), and COV(X,Y) = Cor(X,Y)*sd(X)*sd(Y)
	    ;; the preceding statement has been shown to be incorrect.  SDA 12/13/94
	    '(setf ssr-portions
		  (loop for i from 1 below k
			for b = (aref Bs i) collect
			(* b n (aref cor 0 i) (nth i sd-variables) (car sd-variables))))
	    '(spy ssr-portions)
	    (setf ssr-portions
		  (loop for j from 1 below k collect
			(loop for i from 0 below n
			      for xij = (aref x i j)
			      for yi = (aref y i) 
			      sum (* xij yi) into sum-xy
			      sum xij into sum-x
			      sum yi into sum-y
			      finally (return (* (aref Bs j)
						 (- sum-xy (/ (* sum-x sum-y) n)))))))
	    '(spy ssr-portions))
	  
	  ;; Massage our data structures into the form that the caller expects.
	  (let* ((coefficients  (coerce Bs 'list))
		 (intercept     (pop coefficients))
		 (r-scores      (let ((rs nil))
				  (loop for j from 0 below k do
					(loop for i from 1 below k do
					      (push (aref cor i j) rs)))
				  (nreverse rs)))
		 (var-frac-variance (mapcar #'square r-scores)))
	    ;; don't want t for intercept
	    (pop Ts)
	    ;; omit intercept from indices for `zeroed'
	    (do ((l zeroed (cdr l))) ((null l)) (setf (car l) (- (car l) 1)))
	    (values intercept coefficients r-scores Ts betas R2 F ssr-portions
		    var-frac-variance SSR SSE MSR MSE zeroed)))))))

;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

(defun correlation-matrix (dv ivs)
  "Returns a matrix of all the correlations of all the variables.  The dependent
variable is row and column zero."
  (let ((n (1+ (length ivs))))
    (push dv ivs)
    (let ((matrix (make-array (list n n) :element-type 'single-float)))
      (dotimes (i n)
	(setf (aref matrix i i) 1f0))
      (loop for v1 in ivs for i from 0 do
	    (loop for v2 in ivs for j from 0 below i do
		  (let ((r (r-score v1 v2)))
		    (setf (aref matrix i j) r)
		    (setf (aref matrix j i) r))))
      matrix)))
