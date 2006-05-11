(in-package #:metabang.math)

(deftestsuite test-basic-statistics () ())

(deftestsuite test-data-length (test-basic-statistics) ())
(addtest (test-data-length)
  (ensure-same (data-length '(1 2 3)) 3 :test '=))

(addtest (test-data-length)
  (ensure-same (data-length '(1 2 3) :start 1) 2 :test '=))

(addtest (test-data-length)
  (ensure-same (data-length '(1 2 3) :start 1 :end 2) 1 :test '=))

(addtest (test-data-length)
  (ensure-same (data-length '(1 2 3) :end 2) 2 :test '=))

(addtest (test-data-length)
  (ensure-error (data-length '(1 2 3) :start 100) 2 :test '=))

(addtest (test-data-length)
  (ensure-error (data-length '(1 2 3) :end 100) 2 :test '=))

(addtest (test-data-length)
  (ensure-error (data-length '(1 2 3) :start 3 :end 1) 2 :test '=))



;;; --*--
;;; ***************************************************************************
;;; CLASP ERROR CONDITIONS

#+test
(defmacro error-handling (form)
  "This test form just returns the name of the error that was signalled, just to
show that they are properly signalled and captured."
  `(#+Explorer cl:handler-case
    #-Explorer handler-case
    ,form
     (type-error
	      (condition)
		 (declare (ignore condition))
		 ;; (format t "Condition ~s is a subtype of `type-error.'~%" (type-of condition))
		 :type-error)
     (simple-error () :simple-error)
     #+Explorer
     (eh:type-error () 'eh:type-error)
     #+Explorer
     (sys:failed-assertion () 'sys:failed-assertion)
     (no-data () :no-data)
     (insufficient-data () :insufficient-data)
     (zero-variance () :zero-variance)
     ;; Actually, nothing should signal this, because they should be caught earlier.
     (clasp-error () :clasp-error)
     (unmatched-sequences () :unmatched-sequences)
     #+Lucid
     (t (condition)
	;; ``uncaught'' conditions print and their name
	(format t "CONDITION ~S IS NOT EXPLICITLY CAUGHT!~%" (type-of condition))
	#+LET-IT-RIDE
	(error condition))))

;;; ============================================================================
;;; TEST-FUNCTION

#+test
(defun test-function (fname)
  "Call `fname' on a number of different arguments, some legal and some illegal.
Trap errors and report the result."
  (format t "~&Testing ~s~2%" fname)
  (let ((*print-array* t))
    (dolist (args `((a)
		    (())
		    ((nil))
		    ((nil nil))
		    ((1))
		    ((1 2 3 4 5))
		    (#(1 2 3 4 5))
		    ((1 2 3 4 5) :start 2)
		    (#(1 2 3 4 5) :end 3)
		    ((1 2 3 4 5) :start 1 :end 4)
		    (#(1 2 3 4 5) :key 1+)
		    ((1 2 3 4 5) :key -)
		    ("ABCDE" :key char-int)
		    (((w 2) (e 4) (i 1) (r 5) (d 3)) :key cadr)))
      ;; Okay

      (let ((results (multiple-value-list
		     (apply #'(lambda (data &rest other-args)
				(error-handling
				 (apply (symbol-function fname)
					data
					other-args)))
			    args))))
	(if (null (cdr args))
	    (format t "~&~s~55t~{~s ~}~%" (car args) results)
	    (format t "~&~s~35t~{~s ~}~55t~{~s ~}~%"
		    (car args)
		    #-Explorer
		    (cdr args)
		    #+Explorer
		    (mapcar #'(lambda (x)
				(if (functionp x)
				    (ticl:function-name x) x))
			    (cdr args))
		    results))))))

;;; ---------------------------------------------------------------------------

#+test
(test-function 'data-length)

#+test
(test-function 'mean)



#+test
(test-function 'sum-of-squares)



#+test
(test-function 'variance)
#+test
(test-function 'standard-deviation)


#+test
(test-function 'skewness)


#+test
(test-function 'minimum)
#+test
(test-function 'maximum)

#+test
(test-function 'range)


#+test
(defun error-test-quartile ()
  (let ((*print-array* t))
    ;; data constraints
    (spy (error-handling (quantile 'a 0)))
    (spy (error-handling (quantile 'nil 0)))
    (spy (error-handling (quantile '(a) 0)))
    (spy (error-handling (quantile '(a b c) 0)))
    ;; q constraints
    (spy (error-handling (quantile '(1 2 3 4 5) nil)))
    (spy (error-handling (quantile '(1 2 3 4 5) 'a)))
    (spy (error-handling (quantile '(1 2 3 4 5) -1)))
    (spy (error-handling (quantile '(1 2 3 4 5) 3)))))

#+test
(defun test-quartile ()
  (let ((*print-array* t))
    (spy (quantile '(1 2 3 4 5) 0))
    (spy (quantile '(1 2 3 4 5) 1/4))
    (spy (quantile '(1 2 3 4 5) 1/2))
    (spy (quantile '(1 2 3 4 5) 3/4))
    (spy (quantile '(1 2 3 4 5) 1))
    ;; interpolation
    (spy (quantile '(1 2 3 4 5) 1/3))
    (spy (quantile '(1 2 3 4 5) 2/3))
    ;; keyword processing
    (spy (quantile '(1 2 3 4 5) 1/2 :start 2))
    (spy (quantile '(1 2 3 4 5) 1/2 :end 4))
    (spy (quantile '(1 2 3 4 5) 1/2 :start 2 :end 4))
    (spy (quantile "ABCDE" 1/2 :key #'char-int))
    (spy (quantile '#((a . 1) (o . 2) (r . 3) (l . 4) (v . 5))
		   2/3
		   :key #'cdr))))

#+(and test Explorer)
(defun plot-quantile ()
  "This is the example from the manual."
  (loop for q from 0.0 to 1.0 by (/ 128.0)
	collect (list (quantile '(2.0 3.0 5.0 8.0 13.0) q) q) into pts
	finally (plotter:plot-stuff
		  `(((:polyline-without-vertices) . ,pts)))))



#+test
(test-function 'median)
#+test
(defun trimmed-mean-10 (data &rest standard-args)
  (apply #'trimmed-mean data .1 standard-args))

#+test
(test-function 'trimmed-mean-10)


#+test
(defun test-trimmed-mean ()
  (dotimes (i 21)
    (spy (trimmed-mean '(1 2 3 4) (/ i 40))))
  (dotimes (i 21)
    (spy (trimmed-mean '(1 2 3 4 5) (/ i 40))))
  (dotimes (i 21)
    (spy (trimmed-mean '(1.0 2.0 4.0 8.0) (/ i 40))))
  (spy (trimmed-mean '(1.0 2.0 4.0 8.0) .33 :start 2))
  (spy (trimmed-mean '(1.0 2.0 4.0 8.0) .33 :end 3))
  (spy (trimmed-mean '((1.0 a) (2.0 b) (4.0 c) (8.0 d)) .5 :key #'first)))
  
  

#+test
(test-function 'mode)

#+test
(defun test-mode ()
  (spy (mode '(1 1 1 1 1)))
  (spy (mode '(1 1 1 2 2 1 1)))
  (spy (mode '(1 1 1 2 2 4 4 4 4)))
  (spy (mode '(1 1 1 2 2 2 2 4 4)))
  (spy (mode '(1 1 1 2 2 2 2 4 4) :start 3))
  (spy (mode '(1 1 1 2 2 2 2 4 4) :end 5))
  (spy (mode '(1 1 1 2 2 2 2 4 4) :start 6))
  (spy (mode '(foo bar bar baz baz baz quux quux quux quux) :key #'sxhash)))


#+test
(defun test-mode-for-continuous-data (&optional window)
  (spy (mode-for-continuous-data '(1 1 1 1 1)         :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 1 1)     :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 4 4 4 4) :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 2 2 4 4) :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 2 2 4 4) :start 3 :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 2 2 4 4) :end 5 :window window))
  (spy (mode-for-continuous-data '(1 1 1 2 2 2 2 4 4) :start 6 :window window))
  (spy (mode-for-continuous-data '(1 1 10 100 100 100 100 1000 10000) :key #'log :window window)))

#+TEST
(smart-mode (sample-normal-to-list 0 1 100))
#+test
(defun test-multiple-modes ()
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 1))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 2))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 3))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 4))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 5))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 2 :start 3))
  (spy (multiple-modes '(1 2 2 3 3 3 4 4 4 4) 2 :end 6))
  (spy (multiple-modes '(foo bar bar baz baz baz quux quux quux quux) 2 :key #'sxhash)))

#+test
(test-function 'interquartile-range)
#+test
(test-function 'tukey-summary)

#+test
(defun test-tukey-summary ()
  (spy (tukey-summary '(1 2 3 4 5 6 7 8 9)))
  (spy (tukey-summary '(1 2 3 4 5 6 7 8 9 10))))


#+test
(test-function 'statistical-summary)
#+test
(defun test-t-test-one-sample ()
  "The following data is drawn from example 7.8, page 262, from Probability and
Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct t and does not reject H0."
  (spy (t-test-one-sample '(27.2 29.3 31.5 28.7 30.2 29.6) :negative 30))
  (spy (error-handling (t-test-one-sample 'a :both)))
  (spy (error-handling (t-test-one-sample '() :both)))
  (spy (error-handling (t-test-one-sample '(nil) :both)))
  (spy (error-handling (t-test-one-sample '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test-one-sample '(1 2 3 4 5) :both))))


#+test
(defun test-t-test ()
  ;; These first examples check that the basic processing is okay.
  (spy (error-handling (t-test '() '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(nil nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1 nil 1 1 1 1) '(2 2 2 2 2) :both)))
  (spy (error-handling (t-test '(1 1) '(2 2) :both)))
  ;; The first two should be highly significant, the last highly insignificant
  (spy (t-test '(11 12 13) '(1 2 3) :both))
  (spy (t-test '(11 12 13) '(1 2 3) :positive))
  (spy (t-test '(11 12 13) '(1 2 3) :negative))
  (spy (t-test '(11 12 13) '(1 2 3) :both  9))
  (spy (t-test '(11 12 13) '(1 2 3) :both 10))
  (spy (t-test '(11 12 13) '(1 2 3) :both 11))
  (spy (t-test '(11 12 13) '(1 2 3) :positive  9))
  (spy (t-test '(11 12 13) '(1 2 3) :positive 10))
  (spy (t-test '(11 12 13) '(1 2 3) :positive 11))
  (spy (t-test '(11 12 13) '(1 2 3) :negative  9))
  (spy (t-test '(11 12 13) '(1 2 3) :negative 10))
  (spy (t-test '(11 12 13) '(1 2 3) :negative 11))
  )

#+test
(defun test-d-test ()
  ;; These first examples check that the basic processing is okay.
  (spy (error-handling (d-test '() '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(nil nil) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1 nil 1 1 1 1) '(2 2 2 2 2) :both)))
  (spy (error-handling (d-test '(1 1) '(2 2) :both))))

#+test
(defun compare-d-test-to-t-test ()
  ;; We don't expect equality of the p-values, but similarity
  (let ((group1 '(4 5 6))
	(group2 '(1 2 3))
	tt t-sig dd d-count)
    (format t "~2&")
    '(dolist (tails '(:both :positive :negative))
      (dolist (h0mean '(0 2 3 4))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))
    '(dolist (tails '(:positive))
      (dolist (h0mean '(1 2 3 4 5))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))
    (dolist (tails '(:both))
      (dolist (h0mean '(1 2 3 4 5 6 7))
	(multiple-value-setq (tt t-sig) (t-test group1 group2 tails h0mean))
	(multiple-value-setq (dd d-count) (d-test group1 group2 tails :h0mean h0mean :times 1000))
	(format t "tails = ~s; h0mean = ~s~%t-test sig = ~5,3f~%d-test sig = ~5,3f (~d/1000)~2%"
		tails h0mean
		t-sig (/ d-count 1000.0) d-count)))))

#+(and test Explorer)
(defun time-d-test ()
  (let ((s1 (loop repeat 20 collect (random 10.0)))
	(s2 (loop repeat 20 collect (+ 2.0 (random 10.0)))))
    (time:timeit (:cpu :cons :number-cons)
      (t-test s1 s2 :both))
    (time:timeit (:cpu :cons :number-cons)
      (d-test s1 s2 :both))))

#+test
(defun test-t-test-matched ()
  "The following data is drawn from example 8.8, page 297, from Probability and
Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct t and does not reject H0."
  (spy (t-test-matched
	 '(30.99 31.47 30.00 30.64 35.25 30.62 31.91 31.37 13.22 21.14 27.21 28.27 29.75 24.90 27.86 31.33)
	 '(30.05 31.75 28.50 31.18 35.12 30.55 31.88 31.05 12.97 21.92 27.26 28.14 30.05 25.10 27.72 31.30)
	 :both)))

#+test
(defun test-inner-product ()
  (let ((*print-array* t))
    (spy (inner-product '(1 2 3) '(1 2 3)))
    (spy (inner-product '#(1 2 3) '(1 2 3)))
    (spy (inner-product '(1 2 3) '#(1 2 3)))
    (spy (inner-product '#(1 2 3) '#(1 2 3)))
    (spy (inner-product '(1 2 3) '(1 2 3) :start1 1 :end1 3 :start2 0 :end2 3))
    (spy (inner-product '#(1 2 3) '(1 2 3) :start1 1 :end1 3 :start2 0 :end2 3))
    (spy (inner-product '(1 2 3) '#(1 2 3) :start1 0 :end1 2 :start2 0 :end2 3))
    (spy (inner-product '#(1 2 3) '#(1 2 3) :start1 0 :end1 2 :start2 0 :end2 3)))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1)))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :end1 4))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :end1 4))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start2 1))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :start2 2))
  (spy (inner-product '(1 2 3 4 5) '(1 1 1 1 1) :start1 1 :end2 4)))

#+test
(defun test-correlation ()
  "The following data is drawn from example 12.10, page 449, from Probability
and Statistics for Engineering and the Sciences, by Jay L.  DeVore, published by
Brooks/Cole Publishing Company, copyright 1982.  The function above computes the
correct correlation."
  (spy (correlation '(1.98 1.44 2.02 1.20 1.57 1.82 1.45 1.80)
		    '(5.6  7.7  8.8  5.1  6.8  3.9  4.5  5.8)))
  (spy (error-handling (correlation 'a '(2 3 4 5))))
  (spy (error-handling (correlation '() '(2 3 4 5))))
  (spy (error-handling (correlation '(nil) '(2 3 4 5))))
  (spy (error-handling (correlation '(nil nil) '(2 3 4 5))))
  (spy (error-handling (correlation '(1 1 1 1) '(2 3 4 5)))))

#+test
(defun test-cross-correlation ()
  (let ((a1 (make-array 100))
	(a2 (make-array 100)))
    (dotimes (i 100)
      (setf (aref a1 i) i)
      (setf (aref a2 i) i))
    (format t "~%~{~f~%~}" (cross-correlation a1 a2 40))
    #+Explorer
    (time:timeit (:cpu :cons)
      (cross-correlation a1 a2 1)))
  (let ((a1 (make-list 100))
	(a2 (make-list 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i)
      (setf (elt a2 i) i))
    (format t "~%~{~f~%~}" (cross-correlation a1 a2 40))
    #+Explorer
    (time:timeit (:cpu :cons)
      (cross-correlation a1 a2 1))))



#+test
(defun test-autocorrelation ()
  (let ((a1 (make-array 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i))
    (spy (autocorrelation a1 40)))
  (let ((a1 (make-list 100)))
    (dotimes (i 100)
      (setf (elt a1 i) i))
    (spy (autocorrelation a1 40))))

#+test
(defun test-confidence-interval-z ()
  (spy (confidence-interval-z '(1 2 3 4 5 6) .9))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .95))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .99))
  (spy (confidence-interval-z '(1 2 3 4 5 6) .999))
  #+Explorer
  (progn
    (format t "~2&mean~%")
    (time:timeit (:cpu :cons)
		 (mean '(1 2 3 4 5 6))
		 (mean '(1 2 3 4 5 6))
		 (mean '(1 2 3 4 5 6))
		 (mean '(1 2 3 4 5 6)))
    (format t "~2&variance~%")
    (time:timeit (:cpu :cons)
		 (variance '(1 2 3 4 5 6))
		 (variance '(1 2 3 4 5 6))
		 (variance '(1 2 3 4 5 6))
		 (variance '(1 2 3 4 5 6)))
    (format t "~2&standard deviation~%")
    (time:timeit (:cpu :cons)
		 (standard-deviation '(1 2 3 4 5 6))
		 (standard-deviation '(1 2 3 4 5 6))
		 (standard-deviation '(1 2 3 4 5 6))
		 (standard-deviation '(1 2 3 4 5 6)))
    (format t "~2&confidence interval-z--cached~%")
    (time:timeit (:cpu :cons)
		 (confidence-interval-z '(1 2 3 4 5 6) .9)
		 (confidence-interval-z '(1 2 3 4 5 6) .95)
		 (confidence-interval-z '(1 2 3 4 5 6) .99)
		 '(confidence-interval-z '(1 2 3 4 5 6) .999))
    (format t "~2&confidence interval-z--computed~%")
    (time:timeit (:cpu :cons)
		 (confidence-interval-z '(1 2 3 4 5 6) .9)
		 (confidence-interval-z '(1 2 3 4 5 6) .95)
		 (confidence-interval-z '(1 2 3 4 5 6) .99)
		 (confidence-interval-z '(1 2 3 4 5 6) .999))))

#+test
(defun test-confidence-interval-t ()
  ;; From DeVore, page 335
  (spy (confidence-interval-t
	 '(26.7 25.8 24.0 24.9 26.4 25.9 24.4 21.7 24.1 25.9 27.3 26.9 27.3 24.8 23.6)
	 .95))
  (dolist (n '(10 20 30 50 70 90 120 200))
    (let ((data (loop for i from 1 to n collect (random 1.0)))
	  mean z-low z-high t-low t-high)
      (multiple-value-setq (mean z-low z-high) (confidence-interval-z data .95))
      (multiple-value-setq (mean t-low t-high) (confidence-interval-t data .95))
      (format t "~&dof: ~3d z-width: ~6f t-width: ~6f~%"
	      n (- z-high z-low) (- t-high t-low)))))

#+test
(defun test-confidence-interval-proportion ()
  ;; From DeVore, page 331
  (spy (confidence-interval-proportion 184 244 .90)))

#+test
(defun test-anova-one-way-variables ()
  (spy (error-handling (anova-one-way-variables 'a nil)))
  (spy (error-handling (anova-one-way-variables '() 'b)))
  (spy e(rror-handling (anova-one-way-variables '(a a) '(nil nil))))
  (spy (error-handling (anova-one-way-variables '(a b) '(1 2 3))))
  (spy (error-handling (anova-one-way-variables '(a b) '(1 2))))
  (spy (error-handling (anova-one-way-variables '(a a b b) '(1 1 2 2))))
  )


#+test
(defun test-two-variables->groups ()
  (spy (two-variables->groups '(a a a b b b c c c c)
			      '(1 2 3 4 5 6 7 8 9 0)))
  (spy (two-variables->groups '(a a a)
			      '(1 2 3)))
  (spy (two-variables->groups '(a a a b)
			      '(1 2 3 4))))

#+test
(defun test-anova-one-way-groups ()
  (spy (error-handling (anova-one-way-groups 'a)))
  (spy (error-handling (anova-one-way-groups '())))
  (spy (error-handling (anova-one-way-groups '(a a))))
  (spy (error-handling (anova-one-way-groups '(() ()))))
  (spy (error-handling (anova-one-way-groups '((a) (b)))))
  (spy (error-handling (anova-one-way-groups '((nil nil) (a a)))))
  (spy (error-handling (anova-one-way-groups '((1 1) (2 2))))))

#+test
(defun test-print-anova-table ()
  (let ((table (anova-one-way-groups '((1 2 3) (11 12 13)) nil)))
    (print-anova-table table)))

#+test
(defun test-scheffe-tests ()
  ;; From Cohen
  (let ((means '(4.241 3.754 2.847 2.345))
	(sizes '(29 118 59 59)))
    (spy (scheffe-tests means sizes 1.811 261))))

#+test
(defun test-print-scheffe-table ()
  ;; From Cohen
  (let ((means '(4.241 3.754 2.847 2.345))
	(sizes '(29 118 59 59)))
    (print-scheffe-table (scheffe-tests means sizes 1.811 261))
    (print-scheffe-table (scheffe-tests means sizes 1.811 261)
			 means)))

#+test
(defun test-anova-one-way ()
  (spy (t-test '(1 2 3) '(11 12 13) :both))
  (spy (anova-one-way-groups '((1 2 3) (11 12 13))))
  (spy (anova-one-way-variables '(a a a b b b) '(1 2 3 11 12 13)))
  ;; From DeVore, example 10.1---equal cell sizes.
  (macrolet ((test (call)
	       `(multiple-value-bind (atab means stab sst-alt ci) ,call
		  ;(y-or-n-p "go on?")
		  (format t "~&sst-alt is~:[not~;~] equal:  ~s vs ~s~%"
			  (= sst-alt (second (third atab))) (second (third atab)) sst-alt)
		  (print-anova-table atab)
		  (spy means)
		  (print-scheffe-table stab)
		  (spy ci))))
    ;; with arrays of integers
    (test (anova-one-way-groups
	    '#(#(187 211 179 195 221 183)
	       #(199 176 182 200 169 175)
	       #(186 203 217 197 209 208)
	       #(191 189 184 188 177 205))
	    t 0.9))
    ;; with lists of floats
    (test (anova-one-way-groups
	    '((18.7 21.1 17.9 19.5 22.1 18.3)
	      (19.9 17.6 18.2 20.0 16.9 17.5)
	      (18.6 20.3 21.7 19.7 20.9 20.8)
	      (19.1 18.9 18.4 18.8 17.7 20.5))
	    t 0.9))
    ;; with arrays of integers
    (test (anova-one-way-variables
	    '#(a a a a a a b b b b b b c c c c c c d d d d d d)
	    '#(187 211 179 195 221 183
		   199 176 182 200 169 175
		   186 203 217 197 209 208
		   191 189 184 188 177 205)
	    t 0.9))
    ;; with lists of floats
    (test (anova-one-way-variables	
	    '(a a a a a a b b b b b b c c c c c c d d d d d d)
	    '(18.7 21.1 17.9 19.5 22.1 18.3
		   19.9 17.6 18.2 20.0 16.9 17.5
		   18.6 20.3 21.7 19.7 20.9 20.8
		   19.1 18.9 18.4 18.8 17.7 20.5)
	    t 0.9))
    ))
#+test
(defun test-anova-two-way-groups ()
  ;; example 11.4 from Devore
  (let* ((data  '#(10.5 9.2 7.9 12.8 11.2 13.3 12.1 12.6 14.0 10.8 9.1 12.5
			8.1 8.6 10.1 12.7 13.7 11.5 14.4 15.4 13.7 11.3 12.5 14.5
			16.1 15.3 17.5 16.6 19.2 18.5 20.8 18.0 21.0 18.4 18.9 17.2))
	 (data3 (make-array '(3 4 3) :displaced-to data)))
    (loop for i from 0 below 3 do
	  (loop for j from 0 below 4 do
		(loop for k from 0 below 3 do (format t "~4,1f " (aref data3 i j k)))
		(format t " | "))
	  (format t "~%"))
    (let ((table (anova-two-way-groups data3)))
      (print-anova-table table))))

#+test
(defun test-anova-two-way-variables-unequal-cell-sizes-1 ()
  (let ((iv1 (variable-value (get-variable 'categoricalws (get-dataset 'pa-scaled-success=1))))
        (iv2 (variable-value (get-variable 'plan1 (get-dataset 'pa-scaled-success=1))))
        (dv  (variable-value (get-variable 'sd (get-dataset 'pa-scaled-success=1)))))
    (multiple-value-bind (table ab-matrix row-totals column-totals grand-total a-list b-list cell-counts)
        (anova-two-way-variables-unequal-cell-sizes-internal dv iv1 iv2)
      (print-anova-table table)
      (print-cells ab-matrix row-totals column-totals grand-total a-list b-list cell-counts))))



#+test
;; This is from Olson, p. 657
(defun test-anova-two-way-variables-unequal-cell-sizes-2 ()
  (let ((iv1 '(task1 task1 task1 task1 task1 task2 task2 task2 task2 task2 task2))
        (iv2 '(male male female female female male male male female female female))
        (dv  '(4    6     8     9       10    2     3    4    10       11  12)))
    (multiple-value-bind (table ab-matrix row-totals column-totals grand-total a-list b-list cell-counts)
        (anova-two-way-variables-unequal-cell-sizes dv iv1 iv2)
      (print-anova-table table)
      (print-cells ab-matrix row-totals column-totals grand-total a-list b-list cell-counts))))

;;; ---------------------------------------------------------------------------

#+test
(defun test-anova-two-way-variables-unequal-cell-sizes-3 ()
  (let ((iv2 '(10   10  10  20   30   40   10  10  10   20   20   20   30   30   30   40   40   40   30   30   30   40   10   10   10   20   20   20   30   30   30   40))
        (iv1 '(H    H   H   H    H    H    IFE IFE IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  IFE  P    P    P    P    P    P    P    P    P    P))
        (dv  '(10.5 9.2 7.9 12.8 14.0 12.5 8.1 8.6 10.1 12.7 13.7 11.5 14.4 15.4 13.7 11.3 12.5 14.5 14.4 15.4 13.7 14.5 16.1 15.3 17.5 16.6 19.2 18.5 20.8 18.0 21.0 18.4)))
    (multiple-value-bind (table ab-matrix row-totals column-totals grand-total a-list b-list cell-counts)
        (anova-two-way-variables-unequal-cell-sizes dv iv1 iv2)
      (print-anova-table table)
      (print-cells ab-matrix row-totals column-totals grand-total a-list b-list cell-counts)
      )))


;;; ---------------------------------------------------------------------------

#+test
(defun anova-two-way-variables-by-hand (dv iv1 iv2)
  (let (num-a num-b
	a-list b-list
        (iv1-hash (make-hash-table)))
    ;;accumulate cell summations
    ;; in the following a refers to columns, b to rows
    ;; put each value in a nested hash table, selected by a and b
    ;; the cells contain the sum of elements and the number of elements
    (loop
     for a in iv1
     for b in iv2
     for c in dv
     with iv2-hash
     do
     (setf iv2-hash (gethash a iv1-hash))
     (when (null iv2-hash)
       (setf iv2-hash (make-hash-table))
       (setf (gethash a iv1-hash) iv2-hash))
     (let ((cell (gethash b iv2-hash)))
       (when (null cell)
	 (setf cell (make-list 2 :initial-element 0))
	 (setf (gethash b iv2-hash) cell))
       (incf (first cell) c)
       (incf (second cell))))

     ;; get the number of rows and columns and build the column-list
     (let ((first-cell (gethash (car iv1) iv1-hash)))
       (setf num-a (hash-table-count iv1-hash))
       (setf num-b (hash-table-count first-cell))
       (maphash #'(lambda (key value)
		    (declare (ignore value))
		    (push key a-list))
	        iv1-hash)
       (maphash #'(lambda (key value)
		    (declare (ignore value))
		    (push key b-list))
	        first-cell))
     
     (setf a-list (sort a-list #'string-lessp :key #'(lambda (x) (if (symbolp x) (symbol-name x) (format nil "~a" x)))))
     (setf b-list (sort b-list #'string-lessp :key #'(lambda (x) (if (symbolp x) (symbol-name x) (format nil "~a" x)))))
     (let ((ab-matrix (make-array `(,(1+ num-b) ,(1+ num-a)) :initial-element 0))
           (cell-sums (make-array `(,(1+ num-b) ,(1+ num-a)) :initial-element 0))
           (cell-counts (make-array `(,(1+ num-b) ,(1+ num-a)) :initial-element 0))
	   (harmonic-denominator 0)
           (within-groups-subtractand 0))
       
       ;; Build the matrix by visiting each cell in each hash table and 
       ;; calculating the means, at the same time, calculate the
       ;; harmonic mean of the sample sizes.
       ;; The harmonic mean is given by the formula
       ;; ab/sum\{1/sij\} where ab is the number of rows times the number
       ;; of columns and sij is the number of elements in cell i,j
       ;; the within-groups-subtractand is calculated here because the
       ;; cell counts are discarded after the ab-matrix is calculated and
       ;; sij is in the denominator of the subtractand for the SS within
       ;; groups. (at least they used to be)

       (loop for a-key in a-list
             for a-count from 0
             for a-value = (gethash a-key iv1-hash) do
	     (when (/= num-b (hash-table-count a-value))
	       (error 'missing-cell))
             (loop for b-key in b-list
                   for b-count from 0
                   for (b-sum b-size) = (gethash b-key a-value) do
	           (setf (aref ab-matrix b-count a-count)
	                 (/ b-sum b-size))
		   ;; Testing
                   (setf (aref cell-sums b-count a-count) b-sum)
                   (setf (aref cell-counts b-count a-count) b-size)
                                            
	           (incf harmonic-denominator (spy (/ 1 b-size)))
	           (incf within-groups-subtractand
		         (/ (square b-sum)
		            b-size))))
       
       (let* ((ab (* num-a num-b))
              (harmonic-mean (/ ab harmonic-denominator))
              (sum-squared-cells 0)
              (row-totals (make-list num-b :initial-element 0))
              (column-totals (make-list num-a :initial-element 0)))

         (spy ab harmonic-denominator)
         (spy harmonic-mean)
         ;; Calculate the row and column totals, the sum-squared-cells
         ;; and while we're at it compute the cell counts.
	 (dotimes (a num-a)
	   (dotimes (b num-b)
	     (let ((cell (aref ab-matrix b a)))
	       (incf (nth b row-totals) cell)
	       (incf (nth a column-totals) cell)
	       (incf sum-squared-cells (square cell)))
	     ;; Testing
	     (let ((cell-count (aref cell-counts b a)))
	       ;; Put the totals in the fringe of the array
	       (incf (aref cell-counts b num-a) cell-count)
	       (incf (aref cell-counts num-b a) cell-count)
	       (incf (aref cell-counts num-b num-a) cell-count))
             (let ((cell-sum (aref cell-sums b a)))
	       ;; Put the totals in the fringe of the array
	       (incf (aref cell-sums b num-a) cell-sum)
	       (incf (aref cell-sums num-b a) cell-sum)
	       (incf (aref cell-sums num-b num-a) cell-sum))))
         
         (let* ((grand-total (reduce #'+ row-totals))
                (squared-row-totals (reduce #'+ (mapcar #'square row-totals)))
                (squared-column-totals (reduce #'+ (mapcar #'square column-totals)))
                (squared-grand-total (square grand-total)))

           (format t "~%AB Matrix (Sums)~%")
	   (print-cells cell-counts nil nil nil a-list b-list cell-sums nil)
	   (format t "~%AB Matrix (Means)~%")
	   (print-cells ab-matrix row-totals column-totals grand-total a-list b-list)
           (terpri)
	   (spy (mapcar #'square column-totals) (mapcar #'square row-totals))
           (spy sum-squared-cells grand-total)
           ;; compute the anova values from the summaries caculated above and
           ;; return them in an anova table-like list.
           (let* ((ssa (spy (* harmonic-mean (- (/ squared-column-totals num-b)
				                     (/ squared-grand-total ab)))))
	          (ssb (spy (* harmonic-mean (- (/ squared-row-totals num-a)
				                (/ squared-grand-total ab)))))
	          (ssaxb (spy (* harmonic-mean
		                 (+ (- sum-squared-cells
			               (/ squared-column-totals num-b)
			               (/ squared-row-totals num-a))
			            (/ squared-grand-total ab)))))
                  (abs2 (reduce #'+ (mapcar #'square dv)))
	          (sse (- abs2 within-groups-subtractand))
	          (dfa (- num-a 1))
	          (dfb (- num-b 1))
	          (dfaxb (* dfa dfb))
	          (dfe (- (length dv) ab))
	          (msa (/ ssa dfa))
	          (msb (/ ssb dfb))
	          (msaxb (/ ssaxb dfaxb))
	          (mse (/ sse dfe))
	          (fa (/ msa mse))
	          (fb (/ msb mse))
	          (faxb (/ msaxb mse))
	          (pa (f-significance (float fa) dfa dfe t))
	          (pb (f-significance (float fb) dfb dfe t))
	          (paxb (f-significance (float faxb) dfaxb dfe))
	          (dft (+ dfa dfb dfaxb dfe))
	          (sst (+ ssa ssb ssaxb sse)))
	     (print-anova-table `((,dfaxb ,SSAxB ,MSAxB ,FAxB ,pAxB)
		                  (,dfa ,SSA ,MSA ,FA ,pA)
		                  (,dfb ,SSB ,MSB ,FB ,pB)
		                  (,dfe ,SSE ,MSE)
		                  (,dfT ,SST)))))))))



#+test
(defvar *keppel-17* nil)
                     
#+test
(defun anova-by-hand ()
  (unless *keppel-17*
    (setf *keppel-17*
          (import-dataset "~eksl/systems/clasp/clasp/data/keppel-17-6.data"
			  :separator #\space :include-labels-p t)))
  (let ((iv1 (variable-value (get-variable 'task *keppel-17*)))
        (iv2 (variable-value (get-variable 'sex *keppel-17*)))
        (dv  (variable-value (get-variable 'time *keppel-17*))))
    (anova-two-way-variables-by-hand dv iv1 iv2)))

#+test
(defvar *pa-scaled-success* nil)

#+test
(defun anova-by-hand-2 ()
  (unless *pa-scaled-success*
    (setf *pa-scaled-success*
          (import-dataset "~eksl/systems/clasp/clasp/data/pa-scaled-success=1.data"
			  :separator #\space :include-labels-p t)))
  (let ((iv1 (variable-value (get-variable 'categoricalws *pa-scaled-success*)))
        (iv2 (variable-value (get-variable 'plan1 *pa-scaled-success*)))
        (dv (variable-value (get-variable 'sd *pa-scaled-success*))))
    (anova-two-way-variables-by-hand dv iv1 iv2)))

#+test
(defun print-cells (ab-matrix row-totals column-totals grand-total a-list b-list &optional cell-counts totals)
  (flet ((write-num (n &optional (stream *standard-output*))
          (if (integerp n)
	      (format stream "~6d  " n)
	      (format stream "~6,2f  " n))))
    (format t "~&~10T ~{~6a~^  ~}Totals~%" a-list)
    (let ((num-columns (length a-list))
	  (num-rows (length b-list)))
      (loop for b in b-list
	    for row from 0 do
	    (format t "~&~7a" b)
	    (loop for a in a-list
		  for col from 0 do
		  (write-num (aref ab-matrix row col)))
            (when row-totals
	      (write-num (elt row-totals row)))
            (when cell-counts
	      (format t "~&~7T" "")
	      (loop for a in a-list
		    for col from 0 do
		    (write-num (aref cell-counts row col)))
              (when totals
	        (write-num (aref cell-counts row num-columns)))))
      (when column-totals
        (format t "~&~7T")
        (loop for ctotal in column-totals do
	      (write-num ctotal))
        (when grand-total
	  (write-num grand-total)))
      (when (and cell-counts totals)
        (format t "~&~7T")
        (loop for col from 0 to num-columns do
	      (write-num (aref cell-counts num-rows col)))))
    (terpri)))

#+test
(defun test-anova-two-way-variables ()
  (let ((iv1 '(H H H H H H H H H H H H
		 IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE IFE
		 P P P P P P P P P P P P))
	(iv2 '(10 10 10 20 20 20 30 30 30 40 40 40
		  10 10 10 20 20 20 30 30 30 40 40 40
		  10 10 10 20 20 20 30 30 30 40 40 40))
	(dv  '(10.5 9.2 7.9 12.8 11.2 13.3 12.1 12.6 14.0 10.8 9.1 12.5 
		    8.1 8.6 10.1 12.7 13.7 11.5 14.4 15.4 13.7 11.3 12.5 14.5 
		    16.1 15.3 17.5 16.6 19.2 18.5 20.8 18.0 21.0 18.4 18.9 17.2)))
    (let ((data3 (make-3d-table dv iv1 iv2)))
      (loop for i from 0 below 3 do
	    (loop for j from 0 below 4 do
		  (loop for k from 0 below 3 do (format t "~4,1f " (aref data3 i j k)))
		  (format t " | "))
	    (format t "~%")))
    (let ((table (anova-two-way-variables dv iv1 iv2)))
      (print-anova-table table))))


#+test
(defun test-linear-regression-minimal ()
  ;; example 12.1 from Devore
  (let ((iv '(2.5 5 10 15 17.5 20 25 30 35 40))
	(dv '(63 58 55 61 62 37 38 45 46 19)))
    (spy (linear-regression-minimal dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.2 from Devore
  (let ((iv '(300 350 400 400 450 450 480 480 530 530 580 580 620 620 670 700))
	(dv '(5.8 4.5 5.9 6.2 6.0 7.5 6.1 8.6 8.9 8.2 14.2 11.9 11.1 11.5 14.5 14.8)))
    (spy (linear-regression-minimal dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.3 from Devore
  (let ((iv '(8.3 8.3 12.1 12.1 17.0 17.0 17.0 24.3 24.3 24.3 33.6))
	(dv '(227 312 362 521 640 539 728 945 738 759 1263)))
    (spy (linear-regression-minimal dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  )

#+test
(defun test-linear-regression-brief-summaries ()
  ;; example 12.1 from Devore
  (let ((iv '(2.5 5 10 15 17.5 20 25 30 35 40))
	(dv '(63 58 55 61 62 37 38 45 46 19)))
    (spy (linear-regression-brief dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.2 from Devore
  (let ((iv '(300 350 400 400 450 450 480 480 530 530 580 580 620 620 670 700))
	(dv '(5.8 4.5 5.9 6.2 6.0 7.5 6.1 8.6 8.9 8.2 14.2 11.9 11.1 11.5 14.5 14.8)))
    (spy (linear-regression-brief dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.3 from Devore
  (let ((iv '(8.3 8.3 12.1 12.1 17.0 17.0 17.0 24.3 24.3 24.3 33.6))
	(dv '(227 312 362 521 640 539 728 945 738 759 1263)))
    (spy (linear-regression-brief dv iv))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons) (linear-regression-brief dv iv)))
  ;; example 12.4 from Devore.  The function's p-value doesn't match Devore's
  ;; because his example is a one-tailed test for whether slope is positive,
  ;; rather than non-zero, which is what we calculate.
  (let ((vals (multiple-value-list
		(linear-regression-brief-summaries 32 3893 290 478537 4160 36473))))
    (format t "~%~@{~f~}~%" (mapcar #'float vals))
    (destructuring-bind (slope ignore r2 se-slope) vals
      (let ((t-stat (/ slope se-slope)))
	(spy t-stat (students-t-significance t-stat 30 :positive)))
      (spy (sqrt r2)))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief-summaries 32 3893 290 478537 4160 36473))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief-summaries 32 3893.0 290.0 478537.0 4160.0 36473.0)))
  ;; example 12.5 from Devore.  The p-value doesn't match Devore's because his
  ;; example is testing whether slope=20, rather than whether slope=0.
  (let ((iv '(78 75 78 81 84 86 87))
	(dv '(850 775 750 975 915 1015 1030)))
    (let ((vals (multiple-value-list
		  (linear-regression-brief dv iv))))
      (format t "~%~@{~f~}~%" (mapcar #'float vals))
      (destructuring-bind (slope ignore ignore se-slope) vals
	(let ((dof    (- (length iv) 2))
	      (t-stat (/ (- slope 20) se-slope)))
	  (spy t-stat
	       dof
	       (students-t-significance t-stat dof :positive)
	       (confidence-interval-t-summaries slope dof se-slope .95)))))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief dv iv)))
  ;; example 12.10 from Devore
  (let ((iv '(1.98 1.44 2.02 1.20 1.57 1.82 1.45 1.80))
	(dv '(5.6 7.7 8.8 5.1 6.8 3.9 4.5 5.8)))
    (multiple-value-bind (ignore ignore r2) (linear-regression-brief dv iv)
      (spy (sqrt r2)))
    #+Explorer
    (time:timeit (:cpu :cons :number-cons)
      (linear-regression-brief dv iv)))
  )

#+test
(defun test-linear-regression-verbose ()
  ;; From Devore, example 12.13
  (let ((iv '(1000 1100 1200 1250 1300 1400 1450))
	(dv '(220 280 350 375 450 470 500)))
    (multiple-value-bind (slope intercept determination correlation std-err-slope std-err-intercept anova-table)
	(linear-regression-verbose dv iv)
      (format t "~%~@{~a ~30t~f~%~}"
	      "slope" slope
	      "intercept" intercept
	      "determination" determination
	      "correlation" correlation
	      "std-err-slope" std-err-slope
	      "std-err-intercept" std-err-intercept)
      (spy anova-table)
      (setf anova-table (mapcar #'(lambda (row)
				    (mapcar #'(lambda (value)
						(if (integerp value) value (float value)))
					    row))
				anova-table))
      (format t "~2%~{~s~%~}" anova-table))))

#+test
(defun test-mlr ()
  (multiple-value-bind (dv ivs) (example-data-for-mlr)
    (spy (apply #'multiple-linear-regression-normal dv ivs))))




#+test
(defun test-multiple-linear-regression-normal ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(2 3 4) 50 10.0)
    (spy (apply #'multiple-linear-regression-normal dv ivs))))

#+test
(defun example-data-for-mlr ()
  "Data from example 13.6 in Devore, first edition.  Correct regression function is

y = 19.45 + 1.44 x1 + 0.34 x2

SSE = 29.496
MSE = 1.18
R2  = .965
F   = 344.64
"
  (let ((y '(30.9 32.7 36.7 41.9 40.9 42.9 46.3 47.6 47.2 44.0 47.7 43.9 46.8 46.2 47.0 46.8 45.9 48.8 46.2 47.8 49.2 48.3 48.6 50.2 49.6 53.2 54.3 55.8))
	(x1 '(8.5 8.9 10.6 10.2 9.8 10.8 11.6 12.0 12.5 10.9 12.2 11.9 11.3 13.0 12.9 12.0 12.9 13.1 11.4 13.2 11.6 12.1 11.3 11.1 11.5 11.6 11.7 11.7))
	(x2 '(2 3 3 20 22 20 31 32 31 28 36 28 30 27 24 25 28 28 32 28 35 34 35 40 45 50 55 57)))
    ;; Test that the data are typed in correctly.  They are
    '(progn 
       (spy (length y) (length x1) (length x2))
       (spy (reduce #'+ x1))
       (spy (reduce #'+ x2))
       (spy (reduce #'+ x1 :key #'square))
       (spy (reduce #'+ x2 :key #'square))
       (spy (reduce #'+ (mapcar #'* x1 x2)))
       (spy (reduce #'+ y))
       (spy (reduce #'+ (mapcar #'* y x1)))
       (spy (reduce #'+ (mapcar #'* y x2)))
       (spy (reduce #'+ y))
       (spy (reduce #'+ y :key #'square)))
    (values y (list x1 x2))))

#+test
(defun test-mlr-minimal ()
  (multiple-value-bind (dv ivs) (example-data-for-mlr)
    (spy (apply #'multiple-linear-regression-minimal dv ivs))
    (spy (apply #'multiple-linear-regression-normal dv ivs))))

#+test
(defun generate-test-data-for-multiple-linear-regression
       (true-intercept true-coefs num-data &optional (sd 1.0))
  "Generates a list of dv values and a list of lists of iv values."
  (let ((dv nil)
	(ivs (loop for x in true-coefs collect nil)))
    (dotimes (i num-data)
      (let ((xs (loop for c in true-coefs collect (random 100.0))))
	;; record the independent variables
	(do ((iv-list ivs (cdr iv-list))
	     (x-list  xs  (cdr x-list)))
	    ((null iv-list))
	  (push (car x-list) (car iv-list)))
	;; record the dependent variable, which applies the linear model plus
	;; some noise.
	(push (loop for x in xs
		    for c in true-coefs
		    sum (* x c) into s
		    finally (return (+ true-intercept s (math:normal-random 0.0 sd))))
	      dv)))
    (values dv ivs)))

#+test
(defun test-multiple-linear-regression-minimal ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(2 3 4) 50)
    (spy (apply #'multiple-linear-regression-minimal dv ivs))))



#+test
(defun test-mlr-brief ()
  (multiple-value-bind (dv ivs) (example-data-for-mlr)
    (spy (apply #'multiple-linear-regression-brief dv ivs))
    (multiple-value-bind (int coefs r-list t-bs betas r-square f ss-regs ss-percent ss-reg ss-res mse-reg mse-res)
	(apply #'multiple-linear-regression-normal dv ivs)
      (declare (ignore r-list t-bs betas r-square f ss-regs ss-percent ss-reg mse-reg mse-res))
      (spy int coefs ss-res))))

#+test
(defun test-multiple-linear-regression-brief ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(2 3 4) 1000 1.0)
    (spy (apply #'multiple-linear-regression-brief dv ivs))
    (multiple-value-bind (int coefs r-list t-bs betas r-square f ss-regs ss-percent ss-reg ss-res mse-reg mse-res)
	(apply #'multiple-linear-regression-normal dv ivs)
      (declare (ignore r-list t-bs betas r-square f ss-regs ss-percent ss-reg mse-reg mse-res))
      (spy int coefs ss-res))))

#+test
(defun test-mlr-verbose ()
  (multiple-value-bind (dv ivs) (example-data-for-mlr)
    (let ((v1 (multiple-value-list (apply #'multiple-linear-regression-verbose dv ivs)))
	  (v2 (multiple-value-list (apply #'multiple-linear-regression-normal dv ivs)))
	  (names '(intercept coefficients r-scores Ts betas R2 F ssr-portions vfv SSR SSE MSR MSE)))
      (format t "~3&")
      (mapcar #'(lambda (name v1 v2) (format t "~a:  ~16t~s~%~16t~s~%" name v1 v2)) names v1 v2))))




#+test
(defun test-MLR-verbose-vs-LR-vs-MLR ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(5) 10 10.0)
    (spy (linear-regression-verbose dv (car ivs)))
    (spy (apply #'multiple-linear-regression-normal dv ivs))
    (spy (apply #'multiple-linear-regression-verbose dv ivs))))

#+test
(defun test-multiple-linear-regression-verbose ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(2 3 4 10) 10 5.0)
    (time:timeit (:cpu :label "svd") (apply #'multiple-linear-regression-verbose dv ivs))
    (time:timeit (:cpu :label "norm") (apply #'multiple-linear-regression-normal dv ivs))
    (let ((v1 (multiple-value-list (apply #'multiple-linear-regression-verbose dv ivs)))
	  (v2 (multiple-value-list (apply #'multiple-linear-regression-normal dv ivs)))
	  (names '(intercept coefficients r-scores Ts betas R2 F ssr-portions vfv SSR SSE MSR MSE zeroed)))
      (format t "~3&")
      (mapcar #'(lambda (name v1 v2) (format t "~a:  ~16t~s~%~16t~s~%" name v1 v2)) names v1 v2))))



#+test
(defun test-multiple-linear-regression-verbose-on-degenerate-data ()
  (multiple-value-bind (dv ivs)
      (generate-test-data-for-multiple-linear-regression 1 '(2 3 4 10) 100 5.0)
    ;; create a degenerate column
    (push (mapcar #'(lambda (a b c) (+ (* 1.5 a) (* 2.5 b) (* 3.5 c)))
		  (car ivs) (cadr ivs) (caddr ivs))
	  ivs)
    (let ((v1 (multiple-value-list (apply #'multiple-linear-regression-verbose dv ivs)))
	  (v2 (multiple-value-list (apply #'multiple-linear-regression-normal dv (cdr ivs))))
	  (v3 (multiple-value-list (apply #'multiple-linear-regression-normal dv (cons (car ivs) (cddr ivs)))))
	  (names '(intercept coefficients r-scores Ts betas R2 F ssr-portions vfv SSR SSE MSR MSE zeroed)))
      (format t "~2&")
      (mapcar #'(lambda (name v1 v2 v3) (format t "~a:  ~16t~s~%~16t~s~%~16t~s~%" name v1 v2 v3))
	      names v1 v2 v3))))



#+test
(defun test-smooth-median-2 ()
  (spy (smooth-median-2 '(1 3 5 7 9)))
  (spy (smooth-median-2 '(1 5 2 6 3 7 4 8))))

#+test
(defun test-smooth-median-3 ()
  (spy (smooth-median-3 '(1 2 3 4 5 6)))
  (spy (smooth-median-3 '(1 1 3 1 1 1)))
  (spy (smooth-median-3 '(1 2 3 2 1 0)))
  (spy (smooth-median-3 '(1 2 3 3 2 1))))

#+test
(defun test-smooth-median-4 ()
  (spy (smooth-median-4 '(1 2 3 4 5 6)))
  (spy (smooth-median-4 '(1 1 3 1 1 1)))
  (spy (smooth-median-4 '(1 2 3 2 1 0)))
  (spy (smooth-median-4 '(1 2 3 3 2 1))))

#+test
(defun test-smooth-median-5 ()
  (spy (smooth-median-5 '(1 2 3 4 5 6)))
  (spy (smooth-median-5 '(1 1 3 1 1 1)))
  (spy (smooth-median-5 '(1 2 3 2 1 0)))
  (spy (smooth-median-5 '(1 2 3 3 2 1))))

#+test
(defun test-smooth-hanning ()
  (spy (smooth-hanning '(1 2 3 4 5 6)))
  (spy (smooth-hanning '(1 1 3 1 1 1)))
  (spy (smooth-hanning '(1 2 3 2 1 0)))
  (spy (smooth-hanning '(1 2 3 3 2 1))))

#+test
(defun test-smooth-4253H ()
  ;; I don't know if it's correct, but at least it runs.
  (spy (smooth-4253h '(1 2 3 4 5))))

#+test
(defun test-chi-square-2x2-counts ()
  ;; From B&K, page 330
  (multiple-value-bind (chi2 p) (chi-square-2x2-counts 521 283 130 384 nil)
    (format t "~2&chi^2 = ~f; p = ~f~%" chi2 p)))

#+test
(defun test-chi-square-2x2 ()
  (let* ((events (nconc (loop repeat 521 collecting '(a 0))
			(loop repeat 283 collecting '(a 1))
			(loop repeat 130 collecting '(b 0))
			(loop repeat 384 collecting '(b 1))))
	 (v1     (mapcar #'first events))
	 (v2     (mapcar #'second events)))
    (let ((*print-array* t)) (spy (make-contingency-table v1 v2)))
    (multiple-value-bind (chi2 p table v1-values v2-values)
	(chi-square-2x2 v1 v2)
      (format t "~2&chi^2 = ~f; p = ~f~%" chi2 p))))

#+test
(defun test-chi-square-rxc-counts ()
  (macrolet ((test (array)
	       `(multiple-value-bind (chi2 p) (chi-square-rxc-counts ,array)
		  (format t "~2&chi-square = ~f~%significance = ~f~%" chi2 p))))
    ;; Devore, example 14.12
    (test #2a((198 202)
	      (140 210)
	      (133 217)))
    ;; Devore, example 14.13
    (test #2a((20 28 23 14 12)
	      (14 34 21 14 12)
	      (04 12 10 20 53)))
    ;; Devore, example 14.14
    (test #2a((24 15 17)
	      (52 73 80)
	      (58 86 36)))
    ))

#+test
(defun test-make-contingency-table ()
  (let ((*print-array* t))
    (spy (make-contingency-table '(a a a b b b b c c c c c)
				 '(e f g e f g e f g e f g)))))

#+test
(defun test-chi-square-rxc ()
  ;; These should be independent
  (loop repeat 20 do
	(let ((v1 (loop repeat 50 collecting (elt '(a b c d) (random 4))))
	      (v2 (loop repeat 50 collecting (elt '(a b c d) (random 4)))))
	  (multiple-value-bind (chi2 p table v1-values v2-values)
	      (chi-square-rxc v1 v2)
	    (format t "chi^2 = ~6,3f; p = ~8,6f~%" chi2 p)))))