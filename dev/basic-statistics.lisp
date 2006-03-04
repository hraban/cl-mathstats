;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

|#

(in-package metabang.math)

;;; ---------------------------------------------------------------------------
;;; SIMPLE STATISTICS

(defmethod convert ((thing sequence) (type (eql 'iteratable-thing)))
  (values thing))

;;; ---------------------------------------------------------------------------

(defmethod convert ((thing iteratable-container-mixin) (type (eql 'iteratable-thing)))
  (values thing))

;;; ---------------------------------------------------------------------------
                    
(define-statistic data-length () () ()
   ((data 'iteratable-thing))
   (data &key start end key)
  "Returns the number of data values in `data.' Essentially, this is the Common
Lisp `length' function, except it handles sequences where there is a `start' or
`end' parameter.  The `key' parameter is ignored."
  (declare (ignore key))
  (assert (iteratable-p data))
  (check-type start (or fixnum null))
  (check-type end   (or fixnum null))
  (if (null end)
      (let ((n (size data)))
	(if (null start)
	    n
	    (- n start)))
      (if (null start)
	  end
	  (- end start))))

;;; ---------------------------------------------------------------------------

(define-statistic mean () () ()
   ((data 'iteratable-thing))
   (data &rest standard-args &key start end key)
  "Returns the arithmetic mean of `data,' which should be a sequence.

Signals `no-data' if there is no data."
  (declare (ignore start end key))
  ;;(check-type data sequence)
  (let ((n (apply #'data-length data standard-args)))
    (if (plusp n)
	(float (/ (apply #'reduce-elements data #'+ standard-args) n))
	(error 'no-data))))

;;; ---------------------------------------------------------------------------

(defun sum-of-squares (data &rest standard-args &key start end key)
  "Returns the sum of squared distances from the mean of `data'.

Signals `no-data' if there is no data."
  (declare (ignore start end key))
  (let ((m (apply #'mean data standard-args)))
    (let ((key (getf standard-args :key nil)))
      (if (null key)
	  ;; special case for null key to avoid useless funcalls
	  (apply #'reduce-elements data #'+
		 :key #'(lambda (x) (square (- x m)))
		 standard-args)
	  ;; the general case
	  (apply #'reduce-elements data #'+
		 :key #'(lambda (x) (square (- (funcall key x) m)))
		 standard-args)))))

;;; ---------------------------------------------------------------------------

(define-statistic variance () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the variance of `data,' that is, the `sum-of-squares' divided by
n-1. Signals `no-data' if there is no data.  Signals `insufficient-data' if
there is only one datum."
  (declare (ignore start end key))
  (let ((n (apply #'data-length data standard-args)))
    (case n
      (0 (error 'no-data))
      (1 (error 'insufficient-data))
      (t (/ (apply #'sum-of-squares data standard-args) (- n 1))))))

;;; ---------------------------------------------------------------------------

(define-statistic standard-deviation () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the standard deviation of `data,' which is just the square root of
the variance.

Signals `no-data' if there is no data.  Signals `insufficient-data' if there is
only one datum."
  (declare (ignore start end key))
  (sqrt (apply #'variance data standard-args)))

;;; ---------------------------------------------------------------------------

(define-statistic skewness () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the skewness of `data', which is the sum of cubed distances from the
mean divided by the standard deviation, divided by N."
  (declare (ignore start end key))
  (let* ((n (apply #'data-length data standard-args))
         (m (apply #'mean data standard-args))
         (var (apply #'variance data standard-args))
         (sd (sqrt var)))
    (if (= sd 0)
      (restart-case (error 'zero-standard-deviation)
        (use-value (x) x))
      (let ((key (getf standard-args :key nil)))
        (if (null key)
	  ;; special case for null key to avoid useless funcalls
	  (/ (apply #'reduce-elements data #'+
                    :key #'(lambda (x) (expt (- x m) 3))
                    standard-args) 
             (* (1- n) sd var))
	  ;; the general case
	  (/ (apply #'reduce-elements data #'+
                    :key #'(lambda (x) (expt (- (funcall key x) m) 3))
                    standard-args) 
             (* (1- n) sd var)))))))

;;; ---------------------------------------------------------------------------

(define-statistic minimum () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the element of the sequence `data' whose `key' is minimum.  Signals
`no-data' if there is no data.  If there is only one element in the data
sequence, that element will be returned, regardless of whether it is valid (a
number)."
  (declare (ignore start end))
  (if (null key)
      (apply #'reduce-elements data #'min standard-args)
      (apply #'reduce data
	     #'(lambda (x y)
			  (if (< (funcall key x)
				 (funcall key y))
			      x
			      y))
	     ;; have to override the key function, so we get the real item
	     :key #+Explorer nil #-Explorer #'identity
	     standard-args)))

;;; ---------------------------------------------------------------------------

(define-statistic maximum () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the element of the sequence `data' whose `key' is maximum.  Signals
`no-data' if there is no data.  If there is only one element in the data
sequence, that element will be returned, regardless of whether it is valid (a
number)."
  (declare (ignore start end))
  (if (null key)
      (apply #'reduce-elements data #'max standard-args)
      (apply #'reduce-elements data
	     #'(lambda (x y)
			  (if (> (funcall key x)
				 (funcall key y))
			      x
			      y))
	     ;; have to override the key function, so we get the real item
	     :key #'identity
	     standard-args)))

;;; ---------------------------------------------------------------------------

(define-statistic range () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the range of the sequence `data.' Signals `no-data' if there is no
data.  The range is given by max - min."
  (declare (ignore start end))
  (if (null key)
      (- (apply #'maximum data standard-args)
	 (apply #'minimum data standard-args))
      (let ((big-elt   (apply #'maximum data standard-args))
	    (small-elt (apply #'minimum data standard-args)))
	(- (funcall key big-elt)
	   (funcall key small-elt)))))     

;;; ---------------------------------------------------------------------------

(define-statistic quantile () () ()
  ((data 'iteratable-thing))
  (data q &rest standard-args &key start end key)
  "Returns the element which is the q'th percentile of the data when accessed by
`key.' That is, it returns the element such that `q' of the data is smaller than
it and 1-`q' is above it, where `q' is a number between zero and one, inclusive.
For example, if `q' is .5, this returns the median; if `q' is 0, this returns
the minimum (although the `minimum' function is more efficient).

This function uses the bisection method, doing linear interpolation between
elements i and i+1, where i=floor(q(n-1)).  See the manual for more information.
The function returns three values: the interpolated quantile and the two
elements that determine the interval it was interpolated in.  If the quantile
was exact, the second two values are the same element of the data."
  (check-type data sequence)
  #-(or allegro lucid)
  (check-type q (real 0 1))
  (let* ((n (apply #'data-length data standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      ;; This sorting could be replaced by an O(N) algorithm, but we'll defer that.
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      (multiple-value-bind (index lambda) (floor (* q (- n 1)))
	(if (zerop lambda)
	    (let ((elt (aref temp index)))
	      (values (if key (funcall key elt) elt) elt elt))
	    (let ((elt1 (aref temp index))
		  (elt2 (aref temp (1+ index))))
	      (values (+ (* (if key (funcall key elt1) elt1) (- 1 lambda))
			 (* (if key (funcall key elt2) elt2) lambda))
		      elt1
		      elt2)))))))

;;; ---------------------------------------------------------------------------

(define-statistic median () () ()
  ((data 'iteratable-thing))
  (data &rest standard-args &key start end key)
  "Returns the median of the subsequence of `data' from `start' to `end', using
`key'.  The median is just the 0.5 quantile, and so this function returns the
same values as the `quantile' function."
  (declare (ignore start end key))
  (apply #'quantile data .5 standard-args))

;;; ---------------------------------------------------------------------------

(define-statistic trimmed-mean () () ()
  ((data 'sequence))
  (data percentage &rest standard-args &key start end key)
  "Returns a trimmed mean of `data.' A trimmed mean is an ordinary, arithmetic
mean of the data, except that an outlying percentage has been discarded.  For
example, suppose there are ten elements in `data,' and `percentage' is 0.1: the
result would be the mean of the middle eight elements, having discarded the
biggest and smallest elements.  If `percentage' doesn't result in a whole number
of elements being discarded, then a fraction of the remaining biggest and
smallest is discarded.  For example, suppose `data' is '(1 2 3 4 5) and
`percentage' is 0.25: the result is (.75(2) + 3 + .75(4))/(.75+1+.75) or 3.  By
convention, the 0.5 trimmed mean is the median, which is always returned as a
number."
  (check-type data sequence)
  #-(or allegro lucid)(check-type percentage (real 0 1/2))
  (let* ((n (apply #'data-length data standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      ;; This sorting could be replaced by calls to an O(N) algorithm, but we'll
      ;; defer that.
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, here we go.  By convention, if there is nothing left after the
      ;; trimming, we return the median.  There is nothing left if (1) n is even
      ;; and there is less than 2 items left or (2) n is odd and there is less
      ;; than one item left.
      (if (if (evenp n)
	      (<= (- n (* 2 n percentage)) 2.0)
	      (<= (- n (* 2 n percentage)) 1.0))
	  ;; Not enough left, so take median
	  (if (evenp n)
	      (if (null key)
		  (/ (+ (elt temp (floor n 2))
			(elt temp (- (floor n 2) 1)))
		     2)
		  (/ (+ (funcall key (elt temp (floor n 2)))
			(funcall key (elt temp (- (floor n 2) 1))))
		     2))
	      (if (null key)
		  (elt temp (floor n 2))
		  (funcall key (elt temp (floor n 2)))))
	  ;; There is enough left, so add up the middle (whole) elements, plus a
	  ;; percentage of the end elements.  We compute q, which is the number
	  ;; of whole elements to trim off each end, and r, which is the
	  ;; fractional amount to trim off the next element.
	  (multiple-value-bind (q r) (truncate (* n percentage))
	    (setf r (- 1 r))
	    (let ((sum (+ (* r (if key
				   (funcall key (elt temp q))
				   (elt temp q)))
			  (* r (if key
				   (funcall key (elt temp (- n q 1)))
				   (elt temp (- n q 1))))
			  (if key
			      (reduce #'+ temp
					 :start (1+ q) :end (- n q 1) :key key)
			      (reduce #'+ temp
					 :start (1+ q) :end (- n q 1))))))
	      (/ sum (* n (- 1 (* 2 percentage))))))))))

;;; ---------------------------------------------------------------------------

(defmacro start/end (call-form start-n end-n)
  `(if (null ,start-n)
       (if (null ,end-n)
	   ,call-form
	   (,@call-form :end ,end-n))
       (if (null ,end-n)
	   (,@call-form :start ,start-n)
	   (,@call-form :start ,start-n :end ,end-n))))

;;; ---------------------------------------------------------------------------

(define-statistic mode () () ()
  ((data 'sequence))
  (data &rest standard-args &key start end key)
  "Returns the most frequent element of `data,' which should be a sequence.  The
algorithm involves sorting, and so the data must be numbers or the `key'
function must produce numbers.  Consider `sxhash' if no better function is
available.  Also returns the number of occurrences of the mode.  If there is
more than one mode, this returns the first mode, as determined by the sorting of
the numbers."
  (declare #-MCL (values element number-of-occurrences))
  (check-type data sequence)
  (let* ((n (apply #'data-length data standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, the sorting has already grouped the data, so all we have to do is
      ;; keep a running record of the biggest group seen so far.
      (let* ((current-group      (aref temp 0))
	     (current-group-key  (when key (funcall key current-group)))
	     (current-group-size 0)
	     (biggest-group      nil)
	     (biggest-group-size 0))
	(if (null key)
	    (dotimes (i n)
	      (let ((elt (aref temp i)))
		(if (= elt current-group)
		    (incf current-group-size)
		    (progn (when (> current-group-size biggest-group-size)
			     (setf biggest-group      current-group
				   biggest-group-size current-group-size))
			   (setf current-group elt
				 current-group-size 1)))))
	    (dotimes (i n)
	      (let* ((elt     (aref temp i))
		     (elt-key (funcall key elt)))
		(if (= elt-key current-group-key)
		    (incf current-group-size)
		    (progn (when (> current-group-size biggest-group-size)
			     (setf biggest-group current-group
				   biggest-group-size current-group-size))
			   (setf current-group elt
				 current-group-key elt-key
				 current-group-size 1))))))
	;; This code is just in case the last group is the biggest group.  It
	;; won't be processed above, because groups are only considered for
	;; biggest when the next group starts, which works fine for every group
	;; except the last.
	(when (> current-group-size biggest-group-size)
	  (setf biggest-group      current-group
		biggest-group-size current-group-size))
	(values biggest-group biggest-group-size)))))

;;; ---------------------------------------------------------------------------

(defparameter *continuous-variable-uniqueness-factor*
  0.90d0)
(defparameter *continous-data-window-divisor*
  (- 100 (* *continuous-variable-uniqueness-factor* 100)))

;;; ---------------------------------------------------------------------------

(defun mode-for-continuous-data (data &rest standard-args &key start end key window)
  "Returns the most frequent element of `data,' which should be a sequence.  The
algorithm involves sorting, and so the data must be numbers or the `key'
function must produce numbers.  Consider `sxhash' if no better function is
available.  Also returns the number of occurrences of the mode.  If there is
more than one mode, this returns the first mode, as determined by the sorting of
the numbers.

Keep in mind that if the data has multiple runs of like values that are bigger
than the window size (currently defaults to 10% of the size of the data) this
function will blindly pick the first one. If this is the case you probabaly
should be calling `mode' instead of this function."
  
  (declare #-MCL (values element number-of-occurrences))
  (check-type data sequence)
  (when (not (data-continuous-p data))
    (warn 'seems-to-be-discrete))
  (let* ((n (apply #'data-length data :allow-other-keys t standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n)))
         (j (or window (max 3 (round n *continous-data-window-divisor*)))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))

      ;; define a macro that expands into two different loops depending on whether we need to
      ;; deal with a key or not.
      (macrolet ((doit (key)
                   ;; Find the window which has the smallest difference between the values, this will be
                   ;; the one that has larget number of values of similar value.    
                   `(loop for i from 1 to (- n j 1)
	                  for i+j = (+ i j)
	                  with smallest-i = 0
	                  with smallest-difference = (- ,@(if key `((funcall ,key (aref temp j))) `((aref temp j)))
				                        ,@(if key `((funcall ,key (aref temp 0))) `((aref temp 0))))
	                  for difference = (- ,@(if key `((funcall ,key (aref temp i+j))) `((aref temp i+j)))
			                      ,@(if key `((funcall ,key (aref temp i))) `((aref temp i))))
	                  do
			  (when (< difference smallest-difference)
	                    (setf smallest-difference difference
		                  smallest-i i))
	                  
	                  finally (return
		                   (values (/ (+
                                                (aref temp smallest-i) (aref temp (+ smallest-i j))
                                                #+BAD
                                               ,@(if key `((funcall ,key (aref temp smallest-i)))       `((aref temp smallest-i)))
                                               #+BAD
				                ,@(if key `((funcall ,key (aref temp (+ smallest-i j)))) `((aref temp (+ smallest-i j)))))
			                      2))))))
        (if key
            (doit key)
            (doit nil))))))


;;; ---------------------------------------------------------------------------

(defun data-continuous-p (sequence)
  (let ((length (length sequence))
        ;; The simple, stupid way to see if it is continous...
        (unique-count (length (extract-unique-values sequence))))
    ;; is that if 90% of the data are
  (and (and (> length 10)
            (> unique-count (* *continuous-variable-uniqueness-factor* length))))))

;;;
;;; smart-mode is somewhat smart at determining whether or not to estimate the mode
;;;

(defun smart-mode (sequence &rest args)
  (if (data-continuous-p sequence)
    (apply #'mode-for-continuous-data sequence args)
    (apply #'mode sequence (append (if (not (numberp (car sequence))) (list :key #'sxhash)) args))))

;;; ---------------------------------------------------------------------------

(define-statistic multiple-modes () () ()
  ((data 'sequence))
  (data k &rest standard-args &key start end key)
  "Returns the `k' most frequent elements of `data,' which should be a sequence.
The algorithm involves sorting, and so the data must be numbers or the `key'
function must produce numbers.  Consider #'sxhash if no better function is
available.  Also returns the number of occurrences of each mode.  The value is
an association list of modes and their counts.  This function is a little more
computationally expensive than `mode,' so only use it if you really need
multiple modes."
  (check-type data sequence)
  (let* ((n (apply #'data-length data standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      ;; Okay, the sorting has already grouped the data, so all we have to do is
      ;; keep a running record of the biggest groups seen so far.
      (let* ((current-group      (aref temp 0))
	     (current-group-key  (when key (funcall key current-group)))
	     (current-group-size 0)
	     (modes              nil))
	;; with the exception of the update function, this code is identical to
	;; that of `mode.'
	(flet ((update ()
		 (if (< (length modes) k)
		     (push (cons current-group current-group-size)
			   modes)
		     (let ((min-mode-size (reduce #'min modes :key #'cdr)))
		       (when (< min-mode-size current-group-size)
			 (let ((min-mode (rassoc min-mode-size modes :test #'=)))
			   (setf (car min-mode) current-group
				 (cdr min-mode) current-group-size)))))))
	  (if (null key)
	      (dotimes (i n)
		(let ((elt (aref temp i)))
		  (if (= elt current-group)
		      (incf current-group-size)
		      (progn (update)
			     (setf current-group elt
				   current-group-size 1)))))
	      (dotimes (i n)
		(let* ((elt     (aref temp i))
		       (elt-key (funcall key elt)))
		  (if (= elt-key current-group-key)
		      (incf current-group-size)
		      (progn (update)
			     (setf current-group elt
				   current-group-key elt-key
				   current-group-size 1))))))
	  ;; This code is just in case the last group is the biggest group.  It
	  ;; won't be processed above, because groups are only considered for
	  ;; biggest when the next group starts, which works fine for every group
	  ;; except the last.
	  (update)
	  modes)))))


;;; ---------------------------------------------------------------------------

(define-statistic interquartile-range () () ()
  ((data 'sequence))
  (data &rest standard-args)
  "The interquartile range is similar to the variance of a sample because both
are statistics that measure out ``spread out'' a sample is.  The interquartile
range is the difference between the 3/4 quantile (the upper quartile) and the
1/4 quantile (the lower quartile)."
  (- (apply #'quantile data 3/4 standard-args)
     (apply #'quantile data 1/4 standard-args)))

;;; ---------------------------------------------------------------------------

(define-statistic tukey-summary ()
  ((minimum)
   (first-quartile)
   (median)
   (third-quartile)
   (maximum))
  ()
  ((data 'sequence))
  (data &rest standard-args &key start end key)
  "Computes a Tukey five-number summary of the data.  That is, it returns, in
increasing order, the extremes and the quartiles: the minimum, the 1/4 quartile,
the median, the 3/4 quartile, and the maximum."
  (declare #-MCL (values minimum first-quartile median third-quartile maximum))
  (check-type data sequence)
  (let* ((n (apply #'data-length data standard-args))
	 (start2 (or start 0))
	 (end2 (or end (+ start2 n))))
    (when (zerop n) (error 'no-data))
    (with-temp-vector (temp n)
      (replace temp data :end1 n :start2 start2 :end2 end2)
      (if (null key)
	  (sort temp #'<)
	  (sort temp #'< :key key))
      (flet ((get-quartile (i)
	       (let* ((index (/ (* i (- n 1)) 4))
		      (elt1 (float (aref temp (floor index))))
		      (elt2 (float (aref temp (ceiling index)))))
		 (if (null key)
		     (/ (+ elt1 elt2) 2)
		     (/ (+ (funcall key elt1) (funcall key elt2)) 2)))))
	(values (aref temp 0)
		(get-quartile 1)
		(get-quartile 2)
		(get-quartile 3)
		(aref temp (- n 1)))))))

;;; ---------------------------------------------------------------------------

(define-statistic statistical-summary
    (data-length minimum maximum range median mode mean variance standard-deviation interquartile-range skewness)
   ()
   ()
   ((data 'sequence))
   (data &rest standard-args &key start end key)
  "Compute the length, minimum, maximum, range, median, mode, mean, variance,
standard deviation, and interquartile-range of `sequence' from `start' to `end',
accessed by `key'."
  (declare (ignore start end)
           #-MCL
	   (values length minimum maximum range median mode mean
		   variance standard-deviation interquartile-range))
  (let* ((length   (apply #'data-length data standard-args))
	 (minimum  (apply #'minimum data standard-args))
	 (maximum  (apply #'maximum data standard-args))
	 (range    (if (null key)
		       (- maximum minimum)
		       (- (funcall key maximum)
			  (funcall key minimum))))
	 (median   (apply #'median data standard-args))
	 (mode     (apply #'smart-mode data standard-args))
	 (mean     (apply #'mean data standard-args))
	 (variance (apply #'variance data standard-args))
	 (sd       (sqrt variance))
	 (iqr      (apply #'interquartile-range data standard-args))
         (skewness (apply #'skewness data standard-args)))
    (values length minimum maximum range median mode mean variance sd iqr skewness)))

;;; ---------------------------------------------------------------------------

;;; ***************************************************************************
;;; Dave Fisher's statistics functions
;;; Rewritten by Adam Carlson and Scott D. Anderson
;;; ***************************************************************************

;;; ***************************************************************************
;;; significance statistics

(define-statistic significance ()
   ((statistic)
    (level)))

(define-statistic t-significance (significance)
   ((std-error)
    (dof))
   (statistic level std-error dof))

(define-statistic t-test-one-sample (t-significance) () ()
   ((data 'sequence))
   (data tails &optional (h0-mean 0) &rest standard-args &key start end key)
  "Returns the t-statistic for the mean of the data, which should be a sequence
of numbers.  Let D be the sample mean.  The null hypothesis is that D equals the
`H0-mean.' The alternative hypothesis is specified by `tails': `:both' means D
/= H0-mean, `:positive' means D > H0-mean, and `:negative' means D < H0-mean.

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `zero-variance' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in the sample."
  (declare (ignore start end key)
           #-MCL
	   (values t-statistic significance sample-error dof))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n)
      (error 'no-data))
    (let ((d (apply #'mean data standard-args))
	  (v (apply #'variance data standard-args)))
      (when (zerop v)
	(error 'zero-variance))
      (let* ((se  (sqrt (/ v n)))
	     (tt  (/ (- d h0-mean) se))
	     (sig (students-t-significance tt (- n 1) tails)))
	(values tt sig se (- n 1))))))

;;; ---------------------------------------------------------------------------

(define-statistic t-test (t-significance) () ()
   ((sample-1 'sequence)
    (sample-2 'sequence))
   (sample-1 sample-2 &optional (tails :both) (h0mean 0))
   "Returns the t-statistic for the difference in the means of two samples, which
should each be a sequence of numbers.  Let D=mean1-mean2.  The null hypothesis
is that D=0.  The alternative hypothesis is specified by `tails': `:both' means
D/=0, `:positive' means D>0, and `:negative' means D<0.  Unless you're using
:both tails, be careful what order the two samples are in: it matters!

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `standard-error-is-zero' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in each sample."
  (declare #-MCL (values t-statistic significance std-error dof))
  (check-type tails (member :both :positive :negative))
  (let ((n1 (data-length sample-1))
	(n2 (data-length sample-2)))
    (when (or (zerop n1) (zerop n2))
      (error 'no-data))
    (when (or (< n1 2) (< n2 2))
      (error 'insufficient-data))
    (let* ((mean1 (mean sample-1))
	   (mean2 (mean sample-2))
	   (ss1   (sum-of-squares sample-1))
	   (ss2   (sum-of-squares sample-2))
	   (dof   (+ n1 n2 -2))			; degrees of freedom
	   (sp    (/ (+ ss1 ss2) dof)))		; pooled sample variance
      (when (zerop sp)
	(error 'zero-variance))
      (let* ((std-error    (sqrt (* sp (+ (/ n1) (/ n2)))))
	     (t-statistic  (/ (- (- mean1 mean2) h0mean) std-error))
	     (significance (students-t-significance t-statistic dof tails)))
	(values t-statistic significance std-error dof)))))

;;; ---------------------------------------------------------------------------

(define-statistic d-test (significance)
  ((count)
   (times))
  (statistic level count times)
  ((sample-1 'sequence)
   (sample-2 'sequence))
  (sample-1 sample-2 tails &key (times 1000) (h0mean 0))
  "Two-sample test for difference in means.  Competes with the unmatched,
two-sample t-test.  Each sample should be a sequence of numbers.  We calculate
the mean of `sample-1' minus the mean of `sample-2'; call that D.  Under the null
hypothesis, D is zero.  There are three possible alternative hypotheses: D is
positive, D is negative, and D is either, and they are selected by the `tails'
parameter, which must be :positive, :negative, or :both, respectively.  We count
the number of chance occurrences of D in the desired rejection region, and
return the estimated probability."
  (declare #-MCL (values D significance count))
  (check-type sample-1 sequence)
  (check-type sample-2 sequence)
  (check-type tails (member :both :positive :negative))
  (let ((n1 (data-length sample-1))
	(n2 (data-length sample-2)))
    (when (or (zerop n1) (zerop n2))
      (error 'no-data))
    (when (or (< n1 2) (< n2 2))
      (error 'insufficient-data))
    (let* ((dt  (- (/ (reduce #'+ sample-1) n1)
		   (/ (reduce #'+ sample-2) n2)))
	   (n   (+ n1 n2)))
      (if (zerop h0mean)
	  (with-temp-vector (sample n)
	    (setf (fill-pointer sample) n)
	    (dotimes (i n1) (setf (aref sample i) (elt sample-1 i)))
	    (dotimes (i n2) (setf (aref sample (+ i n1)) (elt sample-2 i)))
	    (let ((count 0))
	      (dotimes (b times)
		(let ((d (- (loop for i from 1 to n1
				  summing (aref sample (random n)) into s1
				  finally (return (/ s1 n1)))
			    (loop for i from 1 to n2
				  summing (aref sample (random n)) into s2
				  finally (return (/ s2 n2))))))
		  (case tails
		    (:both (if (<= (abs dt) (abs d)) (incf count)))
		    (:positive (if (<= dt d) (incf count)))
		    (:negative (if (<= d dt) (incf count))))))
	      (values dt (/ count times) count)))
	  (with-temp-vector (results times)
	    (setf (fill-pointer results) 0)
	    (dotimes (b times)
	      (let ((d (- (loop for i from 1 to n1
				summing (elt sample-1 (random n1)) into s1
				finally (return (/ s1 n1)))
			  (loop for i from 1 to n2
				summing (elt sample-2 (random n2)) into s2
				finally (return (/ s2 n2))))))
		(vector-push d results)))
	    (let ((boot-mean (mean results)))
	      (let ((count 0))
		(dotimes (i times)
		  (let ((d (aref results i)))
		    (case tails
		      (:both (if (<= (abs (- h0mean dt)) (abs (- boot-mean d))) (incf count)))
		      (:positive (if (<= (- dt h0mean) (- d boot-mean)) (incf count)))
		      (:negative (if (<= (- d boot-mean) (- dt h0mean)) (incf count))))))
		(values dt (/ count times) count))))))))

;;; ---------------------------------------------------------------------------

(define-statistic t-test-matched (t-significance) () ()
  ((sample1 'sequence)
   (sample2 'sequence))
  (sample1 sample2 &optional (tails :both))
  "Returns the t-statistic for two matched samples, which should be equal-length
sequences of numbers.  Let D=mean1-mean2.  The null hypothesis is that D=0.  The
alternative hypothesis is specified by `tails': `:both' means D/=0, `:positive'
means D>0, and `:negative' means D<0.  Unless you're using :both tails, be
careful what order the two samples are in: it matters!

The function also returns the significance, the standard error, and the degrees
of freedom.  Signals `standard-error-is-zero' if that condition occurs.  Signals
`insufficient-data' unless there are at least two elements in each sample."
  (declare #-MCL (values t-statistic significance std-error dof))
  (let* ((n1 (data-length sample1))
	 (n2 (data-length sample2)))
    (unless (= n1 n2)
      (error 'unmatched-sequences))
    (when (< n1 2)
      (error 'insufficient-data))
    (with-temp-vector (temp n1)
      (map-into temp #'- sample1 sample2)
      (t-test-one-sample temp tails))))

;;; ---------------------------------------------------------------------------

(define-statistic z-test-one-sample (significance) () ()
  ((data 'sequence))
  (data tails &optional (h0-mean 0) (h0-std-dev 1)
        &rest standard-args &key start end key)
  ;; BAH 7 April 2001 (added 'tails' to prevent warning during compilation)
  (declare (ignore start end key tails) 
           #-MCL
           (values z-statistic significance))
  (let ((n (apply #'data-length data standard-args)))
    (when (zerop n)
      (error 'no-data))
    (let ((d (apply #'mean data standard-args))
          (v (/ h0-std-dev (sqrt n))))
      (when (zerop v)
        (error 'zero-variance))
      (let* ((zs (/ (- d h0-mean) v))
             sig)
        (values zs sig)))))
	     
;;; ---------------------------------------------------------------------------
;;; Should go in utilities

(defun inner-product (sample1 sample2 &key start1 end1 start2 end2)
  "Returns the inner product of the two samples, which should be sequences of
numbers.  The inner product, also called the dot product or vector product, is
the sum of the pairwise multiplication of the numbers.  Stops when either sample
runs out; it doesn't check that they have the same length."
  ;; In the following implementation, we extend the `simple-do' implementation
  ;; to handle start/end arguments, but we try to collapse identical code
  ;; fragments, at least in notation if not in object code.  We do this by a
  ;; local macro that builds a `do,' with most parts conditionalized by whether
  ;; the associated `sample' is a list or a vector.
  (macrolet ((inner-product-loop (v/l-1 v/l-2)
	       `(let ((sum 0)
		      (index1 (or start1 0))
		      (index2 (or start2 0))
		      elt1 elt2)
		  ,(case v/l-1
		     (l `(progn (when start1
				  (setf sample1 (nthcdr start1 sample1)))
				(setf elt1 (car sample1))))
		     (v `(progn (when (null end1)
				  (setf end1 (length sample1)))
				(setf elt1 (aref sample1 index1)))))
		  ,(case v/l-2
		     (l `(progn (when start2
				  (setf sample2 (nthcdr start2 sample2)))
				(setf elt2 (car sample2))))
		     (v `(progn (when (null end2)
				  (setf end2 (length sample2)))
				(setf elt2 (aref sample2 index2)))))
		  (do () (nil)
		    (incf sum (* elt1 elt2))	; the work is done here
		    ,(case v/l-1
		       (l `(progn (setf sample1 (cdr sample1))
				  (incf index1)))
		       (v `(incf index1)))
		    ,(case v/l-2
		       (l `(progn (setf sample2 (cdr sample2))
				  (incf index2)))
		       (v `(incf index2)))
		    (when (or ,(case v/l-1
				 (l `(if (null end1) (endp sample1) (>= index1 end1)))
				 (v `(>= index1 end1)))
			      ,(case v/l-2
				 (l `(if (null end2) (endp sample2) (>= index2 end2)))
				 (v `(>= index2 end2))))
		      (return))
		    ,(case v/l-1
		       (l `(setf elt1 (car sample1)))
		       (v `(setf elt1 (aref sample1 index1))))
		    ,(case v/l-2
		       (l `(setf elt2 (car sample2)))
		       (v `(setf elt2 (aref sample2 index2)))))
		  sum)))
    (etypecase sample1
      (list   (etypecase sample2
		(list   (inner-product-loop l l))
		(vector (inner-product-loop l v))))
      (vector (etypecase sample2
		(list   (inner-product-loop v l))
		(vector (inner-product-loop v v)))))))

;;; ---------------------------------------------------------------------------

(define-statistic covariance () () ()
   ((sample1 'sequence)
    (sample2 'sequence))
   (sample1 sample2 &rest args &key start1 end1 start2 end2)
   "Computes the covariance of two samples, which should be equal-length
sequences of numbers.  Covariance is the inner product of differences between
sample elements and their sample means.  For more information, see the manual."
   (let ((n1 (start/end (data-length sample1) start1 end1))
	 (n2 (start/end (data-length sample2) start2 end2)))
     (cond ((< n2 n1)
	    (setf end1 (+ (or start1 0) n2)))
	   ((< n1 n2)
	    (setf end2 (+ (or start2 0) n1))))
     (/ (- (apply #'inner-product sample1 sample2 args)
	   (/ (* (start/end (reduce #'+ sample1) start1 end1)
		 (start/end (reduce #'+ sample2) start2 end2))
	      n1))
        (1- n1))))

;;; ---------------------------------------------------------------------------

(define-statistic confidence-interval ()
   ((value)
    (lower-bound)
    (upper-bound))
   (value lower-bound upper-bound))

;;; ---------------------------------------------------------------------------

(define-statistic confidence-interval-z (confidence-interval) () ()
   ((data 'sequence))
   (data confidence)
   "Suppose you have a sample of 50 numbers and you want to compute a 90 percent
confidence interval on the population mean.  This function is the one to use.
Note that it makes the assumption that the sampling distribution is normal, so
it's inappropriate for small sample sizes.  Use confidence-interval-t instead.
It returns three values: the mean and the lower and upper bound of the
confidence interval.  True, only two numbers are necessary, but the confidence
intervals of other statistics may be asymmetrical and these values would be
consistent with those confidence intervals.  This function handles 90, 95 and 99
percent confidence intervals as special cases, so those will be quite fast.
`Sample' should be a sequence of numbers.  `Confidence' should be a number
between 0 and 1, exclusive."
   (declare #-MCL (values mean lower upper))
   (check-type data sequence)
   ;; The Allegro compiler barfs on this...
   ;; It generates this erroneous code...
   ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
   #-(or allegro lucid) (check-type confidence (real (0) (1)))
   (confidence-interval-z-summaries
    (mean data)
    (sqrt (/ (variance data) (length data)))
    confidence))

;;; ---------------------------------------------------------------------------

(defun confidence-interval-z-summaries (mean standard-error confidence)
  "This function is just like `confidence-interval-z,' except that instead of
its arguments being the actual data, it takes the following summary statistics:
`mean', a point estimator of the mean of some normally distributed population;
and the `standard-error' of the estimator, that is, the estimated standard
deviation of the normal population.  `Confidence' should be a number between 0
and 1, exclusive."
  #-(or allegro lucid)(check-type mean real)
  #-(or allegro lucid)(check-type standard-error real)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  ;; Can't use `case' because that uses `eql' and (eql 9/10 .9) is false.  Z is
  ;; the width of a confidence interval on the standard Gaussian, which we
  ;; multiply by the standard error to get the confidence interval on the true
  ;; sampling distribution.
  (let* ((z     (cond ((= confidence 90/100) 1.645)
		      ((= confidence 95/100) 1.96)
		      ((= confidence 99/100) 2.58)
		      (t (find-critical-value
			   #'(lambda (x)
			       (gaussian-significance x :both))
			   (- 1 confidence)))))
	 (w     (* standard-error z))
	 (upper (+ mean w))
	 (lower (- mean w)))
    (values mean lower upper)))

;;; ---------------------------------------------------------------------------

(define-statistic confidence-interval-t (confidence-interval) () ()
  ((data 'sequence))
  (data confidence)
  "Suppose you have a sample of 10 numbers and you want to compute a 90 percent
confidence interval on the population mean.  This function is the one to use.
This function uses the t-distribution, and so it is appropriate for small sample
sizes.  It can also be used for large sample sizes, but the function
`confidence-interval-z' may be computationally faster.  It returns three values:
the mean and the lower and upper bound of the confidence interval.  True, only
two numbers are necessary, but the confidence intervals of other statistics may
be asymmetrical and these values would be consistent with those confidence
intervals.  `Sample' should be a sequence of numbers.  `Confidence' should be a
number between 0 and 1, exclusive."
  (declare #-MCL (values mean lower upper))
  (check-type data sequence)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  (let* ((n (length data)))
    (confidence-interval-t-summaries
      (mean data)
      (- n 1)
      (sqrt (/ (variance data) n))
       confidence)))

;;; ---------------------------------------------------------------------------

(defun confidence-interval-t-summaries (mean dof standard-error confidence)
  "This function is just like `confidence-interval-t,' except that instead of
its arguments being the actual data, it takes the following summary statistics:
`mean,' which is the estimator of some t-distributed parameter; `dof,' which is
the number of degrees of freedom in estimating the mean; and the
`standard-error' of the estimator.  In general, `mean' is a point estimator of
the mean of a t-distribution, which may be the slope parameter of a regression,
the difference between two means, or other practical t-distributions.
`Confidence' should be a number between 0 and 1, exclusive."
  (declare #-MCL (values mean lower upper))
  #-(or allegro lucid)(check-type dof (real 1 *))
  #-(or allegro lucid)(check-type mean real)
  #-(or allegro lucid)(check-type standard-error real)
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type confidence (real (0) (1)))
  (let* ((t-x   (find-critical-value
		  #'(lambda (x)
		      (students-t-significance x dof :both))
		  (- 1 confidence)))
	 (w     (* standard-error t-x))
	 (upper (+ mean w))
	 (lower (- mean w)))
    (values mean lower upper)))

;;; ---------------------------------------------------------------------------

(define-statistic confidence-interval-proportion (confidence-interval) () ()
   ()
   (x n confidence)
   "Suppose we have a sample of `n' things and `x' of them are ``successes.'' We
can estimate the population proportion of successes as x/n; call it `p-hat.'
This function computes the estimate and a confidence interval on it.  This
function is not appropriate for small samples with p-hat far from 1/2: `x'
should be at least 5, and so should `n'-`x.' This function returns three values:
p-hat, and the lower and upper bounds of the confidence interval.  `Confidence'
should be a number between 0 and 1, exclusive."
   (declare #-MCL (values p-hat lower upper))
   ;; This formula is from DeVore, page 331
   (let ((p-hat          (/ x n))
	 (standard-error (sqrt (/ (* x (- n x)) (* n n n)))))
     (confidence-interval-z-summaries p-hat standard-error confidence)))

;;; ---------------------------------------------------------------------------

(defun scheffe-tests (group-means group-sizes ms-error df-error)
  "Performs all pairwise comparisons between group means, testing for
significance using Scheffe's F-test.  Returns an upper-triangular table in a
format described in the manual.  Also see the function `print-scheffe-table.'

`Group-means' and `group-sizes' should be sequences.  The arguments `ms-error'
and `df-error' are the mean square error within groups and its degrees of
freedom, both of which are computed by the analysis of variance.  An ANOVA test
should always be run first, to see if there are any significant differences."
  (check-type group-means sequence)
  (check-type group-sizes sequence)
  #-(or allegro lucid)(check-type ms-error real)
  ;; by using `elt,' we sacrifice some efficiency on lists, but it should be
  ;; fairly inconsequential, given that the algorihtm is already O(n^2).
  (let ((N (length group-means)))
    (when (/= N (length group-sizes))
      (error 'unmatched-sequences))
    (let ((N-1 (- N 1)))
      (loop for i from 0 below N-1 collecting
	    (let ((mean-i (elt group-means i))
		  (Ni     (elt group-sizes i)))
	      (loop for j from (1+ i) to N-1 collect
		    ;; Formula from Cohen.  Rather than compute 1/Ni + 1/Nj,
		    ;; which we know will turn into a rational because Ni and Nj
		    ;; are integers, we directly compute (Nj + Ni)/(* Ni Nj)
		    (let* ((mean-j (elt group-means j))
			   (Nj     (elt group-sizes j))
			   (F      (/ (square (- mean-i mean-j))
				      (* ms-error N-1 (/ (+ Ni Nj)
							 (* Ni Nj))))))
		      (list F (f-significance (float f) n-1 df-error t)))))))))

;;; ---------------------------------------------------------------------------

(defun print-scheffe-table
       (scheffe-table &optional group-means (stream *standard-output*))
  "Prints `scheffe-table' on `stream.' If the original one-way anova data had N
groups, the Scheffe table prints as an n-1 x n-1 upper-triangular table.  If
`group-means' is given, it should be a list of the group means, which will be
printed along with the table."
  (let ((N (length scheffe-table)))
    (if (null group-means)
	;; First column is 4 chars wide; the rest are 18 chars wide.
	(progn (format stream "~2&~5t")
	       (loop for row-number from 1 to N do
		     (format stream "~18:@<~d~>| " row-number))
	       (format stream "~%")
	       (loop for row in scheffe-table
		     for row-number from 0 do
		     (format stream "~2d: ~vt~{~{~8,1f ~8,6f | ~}~}~%"
			     row-number (+ 5 (* 20 row-number)) row)))
	;; First column is 12 chars wide; the rest are 18 chars wide.
	(progn (format stream "~2&~13t")
	       (loop for row-number from 1 to N do
		     (format stream "~18:@<~10g~>| "
			     (elt group-means row-number)))
	       (format stream "~%")
	       (loop for row in scheffe-table
		     for row-number from 0 do
		     (format stream "~10g: ~vt~{~{~8,1f ~8,6f | ~}~}~%"
			     (elt group-means row-number)
			     (+ 13 (* 20 row-number))
			     row))))))

;;; ---------------------------------------------------------------------------

(defmethod cross-product ((number-list-1 sequence) (number-list-2 sequence))
  "Takes two sequences of numbers and returns a sequence of cross products.
Formula XYi = Xi * Yi."
  (unless (or (member 'nil number-list-1) 
	      (member 'nil number-list-2)
	      (not (equal (length  number-list-1) 
			  (length  number-list-2))))
    (mapcar #'* number-list-1 number-list-2)))

;;; ---------------------------------------------------------------------------

(defmethod dot-product ((number-list-1 sequence) (number-list-2 sequence))
  "Takes two sequences of numbers and returns the dot product."
  (unless (or (member 'nil number-list-1) 
       (member 'nil number-list-2)
       (not (equal (length  number-list-1) 
     (length  number-list-2))))
    (reduce #'+ (mapcar #'* number-list-1 number-list-2))))

;;; ---------------------------------------------------------------------------

(defun r-score (number-list-1 number-list-2)
  "Takes two sequences and returns the correlation coefficient.
Formula:  Sum (Cross-product (Difference-list (number-list-1)
   			Difference-list (number-list-2)) /
	    (Sqrt (Sum-of-Squares (number-list-1) *
		Sum-of-Squares (number-list-2))))."
  
  (let* ((x-diff (difference-list number-list-1))
	 (y-diff (difference-list number-list-2))
	 (sum-cross-product (sum-list (cross-product x-diff y-diff)))
	 (sum-squares-product (* (sum-of-squares number-list-1)
				 (sum-of-squares number-list-2))))
    (if  (zerop sum-squares-product)
	 0
	 (/ sum-cross-product (sqrt sum-squares-product)))))

;;; ---------------------------------------------------------------------------

(defun difference-list (number-list)
  "Takes a sequence of numbers and returns a sequence of differences 
from the mean.
Formula: xi = Xi - Mean (X)."
  (let* ((result ())
	 (numbers (remove-if 'null number-list))
	 (mean (mean numbers)))
    (dolist (number numbers (nreverse result)) 
      (push (- number mean) result))))

;;; ---------------------------------------------------------------------------

(defun sum-list (number-list)
  "Takes a sequence of numbers and returns their sum.
Formula: Sum(X)."
  (reduce #'+ (remove-if 'null number-list)))

;;; ---------------------------------------------------------------------------

(defun chi-square-2x2-counts (a b c d &optional (yates t))
  "Runs a chi-square test for association on a simple 2 x 2 table.  If `yates'
is nil, the correction for continuity is not done; default is t.

Returns the chi-square statistic and the significance of the value."
  (declare #-MCL (values chi2 p-value))
  (check-type a integer)
  (check-type b integer)
  (check-type c integer)
  (check-type d integer)
  ;; formula from Bohrnstedt and Knoke, page 329.  Just a shortcut for the
  ;; general chi-square formula
  (let ((N     (+ a b c d))
	(denom (* (+ a b) (+ c d) (+ a c) (+ b d)))
	(numer (- (* b c) (* a d))))
    (when yates
      (setf numer (- (abs numer) (/ N 2))))
    ;; finish doing numerator
    (setf numer (* N (square numer)))
    (let* ((chi2    (/ numer denom))
	   (p-value (chi-square-significance (float chi2) 1)))
      (values chi2 p-value))))

;;; ---------------------------------------------------------------------------

(defun chi-square-2x2 (v1 v2)
  "Performs a chi-square test for independence of the two variables, `v1' and
`v2.' These should be categorial variables with only two values; the function
will construct a 2x2 contingency table by counting the number of occurrences of
each combination of the variables.  See the manual for more details."
  ;; Better, non-consing algorithms certainly exist, but I don't have time now.
  (declare #-MCL (values chi-square significance contingency-table
		   v1-values v2-values))
  (multiple-value-bind (2x2-table v1-values v2-values)
      (make-contingency-table v1 v2)
    (unless (equal '(2 2) (array-dimensions 2x2-table))
      (error 'not-binary-variables))
    (let ((a (aref 2x2-table 0 0))
	  (b (aref 2x2-table 0 1))
	  (c (aref 2x2-table 1 0))
	  (d (aref 2x2-table 1 1)))
      (multiple-value-call #'values
	(chi-square-2x2-counts a b c d)
	2x2-table v1-values v2-values))))

;;; ---------------------------------------------------------------------------

(defun chi-square-rxc-counts (contingency-table)
  "Calculates the chi-square statistic and corresponding p-value for the given
contingency table.  The result says whether the row factor is independent of the
column factor.  Does not apply Yate's correction."
  (declare #-MCL (values chi-square p-value))
  (check-type contingency-table (array * 2))
  (destructuring-bind (rows cols) (array-dimensions contingency-table)
    (macrolet ((row-sum (row)
		 `(loop for jj from 0 below cols
			summing (aref contingency-table ,row jj)))
	       (col-sum (col)
		 `(loop for ii from 0 below rows
			summing (aref contingency-table ii ,col))))
      (let ((chi-2 0)
	    (total (loop for i from 0 below (array-total-size contingency-table)
			 summing (row-major-aref contingency-table i))))
	;; this re-calculates a lot of column sums, but that's okay for now.
	;; We'll worry about efficiency in the next release.
	(dotimes (i rows)
	  (let ((row-sum (row-sum i)))
	    (dotimes (j cols)
	      (let ((expected (/ (* row-sum (col-sum j)) total))
		    (observed (aref contingency-table i j)))
		(incf chi-2
		      (/ (square (- expected observed))
			 expected))))))
	(values chi-2 (chi-square-significance (float chi-2)
					       (* (- rows 1) (- cols 1))))))))

;;; ---------------------------------------------------------------------------

(defvar *way-too-big-contingency-table-dimension* 20)

(defun make-contingency-table (v1 v2)
  "Counts each unique combination of an element of `v1' and an element of `v2.'
Returns a two-dimensional table of integers."
  (declare #-MCL (values table v1-values v2-values))
  (let ((n (length v1)))
    (when (/= n (length v2))
      (error 'unmatched-sequences))
    ;; Faster implementations may be possible.
    (let ((v1-values (extract-unique-values v1))
	  (v2-values (extract-unique-values v2)))
      (when (or (> (length v1-values) *way-too-big-contingency-table-dimension*)
                (> (length v2-values) *way-too-big-contingency-table-dimension*))
        (cerror "Make it anyway" 'enormous-contingency-table 
                :size-of-table (max (length v1-values) (length v2-values))))
      (let ((table (make-array (list (length v1-values)
				     (length v2-values))
			       :element-type `(integer 0 ,n)
			       :initial-element 0)))
	;; construct contingency table
	(map nil #'(lambda (v1-event v2-event)
		     (let ((i (position v1-event v1-values))
			   (j (position v2-event v2-values)))
		       (incf (aref table i j))))
	     v1 v2)
	(values table v1-values v2-values)))))

;;; ---------------------------------------------------------------------------

(defun chi-square-rxc (v1 v2)
  "Performs a chi-square test for independence of the two variables, `v1' and
`v2.' These should be categorial variables; the function will construct a
contingency table by counting the number of occurrences of each combination of
the variables.  See the manual for more details."
  (declare #-MCL (values chi-square significance contigency-table
		   v1-values v2-values))
  (check-type v1 sequence)
  (check-type v2 sequence)
  (multiple-value-bind (table v1-values v2-values)
      (make-contingency-table v1 v2)
    (multiple-value-call #'values
      (chi-square-rxc-counts table)
      (g-test table nil nil)
      table v1-values v2-values)))

;;; ---------------------------------------------------------------------------

(defun g-test (contingency-table &optional expected-value-matrix (error-p t))

  "Calculates the G-test for a contingency table.  The formula for the
G-test statistic is

2 * sum[f_ij log [f_ij/f-hat_ij]]

where f_ij is the ith by jth cell in the table and f-hat_ij is the
expected value of that cell.  If an expected-value-matrix is supplied,
it must be the same size as table and it is used for expected values,
in which case the G-test is a test of goodness-of-fit.  If the
expected value matrix is unsupplied, it is calculated from using the
formula

e_ij = [f_i* * f_*j] / f_**

where f_i*, f_*j and f_** are the row, column and grand totals
respectively.  In this case, the G-test is a test of independence.  The degrees of freedom is the same as for the chi-square statistic and the significance is obtained by comparing "
  (flet ((doit (contingency-table &optional expected-value-matrix)
           (declare #-MCL (values g-score g-significance dof))
           (destructuring-bind (rows columns) (array-dimensions contingency-table)
             (let ((row-totals (make-list rows :initial-element 0))
	           (column-totals (make-list columns :initial-element 0))
	           (g-sum 0)
	           grand-total cell expected-value)
               (dotimes (row rows)
	         (dotimes (column columns)
	           (setf cell (aref contingency-table row column))
	           (incf (nth row row-totals) cell)
	           (incf (nth column column-totals) cell)))
               (setf grand-total (reduce #'+ row-totals))
               (dotimes (row rows)
	         (dotimes (column columns)
	           (setf cell (aref contingency-table row column))
	           (if expected-value-matrix
	             (setf expected-value (aref expected-value-matrix row column))
	             (setf expected-value (/ (* (nth row row-totals)
				                (nth column column-totals))
				             grand-total)))
	           (incf g-sum (* cell (log (/ cell expected-value))))))
               (let* ((g-score (* 2 g-sum))
	              (dof (* (- rows 1) (- columns 1)))
	              (g-sig (chi-square-significance g-score dof)))
	         (values g-score g-sig dof))))))
    ;; return :zero-cell if there's a divide-by-zero or an overflow on log zero and error-p is nil
    (if error-p       
      (doit contingency-table expected-value-matrix)
      (catch 'recover
        (handler-bind ((division-by-zero #'(lambda (condition)
                                             (declare (ignore condition))
                                             (throw 'recover (values :zero-cell nil nil))))
                       (floating-point-overflow #'(lambda (condition)
                                             (declare (ignore condition))
                                             (throw 'recover (values :zero-cell nil nil)))))
          (doit contingency-table expected-value-matrix))))))

;;; ---------------------------------------------------------------------------

(defun find-critical-value
       (p-function p-value &optional (x-tolerance .00001) (y-tolerance .00001))
  "Returns the critical value of some statistic.  The function `p-function'
should be a unary function mapping statistics---x values---to their
significance---p values.  The function will find the value of x such that the
p-value is `p-value.' The function works by binary search.  A secant method
might be better, but this seems to be acceptably fast.  Only positive values of
x are considered, and `p-function' should be monotonically decreasing from its
value at x=0.  The binary search ends when either the function value is within
`y-tolerance' of `p-value' or the size of the search region shrinks to less than
`x-tolerance.'"
  ;; The Allegro compiler barfs on this...
  ;; It generates this erroneous code...
  ;; (AND (TYPEP #:G3682 'REAL) (> (THE REAL #:G3682) (0)) ...)
  #-(or allegro lucid) (check-type p-value (real (0) (1)))
  (let* ((x-low 0.0)
	 (fx-low 1.0)
	 (x-high 1.0)
	 (fx-high (funcall p-function x-high)))
    ;; double up
    (do () (nil)
      ;; for general functions, we'd have to try the other way of bracketing,
      ;; and probably have another way to terminate if, say, y is not in the
      ;; range of f.
      (when (>= fx-low p-value fx-high)
	(return))
      (setf x-low x-high
	    fx-low fx-high
	    x-high (* 2.0 x-high)
	    fx-high (funcall p-function x-high)))
    ;; binary search
    (do () (nil)
      (let* ((x-mid  (/ (+ x-low x-high) 2.0))
	     (fx-mid (funcall p-function x-mid))
	     (y-diff (abs (- fx-mid p-value)))
	     (x-diff (- x-high x-low)))
	(when (or (< x-diff x-tolerance)
		  (< y-diff y-tolerance))
	  (return-from find-critical-value x-mid))
	;; Because significance is monotonically decreasing with x, if the
	;; function is above the desired p-value...
	(if (< p-value fx-mid)
	    ;; then the critical x is in the upper half
	    (setf x-low x-mid
		  fx-low fx-mid)
	    ;; otherwise, it's in the lower half
	    (setf x-high x-mid
		  fx-high fx-mid))))))

#+test
(defun test-find-critical-value ()
  (dolist (alpha '(.1 .05 .01))
    (format t "x to give alpha = ~4,2f is:  ~5,3f~%"
	    alpha
	    (find-critical-value #'(lambda (x) (gaussian-significance x :both))
				 alpha)))
  (dolist (dof '(2 5 10))
    (dolist (alpha '(.1 .05 .01))
      (format t "with ~2d dof, the t to give upper-tail alpha = ~4,2f is:  ~5,3f~%"
	      dof
	      alpha
	      (find-critical-value #'(lambda (x) (students-t-significance x dof :positive))
				   alpha)))))



;;; ***************************************************************************
;;; EOF
