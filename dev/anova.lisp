;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

Author: EKSL Utilities

|#

(in-package #:metabang.math)

(define-statistic anova-one-way-variables ()
   ((anova-table)
    (means-list)
    (scheffe-table)
    (sst-alt))
   ()
   ((iv 'sequence)
    (dv 'sequence))
   (iv dv &optional (scheffe-tests-p t) confidence-intervals)
   "Performs a one-way analysis of variance (ANOVA) on the input data, which
should be two equal-length sequences: `iv' is the independent variable,
represented as a sequence of categories or group identifiers, and `dv' is the
dependent variable, represented as a sequence of numbers.  The `iv' variable
must be ``sorted,'' meaning that AAABBCCCCCDDDD is okay but ABCDABCDABDCDC is
not, where A, B, C and D are group identifiers.  Furthermore, each group should
consist of at least 2 elements.

The significance of the result indicates that the group means are not all equal;
that is, at least two of the groups have significantly different means.  If
there were only two groups, this would be semantically equivalent to an
unmatched, two-tailed t-test, so you can think of the one-way ANOVA as a
multi-group, two-tailed t-test.

This function returns five values: 1.  an ANOVA table; 2.  a list a group
means; 3.  either a Scheffe table or nil depending on `scheffe-tests-p'; and
4.  an alternate value for SST. 5. a list of confidence intervals in the form
`(,mean ,lower ,upper) for each group, if `confidence-intervals' is a number
between zero and one, giving the kind of confidence interval, such as 0.9. The
fourth value is only interesting if you think there are numerical accuracy
problems; it should be approximately equal to the SST value in the ANOVA
table.  This function differs from `anova-one-way-groups' only in its input
representation.  See the manual for more information."
   
   #+DEBUG
   (progn
     (check-type iv sequence)
     (check-type dv sequence)
     (check-type iv sequence)
     (check-type dv sequence))
   (let ((n1 (length iv))
	 (n2 (length dv)))
     (when (/= n1 n2) (error 'unmatched-sequences))
     (case n2
       (0 (error 'no-data))
       (1 (error 'insufficient-data)))
     ;; these variable names aren't intuitive without reading the discussion in
     ;; the manual.
     (let* ((n   n2)
	    (gt  (reduce #'+ dv))		; grand total
	    (a   (reduce #'+ dv :key #'square))
	    (b   (/ (square gt) n))
	    (c   0)
	    (ng  0)				; number of groups
	    (group-means nil)
	    (group-sizes nil))
       ;; the following loop calculates c and the group means by going through
       ;; the iv and dv looking for group boundaries.  using '#:new as the
       ;; current group guarantees that the first element will start a new group,
       ;; because it can't be eql to anything.
       (let ((current-group '#:new) group-sum group-length)
	 ;; use `map' `nil' because inputs are sequences, not even guaranteed to be
	 ;; of the same type.  `loop' would be more efficient, but would require a
	 ;; four-leaf type tree.  yuck!
	 (map nil #'(lambda (key value)
		      (unless (eql key current-group)
		        ;; process end of group and begin new group
		        (unless (null group-sum)
			  ;; a null group-sum means there is no previous group to end
			  (incf ng)
			  (when (< group-length 2) (error 'insufficient-data))
			  (push (/ group-sum group-length) group-means)
			  (push group-length group-sizes)
			  (incf c (/ (square group-sum) group-length)))
		        (setf current-group key
			      group-sum 0
			      group-length 0))
		      ;; normal group processing
		      (incf group-sum value)
		      (incf group-length))
	      iv dv)
	 ;; process last group
	 (incf ng)
	 (when (< group-length 1) (error 'insufficient-data))
	 (push (/ group-sum group-length) group-means)
	 (push group-length group-sizes)
	 (setf group-means (nreverse group-means))
	 (setf group-sizes (nreverse group-sizes))
	 (incf c (/ (square group-sum) group-length)))
       (let ((sst (- a b))
	     (ssg (- c b))
	     (sse (- a c)))
	 (when (zerop sse) (error 'zero-variance))
	 ;; the following two computations are for error checking.  see discussion
	 ;; in the manual.  sst-alt is returned as the second value.
	 (let* ((grand-mean (/ gt n))
	        (sst-alt    (reduce #'+ dv :key
				    #'(lambda (elt) (square (- elt grand-mean))))))
	   ;; final calculations
	   (let* ((dfg (1- ng))
		  (dfe (- n ng))
		  (dft (+ dfg dfe))
		  (msg (/ ssg dfg))
		  (mse (/ sse dfe)))
	     ;; yes, it's correct to compute the f-significance by a one-tailed
	     ;; test, because if h0 is false, f is known to be biased on the
	     ;; large side, and so we're only interested in the upper tail of the
	     ;; f distribution.
	     (let* ((f (/ msg mse))
		    (p (f-significance (float f) dfg dfe t)))
	       (values `((,dfg ,ssg ,msg ,f ,p)
			 (,dfe ,sse ,mse)
			 (,dft ,sst))
		       group-means
		       (when scheffe-tests-p
			 (scheffe-tests group-means group-sizes mse dfe))
		       sst-alt
		       (when (and (numberp confidence-intervals)
				  (< 0f0 confidence-intervals 1f0))
			 (let ((current-group '#:new)
			       (group-sums-squared nil))
			   (map nil #'(lambda (key value)
				        (unless (eql key current-group)
					  ;; process end of group and begin new group
					  (setf current-group key)
					  (push 0 group-sums-squared))
				        ;; normal group processing
				        (incf (car group-sums-squared) (square value)))
			        iv dv)
			   (setf group-sums-squared (nreverse group-sums-squared))
			   (mapcar #'(lambda (s2 mean n)
                                       (multiple-value-list
					(confidence-interval-t-summaries
					 mean
					 (- n 1)
					 ;; standard shorthand for variance,
					 ;; combined with division by n to get
					 ;; std.  err.
					 (sqrt (/ (- s2 (* n mean mean)) (* n (- n 1))))
					 confidence-intervals)))
				   group-sums-squared
				   group-means
				   group-sizes)))))))))))

;;; ---------------------------------------------------------------------------

(defun anova-one-way-groups (data &optional (scheffe-tests-p t) confidence-intervals)
  "Performs a one-way analysis of variance (ANOVA) on the `data,' which should
be a sequence of sequences, where each interior sequence is the data for a
particular group.  Furthermore, each sequence should consist entirely of
numbers, and each should have at least 2 elements.

The significance of the result indicates that the group means are not all equal;
that is, at least two of the groups have significantly different means.  If
there were only two groups, this would be semantically equivalent to an
unmatched, two-tailed t-test, so you can think of the one-way ANOVA as a
multi-group, two-tailed t-test.

This function returns five values: 1.  an ANOVA table; 2.  a list a group means;
3.  either a Scheffe table or nil depending on `scheffe-tests-p'; 4.  an
alternate value for SST; and 5.  a list of confidence intervals in the form
`(,mean ,lower ,upper) for each group, if `confidence-intervals' is a number between
zero and one, giving the kind of confidence interval, such as 0.9.  The fourth
value is only interesting if you think there are numerical accuracy problems; it
should be approximately equal to the SST value in the ANOVA table.  This
function differs from `anova-one-way-variables' only in its input
representation.  See the manual for more information."
  (check-type data sequence)
  (case (length data)
    (0 (error 'no-data))
    (1 (error 'insufficient-data)))
  (unless (every #'(lambda (group) (< 1 (length group))) data)
    (error 'insufficient-data))
  ;; These variable names aren't intuitive without reading the discussion in the
  ;; manual.
  (let* ((n   (reduce #'+ data :key #'length))
	 (tg  (reduce #'+ data :key #'(lambda (group) (reduce #'+ group))))
	 (a   (reduce #'+ data :key #'(lambda (group)
					(reduce #'+ group :key #'square))))
	 (b   (/ (square tg) n))
	 (c   (reduce #'+ data :key #'(lambda (group)
					(/ (square (reduce #'+ group))
					   (length group)))))
	 (sst (- a b))
	 (ssg (- c b))
	 (sse (- a c)))
    ;; the following two computations are for error checking.  see discussion in
    ;; the manual.  sst-alt is returned as the last value.
    (let* ((grand-mean (/ tg n))
	   (sst-alt    (reduce #'+ data :key
			       #'(lambda (group)
				   (reduce #'+ group :key
					   #'(lambda (x)
					       (square (- x grand-mean))))))))
      (when (zerop sse)
	(error 'zero-variance))
      ;; final calculations
      (let* ((dfg (1- (length data)))
	     (dfe (- n (length data)))
	     (dft (+ dfg dfe))
	     (msg (/ ssg dfg))
	     (mse (/ sse dfe)))
	;; yes, it's correct to compute the f-significance by a one-tailed test,
	;; because if h0 is false, f is known to be biased on the large side,
	;; and so we're only interested in the upper tail of the f distribution.
	(let* ((f (/ msg mse))
	       (p (f-significance (float f) dfg dfe t)))
	  ;; here's where we start consing.
	  (let ((group-means (map 'list #'mean data)))
	    (values `((,dfg ,ssg ,msg ,f ,p)
		      (,dfe ,sse ,mse)
		      (,dft ,sst))
		    group-means
		    (when scheffe-tests-p
		      (scheffe-tests group-means (map 'list #'length data)
				     mse dfe))
		    sst-alt
		    (when (and (numberp confidence-intervals)
			       (< 0f0 confidence-intervals 1f0))
		      (map 'list #'(lambda (group)
				     (multiple-value-list
				       (confidence-interval-t group confidence-intervals)))
			      data)))))))))

;;; ---------------------------------------------------------------------------

(defun print-anova-table (anova-table &optional (stream *standard-output*))
  "Prints `anova-table' on `stream.'"
  (case (length anova-table)
    ;; one-way anova has 3 lines
    (3 (destructuring-bind ((dfg ssg msg f p)
			    (dfe sse mse)
			    (dft sst))
			   anova-table
	 (format stream "~2&")
	 (format stream "~14@<~a~>~3@{~14:@<~a~>~}~%" "source of" "degrees of" "sum of" "mean")
	 (format stream "~14@<~a~>~5@{~14:@<~a~>~}~%" "variation" "freedom" "squares" "square" "f" "p")
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "group" dfg ssg msg f p)
	 (format stream "~14@<~a~>~14:@<~5:d~>~2@{ ~12,2f ~}~%" "error" dfe sse mse)
	 (format stream "~14@<~a~>~14:@<~5:d~>~1@{ ~12,2f ~}~%" "total" dft sst)
	 (format stream "~%")))
    ;; two-way anova has 5 lines
    (5 (destructuring-bind ((dfab ssab msab fab pab)
			    (dfa ssa msa fa pa)
			    (dfb ssb msb fb pb)
			    (dfe sse mse)
			    (dft sst))
			   anova-table
	 (format stream "~2&")
	 (format stream "~14@<~a~>~3@{~14:@<~a~>~}~%" "source of" "degrees of" "sum of" "mean")
	 (format stream "~14@<~a~>~5@{~14:@<~a~>~}~%" "variation" "freedom" "squares" "square" "f" "p")
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "interaction" dfab ssab msab fab pab)
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "column"         dfa ssa msa fa pa)
	 (format stream "~14@<~a~>~14:@<~5:d~>~3@{ ~12,2f ~} ~12,10f~%" "row"      dfb ssb msb fb pb)
	 (format stream "~14@<~a~>~14:@<~5:d~>~2@{ ~12,2f ~}~%" "error"       dfe sse mse)
	 (format stream "~14@<~a~>~14:@<~5:d~>~1@{ ~12,2f ~}~%" "total"       dft sst)
	 (format stream "~%")))
    (t (error "anova tables have either 3 or 5 lines:  ~s" anova-table))))

;;; ---------------------------------------------------------------------------

(defun anova-two-way-groups (data-array)
  "Calculates the analysis of variance when there are two factors that may
affect the dependent variable.  Because the input is represented as an array, we
can refer to these two factors as the row-effect and the column effect.  Unlike
the one-way ANOVA, there are mathematical difficulties with the two-way ANOVA if
there are unequal cell sizes; therefore, we require all cells to be the same
size, and so the input is a three-dimensional array.

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-variables' only in its input
representation.  See the manual for further discussion of analysis of variance."
  (destructuring-bind (i j k) (array-dimensions data-array)
    ;; computing formulas from devore, page 387, except i've pulled out the
    ;; common subexpressions.  i know there's a lot of rightward creep here,
    ;; because i used `let' instead of `let*' but i like making the data
    ;; dependencies clear.
    (let ((ijk (* i j k))
	  (ij  (* i j))
	  (jk  (* j k))
	  (ik  (* i k)))
      (let ((x+ (loop for index from 0 below ijk
		      summing (row-major-aref data-array index)))
	    (x2+ (loop for index from 0 below ijk
		       summing (square (row-major-aref data-array index))))
	    (e2  (loop for ii from 0 below i summing
		       (loop for jj from 0 below j summing
			     (square (loop for kk from 0 below k
					   summing (aref data-array ii jj kk))))))
	    (a2  (loop for ii from 0 below i summing
		       (square (loop for jj from 0 below j summing
				     (loop for kk from 0 below k
					   summing (aref data-array ii jj kk))))))
	    (b2  (loop for jj from 0 below j summing
		       (square (loop for ii from 0 below i summing
				     (loop for kk from 0 below k
					   summing (aref data-array ii jj kk)))))))
	(let ((x+2/ijk (/ (square x+) ijk)))
	  ;; should we multiply through by ijk?  it doesn't save us computation, but
	  ;; it may improve numerical accuracy.  
	  (let ((sst (- x2+ x+2/ijk))
		(sse (- x2+ (/ e2 k)))
		(ssa (- (/ a2 jk) x+2/ijk))
		(ssb (- (/ b2 ik) x+2/ijk)))
	    (let ((ssab (- sst ssa ssb sse)))
	      (let ((df-t  (- ijk 1))
		    (df-e  (- ijk ij))		; ij*(k-1)
		    (df-a  (- i 1))
		    (df-b  (- j 1)) 
		    (df-ab (- ij i j -1)))	; (i-1)*(j-1)
		(let ((mse  (/ sse df-e))
		      (msa  (/ ssa df-a))
		      (msb  (/ ssb df-b))
		      (msab (/ ssab df-ab)))
		  (let ((fa  (/ msa mse))
			(fb  (/ msb mse))
			(fab (/ msab mse)))
		    (let ((pa  (f-significance (float fa) df-a df-e t))
			  (pb  (f-significance (float fb) df-b df-e t))
			  (pab (f-significance (float fab) df-ab df-e t)))
		      `((,df-ab ,ssab ,msab ,fab ,pab)
			(,df-a ,ssa ,msa ,fa ,pa)
			(,df-b ,ssb ,msb ,fb ,pb)
			(,df-e ,sse ,mse)
			(,df-t ,sst)))))))))))))

;;; ---------------------------------------------------------------------------

(defun make-3d-table (dv iv1 iv2)
  "Collects the `dv' values for each unique combination of an element of `v1'
and an element of `v2.' Returns a three-dimensional table of dv values."
  (let ((n (length dv)))
    (unless (= n (length iv1) (length iv2))
      (error 'unmatched-sequences))
    ;; faster implementations may be possible.
    (let ((iv1-values (extract-unique-values iv1))
	  (iv2-values (extract-unique-values iv2)))
      (let ((k (let ((k-temp 0)
		     (iv1-first (elt iv1-values 0))
		     (iv2-first (elt iv2-values 0)))
		 (map nil #'(lambda (iv1-elt iv2-elt)
			      (when (and (eql iv1-first iv1-elt)
					 (eql iv2-first iv2-elt))
				(incf k-temp)))
		      iv1
		      iv2)
		 k-temp)))
	(let ((table (make-array (list (length iv1-values)
				       (length iv2-values)
				       k)
				 :element-type 't
				 :initial-element nil)))
          ;; i do not understand how a pairwise mapping across these iv2 and iv1 is supposed to
          ;; fill the entire table. don't we have to do a full map? maybe it is just to late to
          ;; be thinking about this. check it tomorrow.
	  ;; construct data table
	  (map nil #'(lambda (dv-value iv1-event iv2-event)
		       (let ((i (position iv1-event iv1-values))
			     (j (position iv2-event iv2-values)))
			 ;; have to search for the first unfilled position.
			 ;; could be made more efficient with row-major-aref.
			 (let ((pos (dotimes (x k)
				      (when (null (aref table i j x))
					(return x)))))
			   (if (null pos)
			       (error 'unequal-cell-sizes)
			       (setf (aref table i j pos) dv-value)))))
	       dv iv1 iv2)
	  ;; check that there aren't any nil's left
	  (loop for x from 0 below (array-total-size table)
		when (null (row-major-aref table x))
		do (error 'unequal-cell-sizes))
	  table)))))

;;; ---------------------------------------------------------------------------

(define-statistic anova-two-way-variables-unequal-cell-sizes ()
    ((anova-table)
     (ab-matrix)
     (row-totals)
     (column-totals)
     (grand-totla)
     (a-labels)
     (b-labels))
   ()
   ((iv1 'sequence)
    (iv2 'sequence)
    (dv 'sequence))
   (iv1 iv2 dv)
   "Calculates the analysis of variance when there are two factors that may
affect the dependent variable, specifically `iv1' and `iv2.'

Unlike the one-way ANOVA, there are mathematical difficulties with the two-way
ANOVA if there are unequal cell sizes. This function differs fron the standard
two-anova by (1) the use of cell means as single scores, (2) the division of
squared quantities by the number of cell means contributing to the quantity
that is squared and (3) the multiplication of a \"sum of squares\" by the harmonic
mean of the sample sizes.

The result of the analysis is an anova-table, as described in the manual.
See the manual for further discussion of analysis of
variance.  The row effect is `iv1' and the
column effect is `iv2.'"

   
   (let ((iv1-hash (make-hash-table))
	 iv2-hash ;cell
	 num-a num-b a-count b-count
	 (harmonic-mean 0)
	 (within-groups-subtractand 0)
	 (sum-squared-cells 0)
	 a-list b-list)
     ;;accumulate cell summations
     ;; in the following a refers to columns, b to rows
     ;; put each value in a nested hash table, selected by a and b
     ;; the cells contain the sum of elements and the number of elements
     (loop
      for a in iv1
      for b in iv2
      for c in dv
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
     ;; build the matrix by visiting each cell in each hash table and 
     ;; calculating the means, at the same time, calculate the
     ;; harmonic mean of the sample sizes.
     ;; the harmonic mean is given by the formula
     ;; ab/sum\{1/sij\} where ab is the number of rows times the number
     ;; of columns and sij is the number of elements in cell i,j
     ;; the within-groups-subtractand is calculated here because the
     ;; cell counts are discarded after the ab-matrix is calculated and
     ;; sij is in the denominator of the subtractand for the ss within
     ;; groups.
     (let (row-totals
	   column-totals
	   (grand-total 0)
	   squared-row-totals
	   squared-column-totals
	   squared-grand-total
	   ab-matrix
           #+test
           cell-counts
	   a-value b-value)
       (setf ab-matrix (make-array `(,num-b ,num-a) :initial-element 0))
       #+test
       (setf cell-counts (make-array `(,(1+ num-b) ,(1+ num-a)) :initial-element 0))
       (setf a-count 0)
       (dolist (a-key a-list)
	 (setf a-value (gethash a-key iv1-hash))
	 (when (/= num-b (hash-table-count a-value))
	   (error 'missing-cell))
	 (setf b-count 0)
	 (dolist (b-key b-list)
	   (setf b-value (gethash b-key a-value))
           ;; how can this ever be true since b-key is mapped over b-list? - westy
	   (when (not (member b-key b-list))
	     (error 'missing-cell))
	   (setf (aref ab-matrix b-count a-count)
	         (apply #'/ b-value))
           #+test
	   (setf (aref cell-counts b-count a-count)
                 (second b-value))
	   (incf harmonic-mean (/ 1 (second b-value)))
	   (incf within-groups-subtractand
		 (/ (square (first b-value))
		    (second b-value)))
	   (incf b-count))
	 (incf a-count))
       (setf harmonic-mean (* num-a num-b (/ 1 harmonic-mean)))
       (setf row-totals (make-list num-b :initial-element 0))
       (setf column-totals (make-list num-a :initial-element 0))
       (let (cell #+test cell-count)
         (dotimes (a num-a)
	   (dotimes (b num-b)
	     (setf cell (aref ab-matrix b a))
	     (incf (nth b row-totals) cell)
	     (incf (nth a column-totals) cell)
	     (incf sum-squared-cells (square cell))
             #+test
             (progn
               ;; testing
               (setf cell-count (aref cell-counts b a))
               ;; put the totals in the fringe of the array
               (incf (aref cell-counts b num-a) cell-count)
	       (incf (aref cell-counts num-b a) cell-count)
               (incf (aref cell-counts num-b num-a) cell-count)))))
           
       (setf grand-total (reduce #'+ row-totals))
       (setf squared-row-totals (reduce #'+ (mapcar #'square row-totals)))
       (setf squared-column-totals (reduce #'+ (mapcar #'square column-totals)))
       (setf squared-grand-total (square grand-total))
       ;; compute the anova values from the summaries caculated above and
       ;; return them in an anova table-like list.
       (let* ((ab (* num-a num-b))
	      (abs2 (reduce #'+ (mapcar #'square dv)))
	      (ssa (* harmonic-mean (- (/ squared-column-totals num-b)
				       (/ squared-grand-total ab))))
	      (ssb (* harmonic-mean (- (/ squared-row-totals num-a)
				       (/ squared-grand-total ab))))
	      (ssaxb (* harmonic-mean
		        (+ (- sum-squared-cells
			      (/ squared-column-totals num-b)
			      (/ squared-row-totals num-a))
			   (/ squared-grand-total ab))))
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
	 (values `((,dfaxb ,ssaxb ,msaxb ,faxb ,paxb)
		   (,dfa ,ssa ,msa ,fa ,pa)
		   (,dfb ,ssb ,msb ,fb ,pb)
		   (,dfe ,sse ,mse)
		   (,dft ,sst))
		 ab-matrix row-totals column-totals grand-total
		 a-list b-list #+test cell-counts)))))

;;; ---------------------------------------------------------------------------

(define-statistic anova-two-way-variables ()
    ((anova-table))
    ()
   ((dv 'sequence)
    (iv1 'sequence)
    (iv2 'sequence))
   (dv iv1 iv2)
    "Calculates the analysis of variance when there are two factors that may
affect the dependent variable, specifically `iv1' and `iv2.' Unlike the one-way
ANOVA, there are mathematical difficulties with the two-way ANOVA if there are
unequal cell sizes; therefore, we require all cells to be the same size; that
is, the same number of values (of the dependent variable) for each combination
of the independent factors.

The result of the analysis is an anova-table, as described in the manual.  This
function differs from `anova-two-way-groups' only in its input representation.
See the manual for further discussion of analysis of variance.  
The row effect is `iv1' and the column effect is `iv2.'"
  (check-type dv sequence)
  (check-type iv1 sequence)
  (check-type iv2 sequence)
  (let ((table (make-3d-table dv iv1 iv2)))
    (anova-two-way-groups table)))

;;; ---------------------------------------------------------------------------

