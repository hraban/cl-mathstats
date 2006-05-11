;;; -*- Mode:Common-Lisp; Package: metabang.math; Base:10 -*-
;;;; *-* Last-edit: Tuesday, June 14, 2005 09:43:41; Edited-By: Gary *-* 

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Matrix Functions                             *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;

;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice of EKSL, this paragraph and the one following appear
;;; in all copies and in supporting documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is advised of the possiblity of
;;; such damages.

;;; For more information write to clasp-support@cs.umass.edu

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-21-90 File Created.  (DFISHER)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package #:metabang.math)

(defmacro check-type-of-arg (arg-name predicate type-string &optional error-type-name)
  "Generate error if the value of ARG-NAME doesn't satisfy PREDICATE.
PREDICATE is a function name (a symbol) or an expression to compute.
TYPE-STRING is a string to use in the error message, such as \"a list\".
ERROR-TYPE-NAME is a keyword that tells condition handlers what type was desired."
    (and (null error-type-name)
	 (symbolp predicate)
	 (setq error-type-name predicate))
    `(do () (,(cond ((symbolp predicate)
                     `(,predicate ,arg-name))
                    (t predicate))
             ,arg-name)
	 (setf ,arg-name
	       (error 'simple-type-error
		       :format-string "The argument ~A was ~S, which is not ~A."
		       :format-arguments (list ',arg-name ,arg-name ',type-string)))))
#|
(defsubst neq (x y)
  "T if X and Y are not the same object."
  (not (eq x y)))
|#

(defun scalar-matrix-multiply (scalar matrix)
  "Multiplies a matrix by a scalar value in the form M[i,j] = s*M[i,j]."
  (let* ((rows (array-dimension matrix 0))
	 (cols (array-dimension matrix 1))
	 (result (make-array (list rows cols))))
    (dotimes (i (array-dimension matrix 0))
      (dotimes (j (array-dimension matrix 1))
	(setf (aref result i j) (* scalar (aref matrix i j)))))
    result))



(defun 1-or-2d-arrayp (array)
  (and (arrayp array)
       (or (= (array-rank array) 1)
	   (= (array-rank array) 2))))



;;; Convert a 2d array into a list of lists of the elements
(defun list-2d-array (array)
  (check-type-of-arg array
	     (and (arrayp array)
		  (= (array-rank array) #o2))
	     "a two-dimensional array")
  (do ((i #o0 (1+ i))
       (dim-1 (array-dimension array #o0))
       (dim-2 (array-dimension array #o1))
       (list ()))
      ((>= i dim-1)
       (nreverse (the list list)))
    (push
      (do ((j #o0 (1+ j))
	   (list ()))
	  ((>= j dim-2)
	   (nreverse (the list list)))
	(push (aref array i j) list))
      list))) 

;;; Fill up a 2d array from a list.  Like FILLARRAY,
;;; the lists can wrap around as needed.
(defun fill-2d-array (array list)
  (check-type-of-arg array
	     (and (arrayp array)
		  (= (array-rank array) #o2))
	     "a two-dimensional array")
  (do ((i #o0 (1+ i))
       (dim-1 (array-dimension array #o0))
       (dim-2 (array-dimension array #o1))
       (l list (cdr l))
       (sublist))
      ((>= i dim-1))
    (and (null l)
	 (setq l list))
    (setq sublist (car l))
    (do ((j #o0 (1+ j))
	 (l sublist (cdr l)))
	((>= j dim-2))
      (and (null l)
	   (setq l sublist))
      (setf (aref array i j) (car l))))) 

;;; Multiply two matrices into a third.  
;;; A 1d array of dimension N is treated as a Nx1 array.
(defun multiply-matrices (matrix-1 matrix-2 &optional matrix-3 &aux saved-matrix-3)
  "Multiply matrices MATRIX-1 and MATRIX-2, storing into MATRIX-3 if supplied.
If MATRIX-3 is not supplied, then a new (ART-Q type) array is returned, else
MATRIX-3 must have exactly the right dimensions for holding the result of the multiplication.
Both MATRIX-1 and MATRIX-2 must be either one- or two-diimensional.
The first dimension of MATRIX-2 must equal the second dimension of MATRIX-1, unless MATRIX-1
is one-dimensional, when the first dimensions must match (thus allowing multiplications of the
form VECTOR x MATRIX)"
  (check-type-of-arg matrix-1 1-or-2d-arrayp "a one- or two-dimensional array")
  (check-type-of-arg matrix-2 1-or-2d-arrayp "a one- or two-dimensional array")
  (check-type-of-arg matrix-3
	     (or (null matrix-3)
		 (1-or-2d-arrayp matrix-3))
	     "a one- or two-dimensional array or NIL")
  (let ((dim-1-1 (if (= (array-rank matrix-1) #o1) #o1 (array-dimension matrix-1 #o0)))
	(dim-1-2 (if (= (array-rank matrix-1) #o1)
		     (array-dimension matrix-1 #o0)
		     (array-dimension matrix-1 #o1)))
	(dim-2-1 (array-dimension matrix-2 #o0))
	(dim-2-2 (if (= (array-rank matrix-2) #o1) #o1 (array-dimension matrix-2 #o1)))
	(dim-3-1 (when matrix-3
		   (if (= (array-rank matrix-3) #o1) #o1 (array-dimension matrix-3 #o0))))
	(dim-3-2 (when matrix-3
		   (if (= (array-rank matrix-3) #o1)
		       (array-dimension matrix-3 #o0)
		       (array-dimension matrix-3 #o1)))))
    (unless (= dim-2-1 dim-1-2)
      (error nil "The ~~Dx~D matrix ~S and the ~Dx~D matrix ~S cannot be multiplied~"
	      dim-1-1 dim-1-2 matrix-1 dim-2-1 dim-2-2 matrix-2))
    (if matrix-3
	(if (and (= dim-1-1 dim-3-1)
		 (= dim-2-2 dim-3-2))
	    ;; We have a destination; see if it's the same as one of the sources.
	    ;; If it is, substitute a temporary for the destination.  We only check
	    ;; EQness, not displacements.
	    (when (or (eq matrix-3 matrix-1)
		      (eq matrix-3 matrix-2))
	      (setq saved-matrix-3 matrix-3
		    matrix-3 (make-array (array-dimensions matrix-3))))
	    (error nil
		    "The ~~Dx~D matrix ~S is not the right size for multiplying the
~Dx~D matrix ~S and the
~Dx~D matrix ~S.~"
		    dim-3-1 dim-3-2 matrix-3 dim-1-1 dim-1-2 matrix-1 dim-2-1 dim-2-2 matrix-2))
	;; We don't make a 1xn matrix here since the user probably wants a vector result.
	(setq matrix-3
	      (make-array (if (= (array-rank matrix-1) #o1)
			      dim-2-2
			      (list dim-1-1 dim-2-2)))))
    ;; Make indirect arrays to any vectors, so can use ar-2 everywhere below.
    (let ((mat-1 (if (= #o2 (array-rank matrix-1))
		     matrix-1
		     (make-array (list dim-1-1 dim-2-1)
				 :displaced-to matrix-1)))
	  (mat-2 (if (= #o2 (array-rank matrix-2))
		     matrix-2
		     (make-array (list dim-2-1 dim-2-2)
				 :displaced-to matrix-2)))
	  (mat-3 (if (= #o2 (array-rank matrix-3))
		     matrix-3
		     (make-array (list dim-1-1 dim-2-2)
				 :displaced-to matrix-3))))
      ;; Do the actual multiplication
      (dotimes (i dim-1-1)
	(dotimes (j dim-2-2)
	  (setf (aref mat-3 i j)
		(do ((k #o0 (1+ k))
		     (sum #o0 (+ sum (* (aref mat-1 i k) (aref mat-2 k j)))))
		    ((>= k dim-2-1)
		     sum)))))

      ;; If we substituted a temporary above, copy the result into the saved
      ;; destination and return the temporary to free storage.
      (when saved-matrix-3
	(dotimes (i (array-total-size matrix-3))
	  (setf (row-major-aref saved-matrix-3 i)
		(row-major-aref matrix-3 i)))
	(setq matrix-3 saved-matrix-3))))
  matrix-3)

;;; Gauss-Jordan inversion
(defun invert-matrix (matrix &optional into-matrix)
  "If matrix is singular returns nil, else returns its inverse.
   If into-matrix is supplied, inverse is returned in it,
    otherwise a new array is created."
  (let* ((dims (array-dimensions matrix))
	 (rows (car dims))
	 (cols (cadr dims)))
    (unless (= rows cols)
      (error () "Matrix ~s not square" matrix))
    (if (null into-matrix)
	(setq into-matrix (make-array dims))
	(let ((dims-into (array-dimensions into-matrix)))
	  (unless (and (= rows (car dims-into)) (= cols (cadr dims-into)))
	    (error () "into-matrix ~s does not have same dimensions as matrix ~s" into-matrix
		   matrix))))
    (let ((xcols (+ rows cols))
	  (vv (make-array rows)))
      (dotimes (r rows)
	(let ((v (make-array xcols :initial-element #o0)))
	  (setf (aref v (+ r rows)) #o1)
	  (dotimes (c cols)
	    (setf (aref v c) (aref matrix r c)))
	  (setf (aref vv r) v)))
      (dotimes (r rows)
	(let ((imax #o-1)
	      (vmax #o-1))
	  (do ((i r (1+ i)))
	      ((= i rows))
	    (let ((v (abs (aref (aref vv i) r))))
	      (cond
		((> v vmax) (setq vmax v) (setq imax i)))))
	  (cond ((zerop vmax) (return-from invert-matrix ())))
	  (cond ((not (= r imax))
		 (let ((exch-temp (aref vv r)))
		   (setf (aref vv r) (aref vv imax))
		   (setf (aref vv imax) exch-temp)))))
	(let ((pivot-row (aref vv r)))
	  (do ((d (aref pivot-row r))
	       (i (1+ r) (1+ i)))
	      ((= i xcols))
	    (setf (aref pivot-row i)
		  (/ (aref pivot-row i) d)))
	  (do ((i (1+ r) (1+ i)))
	      ((= i rows))
	    (let ((row (aref vv i)))
	      (do ((d (aref row r))
		   (j (1+ r) (1+ j)))
		  ((= j xcols))
		(setf (aref row j) (- (aref row j) (* d (aref pivot-row j)))))))))
      (do ((r (1- rows) (1- r)))
	  ((= r #o0))
	(let ((pivot-row (aref vv r)))
	  (do ((i (1- r) (1- i)))
	      ((< i #o0))
	    (let ((row (aref vv i)))
	      (do ((d (aref row r))
		   (j rows (1+ j)))
		  ((= j xcols))
		(setf (aref row j) (- (aref row j) (* d (aref pivot-row j)))))))))
      (dotimes (r rows)
	(let ((v (aref vv r)))
	  (dotimes (c cols)
	    (setf (aref into-matrix r c) (aref v (+ c cols)))))))
    (values into-matrix)))

(defun matrix-norm (matrix)
  "Returns the norm of matrix.
   The norm is the maximum over the rows of the sum of the abs of the columns."
  (let* ((dim (array-dimensions matrix))
         (rows (car dim))
         (cols (cadr dim))
         (m 0))
    (dotimes (r rows)
      (let ((s 0))
        (dotimes (c cols)
          (incf s (float (abs (aref matrix r c)))))
        (if (> s m) (setf m s))))
    m))

(defun invert-matrix-iterate (matrix &optional into-matrix)
  "If matrix is singular returns nil, else returns the inverse of matrix.
   Uses iterative improvement until no further improvement is possible."
  (let* ((dim (array-dimensions matrix))
	 (rows (car dim))
	 (cols (cadr dim)))
    (unless (= rows cols)
      (error () "matrix ~s not square" matrix))
    (prog ((d (invert-matrix matrix into-matrix))
	   (f (make-array dim))
	   (dt (make-array dim))
	   n
	   nt)
	  (unless d (return nil))
	  (dotimes (r rows)
	    (dotimes (c cols)
	      (let ((s (if (= r c) #o1 #o0)))
		(dotimes (i rows)
		  (decf s (* (aref matrix r i) (aref d i c))))
		(setf (aref f r c) s))))
	  (setq n (matrix-norm f))
       la
	  (dotimes (r rows)
	    (dotimes (c cols)
	      (let ((s #o0))
		(dotimes (i rows)
		  (incf s (* (aref d r i) (aref f i c))))
		(setf (aref dt r c) (+ s (aref d r c))))))
	  (dotimes (r rows)
	    (dotimes (c cols)
	      (let ((s (if (= r c) #o1 #o0)))
		(dotimes (i rows)
		  (decf s (* (aref matrix r i) (aref dt i c))))
		(setf (aref f r c) s))))
	  (setq nt (matrix-norm f))
	  (cond
	    ((< nt n) (setq n nt) (setq nt d) (setq d dt) (setq dt nt) (go la))
	    ((null into-matrix) (return d))
	    ((eq d into-matrix) (return d))
	    (t
	     (dotimes (r rows)
	       (dotimes (c cols)
		 (setf (aref into-matrix r c) (aref d r c))))
	     (return into-matrix)))))) 

(defun transpose-matrix (matrix &optional into-matrix &aux dim-1 dim-2)
  (check-type-of-arg matrix
	     (and (arrayp matrix)
		  (= (array-rank matrix) #o2))
	     "a two-dimensional array")
  (setq dim-1 (array-dimension matrix #o0)
	dim-2 (array-dimension matrix #o1))
  (if into-matrix
      (or
	(and (eq dim-1 (array-dimension into-matrix #o1))
	     (eq dim-2 (array-dimension into-matrix #o0)))
	(error nil "~s wrong dimensions for transpose of ~s" into-matrix matrix))
      (setq into-matrix (make-array (list dim-2 dim-1))))
  (if (eq matrix into-matrix)			;special case
      (dotimes (i dim-1)
	(do ((j i (1+ j)))
	    ((>= j dim-1)
	     nil)
	  (setf (aref matrix j i)
		(prog1
		  (aref matrix i j)
		  (setf (aref matrix i j) (aref matrix j i))))))
      (dotimes (i dim-1)
	(dotimes (j dim-2)
	  (setf (aref into-matrix j i) (aref matrix i j)))))
  into-matrix)

;;; ---------------------------------------------------------------------------

(defun normalize-matrix (m)
  "Returns a new matrix such that the sum of its elements is 1.0"
  (matrix-multiply m (/ (sum-of-array-elements m))))
                                   
;;; ---------------------------------------------------------------------------

(defun sum-of-array-elements (array)
  (let ((result 0))
    (maparray array (lambda (element) (incf result element)))
    result))



