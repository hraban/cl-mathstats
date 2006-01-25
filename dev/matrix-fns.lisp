;;;; -*- Mode:Common-Lisp; Package:eksl-utilities; Fonts:(MEDFNT COURIERFB HL12I TR12 MEDFNB CPTFONTI HL12B); Base:10 -*-
;;;; *-* File: Billy-Pilgrim:Users:gwking:repository:p2dis:eksl-math:dev:matrix-fns.lisp *-*
;;;; *-* Last-edit: Wednesday, June 15, 2005 22:18:50; Edited-By: Gary *-* 
;;;; *-* Machine: billy-pilgrim *-*
;;;; *-* Software: Macintosh Common Lisp, Version 5.1b4 *-*
;;;; *-* Lisp: Macintosh Common Lisp, Version 5.1b4 *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                     Matrix Manipulations Functions                     *
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
;;;  07-17-92 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package metabang.math)

;;; --*--
;;; ***************************************************************************

;;; This file contains functions for various matrix manipulations.  It includes
;;; addition and multiplication of two matrices, addition and multiplication of
;;; matrices by scalars, and transposition of matrices.


;;; ---------------------------------------------------------------------------

(defun matrix-trace (matrix)
  (let ((result 0d0))
    (dotimes (i (array-dimension matrix 0))
      (incf result (aref matrix i i)))
    result))

;;; ---------------------------------------------------------------------------

(defun matrix-multiply (&rest args)
  "Does successive multiplications of each element in `args'.  If two
elements are scalar, then their product is i * j, if a scalar is
multiplied by a matrix, then each element in the matrix is multiplied
by the scalar, lastly, if two matrices are multiplied then standard
matrix multiplication is applied, and the ranks must be such that if
ARGi is rank a x b and ARGj is rank c x d, then b must be equal to c."
  (let* ((arg1 (car args))
	 (arg2 (and arg1 (cadr args))))
    (if (null arg2)
	arg1
      (if (null arg1)
	  arg2
	(if (numberp arg1)
	    (if (numberp arg2)
		(return-from matrix-multiply (apply #'matrix-multiply
						    (cons (* arg1 arg2) 
							  (cddr args))))
	      (return-from matrix-multiply
		(apply #'matrix-multiply
		       (cons (matrix-times-scalar arg2 arg1) 
			     (cddr args)))))
	  (if (numberp arg2)
	      (return-from matrix-multiply
		(apply #'matrix-multiply
		       (cons (matrix-times-scalar arg1 arg2) 
			     (cddr args))))
	    (return-from matrix-multiply
	      (apply #'matrix-multiply
		     (cons (matrix-times-matrix arg1 arg2)
			   (cddr args))))
	    )
	  ))
      )
    )
  )

;;; ---------------------------------------------------------------------------

(defun matrix-times-scalar! (matrix scalar)
  "Multiply a matrix by a scalar value"
  (let ((temp (linearize-array matrix)))
    (loop for i from 0 to (1- (array-total-size temp)) do
          (setf (aref temp i) (* (aref temp i) scalar)))
    matrix))

;;; ---------------------------------------------------------------------------

(defun matrix-times-scalar (matrix scalar)
  "Multiply a matrix by a scalar value"
  (matrix-times-scalar! (copy-array matrix) scalar))

;;; ---------------------------------------------------------------------------

(defun matrix-times-matrix (mat1 mat2)
  "Multiplies two matrices together"
  (if (= (array-dimension mat1 1)
	 (array-dimension mat2 0))
      (let ((result (make-array (list (array-dimension mat1 0)
				      (array-dimension mat2 1)))))
	(dotimes (row (array-dimension result 0))
	  (dotimes (column (array-dimension result 1))
	    (let ((terms 0))
	      (dotimes (middle (array-dimension mat1 1))
		(setf terms (+ terms (* (or (aref mat1 row middle) 0) 
					(or (aref mat2 middle column) 0)))))
	      (setf (aref result row column) terms))))
	(return-from matrix-times-matrix result))
      (progn
	(format t "~&Illegal matrix multiplication: 
Matrix sizes ~a x ~a and ~a x ~a don't match."
		(array-dimension mat1 0)
		(array-dimension mat1 1)
		(array-dimension mat2 0)
		(array-dimension mat2 1))
	(return-from matrix-times-matrix nil))))


(defun matrix-addition (&rest args)
  ""

  (let* ((arg1 (car args))
	 (arg2 (and arg1 (cadr args))))
    (if (null arg2)
	arg1
	(if (null arg1)
	    arg2
	    (if (numberp arg1)
		(if (numberp arg2)
		    (return-from matrix-addition (apply #'matrix-addition
							(cons (+ arg1 arg2) 
							      (cddr args))))
		  (return-from matrix-addition
		    (apply #'matrix-addition
			   (cons (matrix-plus-scalar arg2 arg1) 
				 (cddr args)))))
	      (if (numberp arg2)
		  (return-from matrix-addition
		    (apply #'matrix-addition
			   (cons (matrix-plus-scalar arg1 arg2) 
				 (cddr args))))
		(return-from matrix-addition
		  (apply #'matrix-addition
			 (cons (matrix-plus-matrix arg1 arg2)
			       (cddr args)))))))))) 

(defun matrix-plus-scalar (matrix scalar)
  "Add a scalar value to a matrix"
  (let ((return-mat (make-array (array-dimensions matrix))))
    (dotimes (column (array-dimension matrix 1))
      (dotimes (row (array-dimension matrix 0))
	(setf (aref return-mat row column)
	  (+ scalar (or (aref matrix row column) 0)))))
    return-mat))

(defun matrix-plus-matrix (mat1 mat2)
  "Adds two matrices together"
  (if (and (= (array-dimension mat1 0)
	      (array-dimension mat2 0))
	   (= (array-dimension mat1 1)
	      (array-dimension mat2 1)))
      (let ((result (make-array (list (array-dimension mat1 0)
				      (array-dimension mat1 1)))))
	(dotimes (row (array-dimension result 0))
	  (dotimes (column (array-dimension result 1))
	    (setf (aref result row column) (+ (or (aref mat1 row column) 0)
					      (or (aref mat2 row column) 0)))))
	(return-from matrix-plus-matrix result))
      (progn
	(format t "~&Illegal matrix addition: Matrix sizes ~a x ~a and ~a x ~a don't match."
		(array-dimension mat1 0)
		(array-dimension mat1 1)
		(array-dimension mat2 0)
		(array-dimension mat2 1))
	(return-from matrix-plus-matrix nil))))


(defun reduce-matrix (mat)
  "Uses the Gauss-Jordan reduction method to reduce a matrix."

;;; The Gauss-Jordan reduction method is described in almost any linear algebra
;;; textbook.  The reference I used is: Kolman, Bernard; Introductory Linear
;;; Algebra with Applications, Second Edition; Macmillan Publishing Co., Inc.,
;;; New York, 1980, pp 46-50
  
  ;; copy mat to newmat, changing nil's to 0's
  (let ((newmat (make-array (array-dimensions mat))))
    (dotimes (x (array-dimension newmat 0))
      (dotimes (y (array-dimension newmat 1))
	(setf (aref newmat x y) (or (aref mat x y) 0))))
    
    ;; for each row
    (dotimes (first-row (array-dimension newmat 0))
      (let ((pivot-row nil)
	    (pivot-column nil))
	;; Finds the first non-zero entry in the matrix, searching in
	;; column-major order.  This is kinda complicated, so here's how it
	;; works: If the inner dotimes (which looks down a column for the
	;; first non-zero row) is successful, then it stores the row and column
	;; found in pivot-row and pivot-column, and returns a t, indicating to
	;; the outer dotimes that the search has been successful and should be
	;; stopped.  If there are no non-zero entries in the column, then the
	;; inner loop returns nil, and the outer loop continues.  If no
	;; non-zero entries are found by the time the outer loop is done, then
	;; there are no more reductions to be done, so newmat is returned.  In
	;; the inner loop, the pivot-r-counter is added to first-row to search
	;; only below those rows which have already been reduced.
	(if (dotimes (pivot-c (array-dimension newmat 1))
	      (if (dotimes (pivot-r-counter (- (array-dimension newmat 0)
					       first-row))
		    (let ((pivot-r (+ pivot-r-counter first-row)))
		      (if (= 0 (aref newmat pivot-r pivot-c))
			  nil
			  (progn (setf pivot-row pivot-r
				       pivot-column pivot-c)
				 (return t)))))
		  (return t)
		  nil))
	    nil
	    (return newmat))
	
	;; swap the pivot-row into first-row position if it's not already there
	(unless (= pivot-row first-row)
	  (let ((temp))
	    (dotimes (y (array-dimension newmat 1))
	      (setf temp (aref newmat pivot-row y)
		    (aref newmat pivot-row y) (aref newmat first-row y)
		    (aref newmat first-row y) temp))))
	(setf pivot-row first-row)
	
	;; divide through on the pivot-row by the pivot value
	(let ((pivot-value (aref newmat pivot-row pivot-column)))
	  (dotimes (column (array-dimension newmat 1))
	    (setf (aref newmat pivot-row column)
	      (/ (aref newmat pivot-row column)
		 pivot-value))))
	;; for each other row, if there is a non-zero value in the
	;; pivot column, then subtract the pivot-row times the value in
	;; that row's pivot column from the row in question
	(dotimes (row (array-dimension newmat 0))
	  (unless (or (= row pivot-row)
			(= 0 (aref newmat row pivot-column)))
	    (let ((multiplier (aref newmat row pivot-column)))
	      (dotimes (column (array-dimension newmat 1))
		(setf (aref newmat row column)
		  (- (aref newmat row column)
		     (* (aref newmat pivot-row column)
			multiplier)))))))
	))
    newmat))

;;; ***************************************************************************
;;; EOF
