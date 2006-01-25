;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

$Id: smoothing.lisp,v 1.3 2005/09/07 16:15:07 gwking Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package metabang.math)

(export '(smooth-median-2 smooth-median-3 smooth-median-4
          smooth-median-5 smooth-hanning smooth-4253h
          smooth-mean-2 smooth-mean-3 smooth-mean-4
          smooth-mean-5))

#+Test
(defun compare-smoothers (data)
  (mapc (lambda (f)
          (format t "~%~30,A ~A" f (funcall f data))) 
        '(smooth-median-2 smooth-median-3 smooth-median-4
          smooth-median-5 smooth-hanning smooth-4253h
          smooth-mean-2 smooth-mean-3 smooth-mean-4
          smooth-mean-5))
  (values))

;;; ---------------------------------------------------------------------------

(defun smooth-median-2 (data)
  "Smooths `data' by replacing each element with the median of it and its
neighbor on the left.  A median of two elements is the same as their mean.  The
end is handled by duplicating the end element.  This function is not
destructive; it returns a list the same length as `data,' which should be a
sequence of numbers."
  ;; This is a special case, but we might as well take advantage of it.
  (let ((prev (car data)))
    (map 'list
	 #'(lambda (x)
	     (prog1 (/ (+ x prev) 2.0)
		    (setf prev x)))
	 data)))

;;; ---------------------------------------------------------------------------

(defun smooth-median-3 (data)
  "Smooths `data' by replacing each element with the median of it and its two
neighbors.  The ends are handled by duplicating the end elements.  This function
is not destructive; it returns a list the same length as `data,' which should be
a sequence of numbers."
  (flet ((median-3 (a b c)
	   ;; special code for median of three elts
	   (if (< a b)
	       (if (< a c)
		   (if (< b c) b c)
		   a)
	       (if (< a c)
		   a
		   (if (< b c) c b)))))
    ;; (spy (median-3 1 1 2))
    ;; (spy (median-3 1 2 1))
    ;; (spy (median-3 2 1 1))
    ;; (spy (median-3 1 3 2))
    ;; (spy (median-3 1 2 3))
    ;; (spy (median-3 2 1 3))
    ;; (spy (median-3 1 3 2))
    ;; (spy (median-3 3 1 2))
    ;; (spy (median-3 2 3 1))
    ;; (spy (median-3 3 2 1))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 2))
	(setf (aref temp 0) (elt data 0))
	(replace temp data :start1 1)
	(setf (aref temp (1+ n)) (elt data (- n 1)))
	(map 'list
	     #'median-3
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2))))))

;;; ---------------------------------------------------------------------------

(defun smooth-median-4 (data)
  "Smooths `data' by replacing each element with the median of it, its left
neighbor, and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  ;; the `median-4' function could probably be improved.
  (flet ((median-4 (a b c d)
	   ;; make `a' the biggest
	   (when (< a b) (rotatef a b))
	   (when (< a c) (rotatef a c))
	   (when (< a d) (rotatef a d))
	   ;; make `d' the smallest
	   (when (< b d) (rotatef b d))
	   (when (< c d) (rotatef c d))
	   ;; median is mean of `b' and `c'
	   (/ (+ b c) 2.0)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 3))
	(setf (aref temp 0) (elt data 0))
	(replace temp data :start1 1)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 1)) last)
	  (setf (aref temp (+ n 2)) last))
	(map 'list
	     #'median-4
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3))))))

;;; ---------------------------------------------------------------------------

(defun smooth-median-5 (data)
  "Smooths `data' by replacing each element with the median of it, its two left
neighbors and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  ;; median-5 can definitely be improved.
  (flet ((median-5 (a b c d e)
	   ;; tournament to make `a' biggest
	   (when (< a b) (rotatef a b))
	   (when (< c d) (rotatef c d))
	   (when (< a c) (rotatef a c))
	   (when (< a e) (rotatef a e))
	   ;; smallest is now b, d, or e; make e smallest
	   (when (< b e) (rotatef b e))
	   (when (< d e) (rotatef b e))
	   ;; now, median is b, c, or d.  Sort them
	   (when (< b c) (rotatef b c))
	   (when (< b d) (rotatef b d))
	   (when (< c d) (rotatef c d))
	   c))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 4))
	(let ((first (elt data 0)))
	  (setf (aref temp 0) first)
	  (setf (aref temp 1) first))
	(replace temp data :start1 2)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 2)) last)
	  (setf (aref temp (+ n 3)) last))
	(map 'list
	     #'median-5
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3)
	     (make-array n :displaced-to temp :displaced-index-offset 4))))))

;;; ---------------------------------------------------------------------------

(defun smooth-hanning (data)
  "Smooths `data' by replacing each element with the weighted mean of it and its
two neighbors.  The weights are 1/2 for itself and 1/4 for each neighbor.  The
ends are handled by duplicating the end elements.  This function is not
destructive; it returns a list the same length as `data,' which should be a
sequence of numbers."
  (flet ((weighted-mean (a b c)
	   (/ (+ a (* 2 b) c) 4.0)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 2))
	(let ((first (elt data 0)))
	  (setf (aref temp 0) first))
	(replace temp data :start1 1)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 1)) last))
	(map 'list
	     #'weighted-mean 
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2))))))

;;; ---------------------------------------------------------------------------

(defun smooth-4253H (data)
  "Smooths `data' by successive smoothing: 4,median; then 2,median; then
5,median; then 3,median; then hanning.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a list of numbers."
  (smooth-hanning
    (smooth-median-3
      (smooth-median-5
	(smooth-median-2
	  (smooth-median-4
	    data))))))

;;; ---------------------------------------------------------------------------

;;;
;;; mean smoothing functions
;;;

(defun smooth-mean-2 (data)
  "With a window of size two, the median and mean smooth functions are the
same."
  (smooth-median-2 data))

(defun smooth-mean-3 (data)
  "Smooths `data' by replacing each element with the mean of it and its two
neighbors.  The ends are handled by duplicating the end elements.  This function
is not destructive; it returns a list the same length as `data,' which should be
a sequence of numbers."
  (flet ((mean-3 (a b c)
           (/ (+ a b c) 3.0)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 2))
        (setf (aref temp 0) (elt data 0))
        (replace temp data :start1 1)
        (setf (aref temp (1+ n)) (elt data (1- n)))
        (map 'list
             #'mean-3
             (make-array n :displaced-to temp :displaced-index-offset 0)
             (make-array n :displaced-to temp :displaced-index-offset 1)
             (make-array n :displaced-to temp :displaced-index-offset 2))))))

(defun smooth-mean-4 (data)
  "Smooths `data' by replacing each element with the mean of it, its left
neighbor, and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  (flet ((mean-4 (a b c d)
	   (/ (+ a b c d) 4.0)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 3))
	(setf (aref temp 0) (elt data 0))
	(replace temp data :start1 1)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 1)) last)
	  (setf (aref temp (+ n 2)) last))
	(map 'list
	     #'mean-4
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3))))))

(defun smooth-mean-5 (data)
  "Smooths `data' by replacing each element with the median of it, its two left
neighbors and its two right neighbors.  The ends are handled by duplicating the
end elements.  This function is not destructive; it returns a list the same
length as `data,' which should be a sequence of numbers."
  (flet ((mean-5 (a b c d e)
           (/ (+ a b c d e) 5.0)))
    (let ((n (data-length data)))
      (with-temp-vector (temp (+ n 4))
	(let ((first (elt data 0)))
	  (setf (aref temp 0) first)
	  (setf (aref temp 1) first))
	(replace temp data :start1 2)
	(let ((last (elt data (- n 1))))
	  (setf (aref temp (+ n 2)) last)
	  (setf (aref temp (+ n 3)) last))
	(map 'list
	     #'mean-5
	     (make-array n :displaced-to temp :displaced-index-offset 0)
	     (make-array n :displaced-to temp :displaced-index-offset 1)
	     (make-array n :displaced-to temp :displaced-index-offset 2)
	     (make-array n :displaced-to temp :displaced-index-offset 3)
	     (make-array n :displaced-to temp :displaced-index-offset 4))))))


;;; ---------------------------------------------------------------------------

