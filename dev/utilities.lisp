(in-package #:metabang.math)

(defvar *temporary-vector* nil
  "A temporary vector for use by statistical functions such as `quantile,' which
uses it for sorting data.  This avoids consing or rearranging the user's data.")

(defmacro with-temp-vector ((temp min-size) &body forms)
  "Binds `temp' to a vector of length at least `min-size.' It's a vector of
pointers and has a fill-pointer, initialized to `min-size.'"
  ;; The following will NOT prevent sychronization problems in a parallel lisp,
  ;; but it will make them less likely, because the only timing problem will be
  ;; if a process swap allows another process to grab the same
  ;; *temporary-vector*, that is, before the variable gets set to nil.  The real
  ;; solution is critical sections, which is not in Common Lisp.
  `(let ((,temp *temporary-vector*))
     ;; no process swap between the previous LET and the next SETF, we hope!
     (setf *temporary-vector* nil) 
     (when (or (null ,temp)
	       (< (array-total-size ,temp) ,min-size))
       (setf ,temp (make-array ,min-size :fill-pointer ,min-size)))
     ;; nulling out the array makes any bugs in the code more likely to
     ;; show up, but is not really necessary
     #+TEST
     (fill-all ,temp nil)
     (setf (fill-pointer ,temp) ,min-size)
     (multiple-value-prog1 (progn . ,forms)
			   (setf *temporary-vector* ,temp))))

(defvar *temporary-table* nil
  "A temporary table. This avoids consing.")

(defmacro with-temp-table ((temp) &body forms)
  "Binds `temp' to a hash table."
  ;; The following will NOT prevent sychronization problems in a parallel lisp,
  ;; but it will make them less likely, because the only timing problem will be
  ;; if a process swap allows another process to grab the same
  ;; *temporary-table*, that is, before the variable gets set to nil.  The real
  ;; solution is critical sections, which is not in Common Lisp.
  `(let ((,temp *temporary-table*))
     ;; no process swap between the previous LET and the next SETF, we hope!
     (setf *temporary-table* nil) 
     (when (null ,temp)
       (setf ,temp (make-hash-table)))
     ;; nulling out the array makes any bugs in the code more likely to
     ;; show up, but is not really necessary
     (clrhash ,temp)
     (multiple-value-prog1 (progn . ,forms)
			   (setf *temporary-table* ,temp))))

;;; ---------------------------------------------------------------------------

(defun extract-unique-values (sequence)
  "A faster version of `remove-duplicates'. Note you cannot specify a :TEST (it is always #'eq)."
  (with-temp-table (table)
    (loop for element in sequence do
	  (unless (gethash element table)
            (setf (gethash element table) element))
          finally (return (let ((val nil)) 
                            (maphash #'(lambda (key value)
                                         (declare (ignore value))
                                         (push key val))
                                     table)
                            (values val))))))
#+test
(defun test-extract-unique-values ()
  (loop for size in '(100 500 1000 #-MCL 5000 #-MCL 10000 #-MCL 50000 #-MCL 100000)
        for list = (loop repeat size collect (random size))
        for theirs = (time (remove-duplicates list))
        for ours =  (time (extract-unique-values list))
        do
        (format t "~&Lengths: initial = ~d; after: theirs = ~d, ours = ~d~%" (length list) (length theirs) (length ours))))