;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

$Id: define-statistical-fun.lisp,v 1.2 2005/05/11 02:18:39 gwking Exp $

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: EKSL Group

DISCUSSION

|#

(in-package metabang.math)


(defmacro with-routine-error-handling (&body body)
  `(progn
     (handler-case ,@body
       (type-error () 
                   (error 'non-numeric-data)))))

;;; ---------------------------------------------------------------------------

(defmacro define-statistic (name &optional superclasses slots values argument-types lambda-list &body body)
  "In clasp, statistical objects have two parts, a class which stores the
various parts of the object and a computing function which computes the value
of the object from arguments.  The define-statistic macro allows the
definition of new statistical types.  The define-statistic macro must be
provided with all the information necessary to create a statistical object,
that is, everything required to create a new class, everything required to
create a computing function and some information to connect the two.  This
last part consists of a list of arguments and their types and a list which
determines how the values of a statistical function should be used to fill the
slots of a statistical object.

When define-statistic is invoked, two things happen, first a class is defined
which is a subclass of 'statistic and any other named `superclasses'.  Second,
a pair of functions is defined.  `clasp-statistics::name' is an internal
function which has the supplied `body' and `lambda-list' and must return as
many values as there are slots in the class `name'.  The function `name' is
also defined, it is basically a wrapper function which converts its arguments
to those which are accepted by `body' and then calls `clasp-statistics::name'.
The parameter clasp:*create-statistical-objects* determines whether the
wrapper function packages the values returned by the intern function into a
statistical object or just returns them as multiple values.

  The `argument-types' argument must be an alist in which the keys are the
names of arguments as given in `lambda-list' and the values are lisp types
which those arguments will be converted to before calling the internal
statistical function.  The primary purpose of this is to allow for coersion of
clasp variables to sequences, but any coercion which is allowed by lisp is
acceptable.  The `values' argument is intended to allow the programmer to
specify which slots in the statistical object are filled by which of the
values returned by the statistical function.  By default, the order of the
values is assumed to be direct slots in order of specification, inherited
slots in order of specification in the superclasses which are also statistics."
  (let* ((documentation (when (stringp (first body)) (pop body)))
         (wrapper-arglist (gensym "ARGS"))
         (internal-name (intern (format nil "~a-INTERNAL" (string name)) '#:cl-mathstats))
         (statistical-superclasses (loop for superclass in superclasses
                                       if (subtypep superclass 'statistic)
                                       collect superclass))
         (root-statistical-superclass (if (and (null statistical-superclasses)
                                               (null slots)
                                               (null values))
                                          'simple-statistic
                                        'composite-statistic)))
    (case root-statistical-superclass
      (simple-statistic
       (setf values '(value)))
      (composite-statistic
       (dolist (superclass statistical-superclasses)
         (cond
          ((subtypep superclass 'simple-statistic)
           (push (list superclass) slots)
           (pushnew superclass values))
          ((subtypep superclass 'composite-statistic)
           (dolist (value (reverse (get superclass :values)))
             (pushnew value values)))))
       (dolist (slot (reverse slots))
         (pushnew (car slot) values))))
    (setf documentation (format nil "~a ~a~%~a" name (remove-&rest lambda-list) documentation))
    (setf superclasses (append superclasses (list root-statistical-superclass)))
    `(progn
       (eval-when (:compile-toplevel)
         (export ',name))
       (defclass ,name ,superclasses
         ,slots)
       (setf (get ',name :values) ',values)
       (defun ,internal-name ,lambda-list
         ,(format nil "See ~a" name)
	 ,@body)
       (defun ,name (&rest args)
         ,@(when documentation `(,documentation))
         ;;$ new routine error checking (schmill)
         (with-routine-error-handling
           (let ((,wrapper-arglist (copy-list args)))
	     ,@(loop for (argument type) in argument-types
                     collect `(setf (nth ,(position argument lambda-list) ,wrapper-arglist)
                                    (convert (nth ,(position argument lambda-list) ,wrapper-arglist) ,type)))
	     ,(if (eq root-statistical-superclass 'simple-statistic)
                `(apply #',internal-name ,wrapper-arglist)
	        `(if *create-statistical-objects*
		   (multiple-value-call #'make-statistic ',name (apply #',internal-name ,wrapper-arglist))
		   (apply #',internal-name ,wrapper-arglist)))))))))

;;; ---------------------------------------------------------------------------

(defun remove-&rest (list)
  "Removes the '&rest arg' part from a lambda-list (strictly for documentation purposes."
  (setf list (copy-list list)) ; don't want to screw with the list
  (do* ((previous-part nil part)
        (part list (cdr part))
        (item (first part) (first part)))
       ((endp part) list)
    (when (eq item '&rest)
      (cond (previous-part
             (setf (cdr previous-part) (cdddr previous-part)))
            (t 
             (return (cddr part)))))))

;;; ---------------------------------------------------------------------------

(defmethod make-statistic (type &rest args)
  (let ((slots (get type :values))
        (object (make-instance type)))
    (do ((slot slots (cdr slot))
         (value args (cdr value)))
	((null slot))
      (setf (slot-value object (car slot)) (car value)))
    object))

(defmethod convert (object type)
  (coerce object type))

(defparameter *create-statistical-objects* nil)

;;; what are these for!!

(defclass data () ())

(defclass statistic (data) ())

(defclass simple-statistic (statistic) ())

(defclass composite-statistic (statistic) ())

(defmethod statisticp (it)
  (declare (ignore it))
  nil)

(defmethod statisticp ((it statistic))
  t)

(defmethod composite-statistic-p (it)
  (declare (ignore it))
  nil)

(defmethod composite-statistic-p ((it composite-statistic))
  t)

(defmethod simple-statistic-p (it)
  (declare (ignore it))
  nil)

(defmethod simple-statistic-p ((it simple-statistic))
  (not (composite-statistic-p it)))



