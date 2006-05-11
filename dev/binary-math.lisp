
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                      Powers of Two Math Functions                      *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Scott D. Anderson
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-12-94 File Created.  (anderson)
;;;  04-12-94 Copied function definitions from ph:fs;primitives.lisp.  
;;;           Changed several functions to not use arrays of constants, 
;;;           but to recompute.  Locality and speed might be better.
;;;           Also, changed most macros to be inline functions.
;;;           (anderson)
;;;  05-06-95 Added `trucate-to-factor' and `round-to-factor,' both 
;;;           drawn from the same place.  (anderson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #:metabang.math)

(defun times2 (i &optional (power 1))
  "Multiply `i' by a power of 2."
  (ash (the fixnum i) power))

(defun div2 (i &optional (power 1))
  "Divide positive fixnum `i' by 2 or a power of 2, yielding an integer result.
For example, (div2 35 5) => 1."
  (ash (the fixnum i) (- power)))

(defun exp2 (n)
  "2^n"
  (ash 1 n))

(defun log2 (n)
  "Log of `n' to base 2."
  (integer-length n))

(defun trunc2 (n power)
  "Truncate `n' to a power of 2."
  ;; Hopefully, if power is a constant, a lot of constant folding will happen.
  (logand n (lognot (- (exp2 power) 1))))

(defun mod2 (n power)
  "Find `n' mod a power of 2."
  ;; Hopefully, if power is a constant, a lot of constant folding will happen.
  (logand n (- (exp2 power) 1)))

(eval-when (:compile-toplevel)
  (proclaim '(inline times2))
  (proclaim '(inline div2))
  (proclaim '(inline exp2))
  (proclaim '(inline log2))
  (proclaim '(inline trunc2))
  (proclaim '(inline mod2)))
