
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

(in-package metabang.math)

(export '(times2
	  div2 
	  exp2 
	  log2 
	  trunc2 
	  mod2))

;;; --*--
;;; ***************************************************************************

(proclaim '(inline times2))
(defun times2 (i &optional (power 1))
  "Multiply `i' by a power of 2."
  (ash (the fixnum i) power))

(proclaim '(inline div2))
(defun div2 (i &optional (power 1))
  "Divide positive fixnum `i' by 2 or a power of 2, yielding an integer result.
For example, (div2 35 5) => 1."
  (ash (the fixnum i) (- power)))

(proclaim '(inline exp2))
(defun exp2 (n)
  "2^n"
  (ash 1 n))

(proclaim '(inline log2))
(defun log2 (n)
  "Log of `n' to base 2."
  (integer-length n))

(proclaim '(inline trunc2))
(defun trunc2 (n power)
  "Truncate `n' to a power of 2."
  ;; Hopefully, if power is a constant, a lot of constant folding will happen.
  (logand n (lognot (- (exp2 power) 1))))

(proclaim '(inline mod2))
(defun mod2 (n power)
  "Find `n' mod a power of 2."
  ;; Hopefully, if power is a constant, a lot of constant folding will happen.
  (logand n (- (exp2 power) 1)))

;;; ***************************************************************************
;;; EOF
