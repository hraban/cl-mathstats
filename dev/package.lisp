;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Copyright 1992 - 2005 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package common-lisp-user)

(defpackage "CL-MATHSTATS"
  (:use "COMMON-LISP" "METATILITIES" "CL-CONTAINERS")
  (:nicknames "METABANG.MATH")
  (:import-from "METATILITIES" 
                #:it #:self)
  (:export 
   #:transpose-matrix
   #:matrix-multiply
   #:matrix-trace
   #:normalize-matrix
   #:sum-of-array-elements))
