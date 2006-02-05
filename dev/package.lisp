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
   #:sum-of-array-elements)
  
  (:export
   #:truncate-to-factor
   #:round-to-factor
   #:ensure-float
   #:linear-scale
   #:2fpi
   #:fpi
   #:degrees->radians
   #:radians->degrees
   #:on-interval
   #:combination-count
   #:permutation-count
   #:square
   #:f-measure
   #:+e+)
  
  (:export
   #:times2
   #:div2 
   #:exp2 
   #:log2 
   #:trunc2 
   #:mod2))
