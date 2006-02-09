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
   #:mod2)
  
  (:export
   #:+0degrees+ #:+5degrees+ #:+10degrees+ #:+15degrees+
   #:+30degrees+ #:+45degrees+ #:+60degrees+ #:+90degrees+
   #:+120degrees+ #:+135degrees+ #:+150degrees+ #:+180degrees+ #:+210degrees+ 
   #:+225degrees+ #:+240degrees+ #:+270degrees+ #:+300degrees+ #:+315degrees+
   #:+330degrees+ #:+360degrees+)
  
  (:export 
   #:gamma-ln
   #:factorial-exact
   #:factorial
   #:factorial-ln
   #:binomial-coefficient
   #:binomial-coefficient-exact
   #:binomial-probability
   #:binomial-probability-exact
   #:beta
   #:safe-exp
   #:underflow-goes-to-zero
   #:without-floating-underflow-traps
   #:gamma-incomplete
   #:error-function
   #:gaussian-cdf
   #:error-function-complement
   #:gaussian-significance
   #:poisson-cdf
   #:chi-square-significance
   #:beta-incomplete
   #:students-t-significance
   #:f-significance
   #:binomial-cdf
   #:binomial-cdf-exact)
  
  (:export 
   #:smooth-median-2 #:smooth-median-3 #:smooth-median-4
   #:smooth-median-5 #:smooth-hanning #:smooth-4253h
   #:smooth-mean-2 #:smooth-mean-3 #:smooth-mean-4
   #:smooth-mean-5)
  
  (:export 
   #:correlation
   #:correlation-from-summaries
   #:partials-from-parents
   #:lagged-correlation
   #:cross-correlation
   #:autocorrelation
   #:linear-regression-minimal-summaries
   #:linear-regression-minimal
   #:linear-regression-brief-summaries
   #:linear-regression-brief
   #:linear-regression-verbose-summaries
   #:linear-regression-verbose
   #:multiple-linear-regression-normal
   #:multiple-linear-regression-arrays
   #:multiple-linear-regression-minimal
   #:multiple-linear-regression-brief
   #:multiple-linear-regression-verbose
   #:correlation-matrix))
