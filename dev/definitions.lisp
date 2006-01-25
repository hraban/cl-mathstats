;;;-*- Mode: Lisp; Package: metabang.math -*-

#| simple-header

$Id: definitions.lisp,v 1.1 2004/12/15 19:13:44 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package metabang.math)

(export '(+0degrees+ +5degrees+ +10degrees+ +15degrees+
          +30degrees+ +45degrees+ +60degrees+ +90degrees+
          +120degrees+ +135degrees+ +150degrees+ +180degrees+ +210degrees+ 
          +225degrees+ +240degrees+ +270degrees+ +300degrees+ +315degrees+
          +330degrees+ +360degrees+))

(defconstant +0degrees+   (degrees->radians 0))
(defconstant +5degrees+   (degrees->radians 5))
(defconstant +10degrees+  (degrees->radians 10))
(defconstant +15degrees+  (degrees->radians 15))
(defconstant +30degrees+  (degrees->radians 30))
(defconstant +45degrees+  (degrees->radians 45))
(defconstant +60degrees+  (degrees->radians 60))
(defconstant +90degrees+  (degrees->radians 90))
(defconstant +120degrees+ (degrees->radians 120))
(defconstant +135degrees+ (degrees->radians 135))
(defconstant +150degrees+ (degrees->radians 150))
(defconstant +180degrees+ (degrees->radians 180))
(defconstant +210degrees+ (degrees->radians 210))
(defconstant +225degrees+ (degrees->radians 225))
(defconstant +240degrees+ (degrees->radians 240))
(defconstant +270degrees+ (degrees->radians 270))
(defconstant +300degrees+ (degrees->radians 300))
(defconstant +315degrees+ (degrees->radians 315))
(defconstant +330degrees+ (degrees->radians 330))
(defconstant +360degrees+ (degrees->radians 360))

