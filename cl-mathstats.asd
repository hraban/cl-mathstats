(in-package :common-lisp-user)
(defpackage "ASDF-CL-MATHSTATS" (:use #:cl #:asdf))
(in-package "ASDF-CL-MATHSTATS")

;;; ---------------------------------------------------------------------------

(defsystem cl-mathstats
  :version "0.8"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"
  
  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "api"
                                            :depends-on ("package"))
                                     (:file "parameters"
                                            :depends-on ("package"))
                                     (:file "math-utilities"
                                            :depends-on ("package"))
                                     (:file "class-defs"
                                            :depends-on ("package"))
                                     (:file "definitions"
                                            :depends-on ("math-utilities"))
                                     (:file "binary-math"
                                            :depends-on ("package"))
                                     (:file "matrices"
                                            :depends-on ("package"))
                                     (:file "matrix-fns"
                                            :depends-on ("matrices"))
                                     (:file "density-fns"
                                            :depends-on ("parameters"))
                                     (:file "svd"
                                            :depends-on ("matrix-fns"))
                                     (:file "utilities"
                                            :depends-on ("package"))
                                     (:file "define-statistical-fun"
                                            :depends-on ("package"))
                                     (:file "basic-statistics"
                                            :depends-on ("class-defs"
                                                         "define-statistical-fun"
                                                         "utilities"))
                                     (:file "smoothing"
                                            :depends-on ("utilities"))
                                     (:file "correlation-regression"
                                            :depends-on ("package"))
                                     (:file "anova"
                                            :depends-on ("package"))))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  :depends-on (metatilities-base cl-containers))

;;; ---------------------------------------------------------------------------

#+Ignore
(define-glu-system meta.sha1
  ("sha1")
  :base-dir "cl-MATHSTATS:"
  :bin-identifiers (:platform :vendor))

;;; ---------------------------------------------------------------------------

#+Ignore
(define-glu-system meta.md5
  ("md5")
  :base-dir "cl-MATHSTATS:"
  :bin-identifiers (:platform :vendor))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************

