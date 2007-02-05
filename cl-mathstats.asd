(in-package #:common-lisp-user)
(defpackage #:asdf-cl-mathstats (:use #:cl #:asdf))
(in-package #:asdf-cl-mathstats)

(defsystem cl-mathstats
  :version "0.8"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Common Lisp math and statistics routines"
  :components ((:module 
		"dev"
		:components
		((:file "package")
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
               (:module 
		"website"
		:components
		((:module "source"
			  :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op cl-mathstats-test)))
  :perform (test-op :after (op c)
                    (describe
                     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:cl-mathstats-test)))
  :depends-on (:metatilities-base :cl-containers))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'cl-mathstats))))
  (values nil))
