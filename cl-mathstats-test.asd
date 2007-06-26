(in-package #:common-lisp-user)
(defpackage #:asdf-cl-mathstats-test (:use #:cl #:asdf))
(in-package #:asdf-cl-mathstats-test)

(defsystem cl-mathstats-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Mathstats"  
  :components ((:module 
		"unit-tests"
		:components ((:file "package")
			     (:file "tests" 
				    :depends-on ("package")))))
  :depends-on (:lift :cl-mathstats))

