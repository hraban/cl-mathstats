(in-package cl-mathstats-test)

(deftestsuite test-cl-mathstats () ())

(deftestsuite test-dot-product (test-cl-mathstats) ())
(addtest (test-dot-product)
  simple-test
  (ensure-same (dot-product '(1 3 -2) '(4 -2 -1.0)) 0.0 :test '=))

  