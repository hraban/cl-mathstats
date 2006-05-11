(in-package #:metabang.math)

(declaim (inline ensure-float)) 
(defun ensure-float (number)
  (float number 1.0d0))

;;; ---------------------------------------------------------------------------

(defun linear-scale (value old-min old-max new-min new-max)
  "Rescales value linearly from the old-min/old-max scale to the new-min/new-max one."
  (let ((old-width (- old-max old-min))
        (new-width (- new-max new-min)))
    (+ (* (- value old-min)
          (/ new-width old-width))
       new-min)))

#+Test
(progn
  (assert (= (linear-scale 1 1 10 1 10) 1))
  (assert (= (linear-scale 1 0 10 0  5) 1/2))
  (assert (= (linear-scale 1 0 10 0 20) 2))
  (assert (= (linear-scale 1 0 10 -10 0) -9))
  )

;;;----------------------------------------------------------------------------
;;; Non-binary math.

(defun truncate-to-factor (n factor)
  "Equivalent to (* factor (truncate n factor)).  For example,
`truncate-to-factor' of 65 and 60 is 60.  Useful for converting to certain
units, say when converting minutes to hours and minutes.  See also
`round-to-factor.'"
  (* factor (truncate n factor)))

#+test
(defun test-truncate-to-factor ()
  (spy (truncate-to-factor 65 60)))

(defun round-to-factor (n factor)
  "Equivalent to (* factor (round n factor)).  For example, `round-to-factor' of
65 and 60 is 60.  Useful for converting to certain units, say when converting
minutes to the nearest hours.  See also `truncate-to-factor.'"
  (* factor (round n factor)))

(eval-when (:compile-toplevel)
  (proclaim '(inline truncate-to-factor))
  (proclaim '(inline round-to-factor)))


;;; ---------------------------------------------------------------------------

(defconstant fpi (coerce pi 'single-float)
  "The constant pi, in single-float format.  Using this constant avoid
run-time double-float contagion.")

;;; ---------------------------------------------------------------------------

(defconstant 2fpi (coerce (* 2.0d0 pi) 'single-float)
  "The constant 2*pi, in single-float format.  Using this constant avoid
run-time double-float contagion.")

;;; ---------------------------------------------------------------------------

(defconstant +e+ 2.71828182845905423
  "An approximation of the constant e \(named for Euler!\).")

;;; ---------------------------------------------------------------------------

(defun degrees->radians (degrees)
  "Convert degrees to radians."
  (* degrees (/ fpi 180.)))

;;; ---------------------------------------------------------------------------

(defun radians->degrees (radians)
  "Convert radians to degrees.  Does not round the result."
  (* radians (/ 180d0 pi)))

;;; ---------------------------------------------------------------------------

(defun on-interval (x lower-bound upper-bound &key (lower-inclusive? t)
                      (upper-inclusive? t))
  "returns t iff x in the interval"
  (cond ((and lower-inclusive? upper-inclusive?)
         (values (and (>= x lower-bound)
                      (<= x upper-bound))))
        ((and lower-inclusive? (null upper-inclusive?))
         (values (and (>= x lower-bound)
                      (< x upper-bound))))
        ((and (null lower-inclusive?) upper-inclusive?)
         (values (and (> x lower-bound)
                      (<= x upper-bound))))
        ((and (null lower-inclusive?) (null upper-inclusive?))
         (values (and (> x lower-bound)
                      (< x upper-bound))))
        (t (format *debug-io* "~A ~A ~A ~A ~A"
                   x lower-bound upper-bound lower-inclusive?
	           upper-inclusive?)
           (warn "Testing, better check the logic --this is beta")
           nil)))

;;; ---------------------------------------------------------------------------

(defun combination-count (n k)
  "Returns the number of combinations of n elements taken k at a time. Assumes valid 
input."
  (let ((result 1))
    (loop for i from 1 to k
          for j from n downto 1 do
          (setf result (* result (/ j i))))
    result))

;;; ---------------------------------------------------------------------------

(defun permutation-count (n k)
  "Returns the number of possible ways of taking k elements out of n total."
  (let ((result 1))
    (loop for j from n downto (1+ (- n k)) do
          (setf result (* result j)))
    result))

;;; ---------------------------------------------------------------------------

(declaim (inline square))
(defun square (x)
  (* x x))

;;; ---------------------------------------------------------------------------

(defun f-measure (precision recall &optional (beta .5))
  "Returns the f-measure, the combination of precision and recall based on
parameter beta - default = .5 which => precision and recall are equally weighted.
beta = 1 => precision is maximized.  beta = 0 => recall is maximized.

From a recent statistics book - All of Statistics - springer verlag
http://www2.springeronline.com/sgw/cda/frontpage/0,,4-10128-22-13887455-0,00.html
"
  (unless (on-interval beta 0 1)
    (warn "beta = ~D out for range" beta))
  (if (or (zerop precision) (zerop recall))
    0d0
    (float (/ 1 (+ (/ beta precision) (/ (- 1 beta) recall))))))

#+iet
(defun foo (p r b)
  "iet's f-value"
  (float (/ (* p r (+ 1 (* b b))) (+ (* b b p) r))))

#+another
(defun bar (p r b)
  "from the paper gary sent around"
  (float (/ (* r p (+ 1 b)) (+ r (* b p)))))


#+test
(loop for (p r b) in '((.5 .5 .5) (.1 .5 .5) (.1 .5 0) (.1 .5 1) (.5 .1 0) (.5 .1 1))
      do (format t "~2&f-measure:  ~D ~D ~D = ~5,D" p r b (f-measure p r b))
      do (format t "~&IET-f-val:  ~D ~D ~D = ~5,D" p r b (foo p r b))
      do (format t "~&Other-f-v:  ~D ~D ~D = ~5,D" p r b (bar p r b))
      do (format t "~&foobar--v:  ~D ~D ~D = ~5,D" p r b (bar p r b)))

#+test
(loop for (p r b) in '((.1 .5 0) (.1 .5 .5) (.1 .5 1.0) (.1 .5 2.0) (.1 .5 3.0))
      ;;;do (format t "~2&f-measure:  ~D ~D ~D = ~5,D" p r b (f-measure p r b))
      do (format t "~2&IET-f-val:  ~D ~D ~D = ~5,D" p r b (foo p r b))
      do (format t "~&Other-f-v:  ~D ~D ~D = ~5,D" p r b (bar p r b)))




;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************