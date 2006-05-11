;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (MD5 :USE FUTURE-COMMON-LISP); Base: 10 -*-

;;; The RSA Data Security, Inc. MD5 Message-Digest Algorithm
;;; Internet RFC 1321
;;; Lisp implementation, 1994, Mark Nahabedian, with enhancements by Tony Eng
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology
;;; last modified 27 April, 1995

;;; If the LISP implementation doesn't support 32 bit fixnums, this will
;;; cons bignums like mad.

(defpackage MD5
  (:use #+Genera "FUTURE-COMMON-LISP"
	#+Lucid "Lisp"
        #+MCL "COMMON-LISP-USER" #+MCL "COMMON-LISP")
  (:export
    "MD5-ENCODE"
    "MD5-DIGEST-BYTE-VECTOR"
    "MD5-ENCODE-STRING"
    "MD5-ENCODE-FILE-FOR-PGP"
    "CHARACTER-TO-BYTE"
    "MAKE-MD5-BUFFER-FILLER-FROM-READER-FUNCTION"
    "MAKE-FILTERED-READER-FUNCTION"
    "MAKE-PGP-READER-FUNCTION"
    "MD5"))

(in-package #:"MD5")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 32 bit word operators

;;; comments represent how the operation is expressed in the RFC.

;;; a + b
(defun 32-add (a b)
  (ldb (byte 32 0) (+ a b)))

;;; number <<< by
(defun 32-left-rot (number by)
  (let ((break (- 32 by)))
    (dpb (ldb (byte break 0) number)
	 (byte break by)
	 (ldb (byte by break)
	      number))))

;;; not num
(defun 32-not (num)
  (ldb (byte 32 0) (lognot num)))

;;; a v b
(defun 32-or (a b)
  (logior a b))

;;; a XOR b
(defun 32-xor (a b)
  (logxor a b))

;;; ab
(defun 32-and (a b)
  (logand a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; padding calculations

;(defun number-of-md5-0pad-bits-required (message-length-in-bits)
;  (mod (- 448 (1+ message-length-in-bits)) 512))	;required "1" bit

(defun md5-length64 (message-length-in-bits)
  (declare (values hi lo))
  (values (ldb (byte 32 32) message-length-in-bits)
	  (ldb (byte 32 0) message-length-in-bits)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MD5 calculation state registers

(defmacro with-md5-state ((a b c d) &body body)
  `(let (,a ,b ,c ,d)
     ,@body))

(defmacro initialize-md5-state (a b c d)
  `(setf ,a #x67452301
	 ,b #xefcdab89
	 ,c #x98badcfe
	 ,d #x10325476))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions

(defun md5-function-f (x y z)
  (32-or (32-and x y) (32-and (32-not x) z)))

(defun md5-function-g (x y z)
  (32-or (32-and x z) (32-and y (32-not z))))

(defun md5-function-h (x y z)
  (32-xor (32-xor x y) z))

(defun md5-function-i (x y z)
  (32-xor y (32-or x (32-not z))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sine table

#|
;;; unfortunately this doesn't give the same numbers as in RFC1321
(dotimes (i 64)
  (if (zerop (mod i 4))
      (fresh-line)
      (write-char #\space))
  (format t "#x~8,'0x"
	  (floor (* 4294967296 (abs (sin (1+ i)))))))
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +random-sine-table+
	     (make-array 64
			 :element-type '(unsigned-byte 32)
			 :initial-contents
			 '(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
			   #xf57c0faf #x4787c62a #xa8304613 #xfd469501
			   #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
			   #x6b901122 #xfd987193 #xa679438e #x49b40821

			   #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
			   #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
			   #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
			   #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a

			   #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
			   #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
			   #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
			   #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665

			   #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
			   #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
			   #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
			   #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some very helpful macros

;;; version in document
(defmacro md5-function-ffgghhii (a b c d X k s i fghi)
  `(setf ,a (32-add ,b
		    (32-left-rot (32-add (32-add ,a
						 (,fghi ,b ,c ,d))
					 (32-add (aref ,X ,k)
						 ,(aref +random-sine-table+ (1- i))))
				 ,s))))

(defmacro md5-generate-code-for-one-round (fghi (a b c d) x
					   (k-initial k-inc)
					   (s0 s1 s2 s3) i-initial)
  (do* ((k k-initial (mod (+ k k-inc) 16))
	(which 0 (mod (1+ which) 4))
	(i i-initial (1+ i))
	(forms nil))
       ((>= i (+ i-initial 16))
	(cons 'progn (nreverse forms)))
    (multiple-value-bind (s abcd)
	(ecase which
	  (0 (values s0 (list a b c d)))
	  (1 (values s1 (list d a b c)))
	  (2 (values s2 (list c d a b)))
	  (3 (values s3 (list b c d a))))
      (push `(md5-function-ffgghhii ,@abcd ,x ,k ,s ,i ,fghi)
	    forms))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hack

(defmacro with-stack-array ((var dimensions &rest keyargs) &body body)
  #+Genera
  `(sys:with-stack-array (,var ,dimensions ,@keyargs) ,@body)
  #-Genera
  `(let ((,var (make-array ,dimensions ,@keyargs)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the MD5 encoder

;;; GET-DATA-FUNCTION takes an array of 16 32 bit words as argument and
;;; fills the array from its source.  It returns T if there is more data
;;; to be read and NIL if all data (including padding and length) have
;;; been delivered)
(defun md5-encode (get-data-function)
  (declare (values a b c d))
  (with-stack-array (x 16 :element-type '(unsigned-byte 32))
    (with-md5-state (a b c d)
      (initialize-md5-state a b c d)
      (loop 
	(if (funcall get-data-function x)
	    (with-md5-state (aa bb cc dd)
	      ; debugging hack: (dump-md5-data-buffer x)
	      (setq aa a bb b cc c dd d)
	      ;; Round 1
	      (md5-generate-code-for-one-round
		md5-function-f (a b c d) x (0 1) (7 12 17 22) 1)
	      ;; Round 2
	      (md5-generate-code-for-one-round
		md5-function-g (a b c d) x (1 5) (5 9 14 20) 17)
	      ;; Round 3
	      (md5-generate-code-for-one-round
		md5-function-h (a b c d) x (5 3) (4 11 16 23) 33)
	      ;; Round 4
	      (md5-generate-code-for-one-round
		md5-function-i (a b c d) x (0 7) (6 10 15 21) 49)
	      (setq a (32-add a aa)
		    b (32-add b bb)
		    c (32-add c cc)
		    d (32-add d dd)))
	    (return (values a b c d)))))))

(defun dump-md5-data-buffer (buffer)
  (fresh-line *trace-output*)
  (dotimes (i 16)
    (format *trace-output* "#x~8,'0x " (aref buffer i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; converting the result to a byte vector

(defmacro md5-digest-byte-vector-big-endian (a b c d)
  (flet ((bytes (num32)
	   `((ldb (byte 8 24) ,num32)
	     (ldb (byte 8 16) ,num32)
	     (ldb (byte 8 8) ,num32)
	     (ldb (byte 8 0) ,num32))))
    `(let ((a ,a) (b ,b) (c ,c) (d ,d))
       (vector ,@(bytes 'a)
	       ,@(bytes 'b)
	       ,@(bytes 'c)
	       ,@(bytes 'd)))))

(defmacro md5-digest-byte-vector-little-endian (a b c d)
  (flet ((bytes (num32)
	   `((ldb (byte 8 0) ,num32)
	     (ldb (byte 8 8) ,num32)
	     (ldb (byte 8 16) ,num32)
	     (ldb (byte 8 24) ,num32))))
    `(let ((a ,a) (b ,b) (c ,c) (d ,d))
       (vector ,@(bytes 'a)
	       ,@(bytes 'b)
	       ,@(bytes 'c)
	       ,@(bytes 'd)))))

;;; The MD5 specification says that the bytes should be in little-endian order.

(defmacro md5-digest-byte-vector (a b c d)
  "Constructs a byte vector from the 4 values returned by MD5-ENCODE."
  `(md5-digest-byte-vector-little-endian ,a ,b ,c ,d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PGP adds some stuff to the source when generating a signature

(defun universal-time-to-PGP-timestamp (universal-time)
  ;; (encode-universal-time 0 0 0 1 1 1970 0)
  (- universal-time 2208988800))

(defun PGP-timestamp-to-universal-time (pgp-timestamp)
  (+ pgp-timestamp 2208988800))

(defun make-pgp-reader-function (source-reader-function pgp-class-byte pgp-timestamp)
  "Given a function which returns sucessive bytes from s source, construct a 
new source function which gets its input from the original and then appends the
provided information for PGP."
  (let ((state :source))
    (flet ((pgp-reader-function ()
	     (if (eq state :source)
		 (let ((data (funcall source-reader-function)))
		   (if data
		       data
		       (progn (setq state 0)
			      pgp-class-byte)))
		 (if (< state 4)
		     (prog1
		       (etypecase pgp-timestamp
			 (integer (ldb (byte 8 (* 8 (- 3 state))) pgp-timestamp))
			 (vector (aref pgp-timestamp state)))
		       (incf state))
		     nil))))
      #'pgp-reader-function)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; useful for defining sources

;;; Define a constructor function for a source.  The constructor
;;; function returns a function suitable as the GET-DATA-FUNCTION
;;; argument to MD5-ENCODE.

;;; Only knows how to deal with 8 bit sources.

(defun make-md5-buffer-filler-from-reader-function (reader-function)
  "Returns a function which can be used as the GET-DATA-FUNCTION argument to MD5-ENCODE.
The argument is a function of no arguments, which returns sucessive 8 bit bytes from 
the source to be encoded, or NIL when the source is exhausted"
  (let ((state :data)
	(count 0)
	;; the following is used to indicate if the first (#80) pad byte was
	;; written when the index i was 13:
	(flag13 nil))
    (flet ((fill-md5-buffer (buffer)
	     (dotimes (i 16 t)
	       ;; I is which 32 bit word of buffer to write to.
	       (flet ((gb ()
			(ecase state
			  (:done (return-from fill-md5-buffer nil))
			  (:data
			    (let ((byte (funcall reader-function)))
			      (if byte
				  (progn (incf count)
					 byte)
				  (progn (setq state :must-pad)
					 (when (= i 13)
					   (setq flag13 1))
					 #x80))))		;first pad byte
			  ;; If we start writing the padding during the 14th
			  ;; word, we must pad the entire buffer and write
			  ;; the length in the next one.
			  (:must-pad
			    ;; this takes care of case when #x80 is the
			    ;; last byte written when i=13, and the next
			    ;; byte is first byte of i=14, in which case
			    ;; length should be written, not another full 
			    ;; buffer of zeroes
			    (when (and (= i 14) flag13)
			      (setq state :pad))
			    (unless (= i 14)
			      (setq state :pad)
			      (setq flag13 nil))
			    0)
			  (:pad
			    (if (= i 14)
				(multiple-value-bind (md5-length64-hi md5-length64-lo)
				    (md5-length64 (* 8 count))
				  (setf (aref buffer 14)
					md5-length64-lo)
				  (setf (aref buffer 15)
					md5-length64-hi)
				  (setq state :done)
				  (return-from fill-md5-buffer t))
				0)))))
		 ;; RFC1211:  "a sequence of bytes can be interpreted as a sequence
		 ;; of 32-bit words, where each consecutive group of four bytes is
		 ;; interpreted as a word with the low-order (least significant)
		 ;; byte given first."
		 (let ((b0 (gb)) (b1 (gb)) (b2 (gb)) (b3 (gb)))
		   (setf (aref buffer i)
			 (dpb b3 (byte 8 24)
			      (dpb b2 (byte 8 16)
				   (dpb b1 (byte 8 8)
					b0)))))))))
      #'fill-md5-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sources

;;; The reader function returns the next byte from the source, or NIL if the
;;; source is exhausted.

;;; provide a way to hack around the lisp system's idea of character sets in an
;;; implementation independent way.
(defmacro character-to-byte (char)
  #+Genera `(scl:ascii-code ,char)
  #-Genera `(char-int ,char))

(defun make-string-reader-function (string &optional (start 0) end)
  (let ((index start)
	(end (or end (length string))))
    (flet ((string-reader-function ()
	     (if (< index end)
		 (prog1 (character-to-byte (aref string index))
			(incf index))
		 nil)))
      #'string-reader-function)))

(defun md5-encode-string (string &optional (start 0) end)
  (md5-encode
    (make-md5-buffer-filler-from-reader-function
      (make-string-reader-function string start end))))


;;; should anything happen between strings?  eg. return newline?
(defun make-list-of-strings-reader-function (list-of-strings)
  (let ((strings list-of-strings)
	string index end)
    (flet ((list-of-strings-read-function ()
	     (flet ((next-string ()
		      (setq string (pop strings))
		      (unless string
			(return-from list-of-strings-read-function nil))
		      (setq index 0)
		      (setq end (length string))))
	       (unless string
		 (next-string))
	       (loop
		 (when (< index end)
		   (return))
		 (next-string))
	       (prog1 (aref string index)
		      (incf index)))))
      #'list-of-strings-read-function)))


;;; This reader function can be composed with others if you want to selectively
;;; ignore some elements of the input when computing the MD5 hash.
(defun make-filtered-reader-function (reader-function predicate)
  (flet ((alpha-digit-only-reader-function ()
	   (loop
	     (let ((c (funcall reader-function)))
	       (unless c
		 (return nil))
	       (when (funcall predicate c)
		 (return c))))))
    #'alpha-digit-only-reader-function))


(defun md5-encode-file-for-pgp (pathname pgp-class-byte pgp-timestamp)
  (with-open-file (stream pathname :direction :input)
    (multiple-value-bind (a b c d)
	(md5-encode
	  (make-md5-buffer-filler-from-reader-function
	    (make-pgp-reader-function
	      #'(lambda ()
		  (let ((c (read-char stream nil nil)))
		    (when c (character-to-byte c))))
	      pgp-class-byte pgp-timestamp)))
      (md5-digest-byte-vector a b c d))))

;;; ---------------------------------------------------------------------------
;;; a bit o' the CLOS
;;; ---------------------------------------------------------------------------

(defgeneric md5 (thing)
  (:documentation "Returns the MD5 encoding of thing as best it can."))

;;; ---------------------------------------------------------------------------

(defmethod md5 ((thing string))
  (md5-encode-string thing))

;;; ---------------------------------------------------------------------------

(defgeneric md5->string (thing)
  (:documentation "Returns the MD5 encoding of thing as a single string. The output
does not match, e.g., md5 on the Unix I have but I'm not sure what it "should" be yet.
At least it is self consistent."))

;;; ---------------------------------------------------------------------------

(defmethod md5->string (thing)
  (multiple-value-bind (a b c d) (md5 thing)
    (format nil "#x~8,'0x~8,'0x~8,'0x~8,'0x" a b c d)))



#|
;;; RFC1321 test suite

(defparameter +md5-test-vector+
	      '(("" #xd41d8cd98f00b204e9800998ecf8427e)
		("a" #x0cc175b9c0f1b6a831c399e269772661)
		("abc" #x900150983cd24fb0d6963f7d28e17f72)
		("message digest" #xf96b697d7cb7938d525a2f31aaf161d0)
		("abcdefghijklmnopqrstuvwxyz" #xc3fcd3d76192e4007dfb496cca67e13b)
		("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
		 #xd174ab98d277d9f5a5611c2c9f419d9f)
		("12345678901234567890123456789012345678901234567890123456789012345678901234567890"
		 #x57edf4a22be3c955ac49da2e2107b67a)))

(defun test-md5-strings ()
  (format t "~&String sources:")
  (dolist (v +md5-test-vector+)
    (apply #'test-md5-vector (make-string-reader-function (first v)) v))
  (format t "~&Done"))

;(defun test-md5-streams ()
;  (format t "~&Stream sources:")
;  (dolist (v +md5-test-vector+)
;    (with-input-from-string (stream (first v))
;      (apply #'test-md5-vector (make-stream-md5-source stream) v)))
;  (format t "~&Done"))

(defun test-md5-vector (source string expected-result)
  (let ((position 128)
	(results (multiple-value-list
		   (md5-encode (make-md5-buffer-filler-from-reader-function source)))))
    (dolist (word results)	; (a b c d)
      (dotimes (i 4)
	(decf position 8)
	(unless (= (ldb (byte 8 (* i 8)) word)
		   (ldb (byte 8 position) expected-result))
	  (format t "~&MD5 failed at ~d ~d ~x~
                     ~&  for string~15t~s~
                     ~&  expected~15t~x~
                     ~&  got~15t~{~x ~}"
                  position i word
		  string expected-result results)
	  (return-from test-md5-vector nil))))
    t))

(defun test-md5-size (size &key (stream *standard-output*)
		      (data (make-string 513 :initial-element #\x)))
  (multiple-value-bind (a b c d)
      (md5-encode (make-md5-buffer-filler-from-reader-function
		    (make-string-reader-function data 0 size)))
    (format stream "~&(~3d~10t#x~8,'0x  #x~8,'0x  #x~8,'0x  #x~8,'0x)"
	    size a b c d)))

(defun test-md5-sizes (&optional (stream *standard-output*))
  (let* ((size 513)
	 (source (make-string size :initial-element #\x)))
    (dotimes (size (1- size))
      (test-md5-size size :data source :stream stream))))

|#
