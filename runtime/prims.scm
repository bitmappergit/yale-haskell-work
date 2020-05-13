;;; prims.scm -- definitions for primitives
;;;
;;; author :  Sandra Loosemore
;;; date   :  9 Jun 1992
;;;
;;; WARNING!!!  This file contains Common-Lisp specific code.
;;;


;;; Helper stuff

(define-integrable (is-fixnum? x)
  (cl:typep x 'cl:fixnum))

(define-integrable (is-integer? x)
  (cl:typep x 'cl:integer))

(define-integrable (is-single-float? x)
  (cl:typep x 'cl:single-float))

(define-integrable (is-double-float? x)
  (cl:typep x 'cl:double-float))

(define-syntax (the-fixnum x)
  `(cl:the cl:fixnum ,x))

(define-syntax (the-integer x)
  `(cl:the cl:integer ,x))

(define-syntax (the-single-float x)
  `(cl:the cl:single-float ,x))

(define-syntax (the-double-float x)
  `(cl:the cl:double-float ,x))

(define-syntax (make-haskell-tuple2 x y)
  `(make-tuple (box ,x) (box ,y)))

;;; Abort
;;; *** Should probably do something other than just signal an error.

(define (prim.abort s)
  (haskell-runtime-error s))

(define (haskell-string->list s)
  (if (null? s)
      '()
      (cons (integer->char (force (car s)))
	    (haskell-string->list (force (cdr s))))))

;;; Char

(define-syntax (prim.char-to-int c)
  `(the-fixnum ,c))

(define-syntax (prim.int-to-char i)
  `(the-fixnum ,i))

(define-syntax (prim.eq-char i1 i2)
  `(= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-eq-char i1 i2)
  `(not (= (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.le-char i1 i2)
  `(<= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-le-char i1 i2)
  `(> (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-lt-char i1 i2)
  `(>= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.lt-char i1 i2)
  `(< (the-fixnum ,i1) (the-fixnum ,i2)))

(define-integrable prim.max-char 255)


;;; Floating

(define-syntax (prim.eq-float f1 f2)
  `(= (the-single-float ,f1) (the-single-float ,f2)))
(define-syntax (prim.not-eq-float f1 f2)
  `(not (= (the-single-float ,f1) (the-single-float ,f2))))
(define-syntax (prim.le-float f1 f2)
  `(<= (the-single-float ,f1) (the-single-float ,f2)))
(define-syntax (prim.not-le-float f1 f2)
  `(> (the-single-float ,f1) (the-single-float ,f2)))
(define-syntax (prim.not-lt-float f1 f2)
  `(>= (the-single-float ,f1) (the-single-float ,f2)))
(define-syntax (prim.lt-float f1 f2)
  `(< (the-single-float ,f1) (the-single-float ,f2)))

(define-syntax (prim.eq-double f1 f2)
  `(= (the-double-float ,f1) (the-double-float ,f2)))
(define-syntax (prim.not-eq-double f1 f2)
  `(not (= (the-double-float ,f1) (the-double-float ,f2))))
(define-syntax (prim.le-double f1 f2)
  `(<= (the-double-float ,f1) (the-double-float ,f2)))
(define-syntax (prim.not-le-double f1 f2)
  `(> (the-double-float ,f1) (the-double-float ,f2)))
(define-syntax (prim.not-lt-double f1 f2)
  `(>= (the-double-float ,f1) (the-double-float ,f2)))
(define-syntax (prim.lt-double f1 f2)
  `(< (the-double-float ,f1) (the-double-float ,f2)))

(define-syntax (prim.float-max f1 f2)
  `(the-single-float (max (the-single-float ,f1) (the-single-float ,f2))))
(define-syntax (prim.float-min f1 f2)
  `(the-single-float (min (the-single-float ,f1) (the-single-float ,f2))))

(define-syntax (prim.double-max f1 f2)
  `(the-double-float (max (the-double-float ,f1) (the-double-float ,f2))))
(define-syntax (prim.double-min f1 f2)
  `(the-double-float (min (the-double-float ,f1) (the-double-float ,f2))))

(define-syntax (prim.plus-float f1 f2)
  `(the-single-float (+ (the-single-float ,f1) (the-single-float ,f2))))
(define-syntax (prim.minus-float f1 f2) 
  `(the-single-float (- (the-single-float ,f1) (the-single-float ,f2))))
(define-syntax (prim.mul-float f1 f2)
  `(the-single-float (* (the-single-float ,f1) (the-single-float ,f2))))
(define-syntax (prim.div-float f1 f2)
  `(the-single-float (/ (the-single-float ,f1) (the-single-float ,f2))))

(define-syntax (prim.plus-double f1 f2)
  `(the-double-float (+ (the-double-float ,f1) (the-double-float ,f2))))
(define-syntax (prim.minus-double f1 f2) 
  `(the-double-float (- (the-double-float ,f1) (the-double-float ,f2))))
(define-syntax (prim.mul-double f1 f2)
  `(the-double-float (* (the-double-float ,f1) (the-double-float ,f2))))
(define-syntax (prim.div-double f1 f2)
  `(the-double-float (/ (the-double-float ,f1) (the-double-float ,f2))))


(define-syntax (prim.neg-float f)
  `(the-single-float (- (the-single-float ,f))))

(define-syntax (prim.neg-double f)
  `(the-double-float (- (the-double-float ,f))))

(define-syntax (prim.abs-float f)
  `(the-single-float (cl:abs (the-single-float ,f))))

(define-syntax (prim.abs-double f)
  `(the-double-float (cl:abs (the-double-float ,f))))


(define-syntax (prim.exp-float f)
  `(the-single-float (cl:exp (the-single-float ,f))))
(define-syntax (prim.log-float f)
  `(the-single-float (cl:log (the-single-float ,f))))
(define-syntax (prim.sqrt-float f)
  `(the-single-float (cl:sqrt (the-single-float ,f))))
(define-syntax (prim.sin-float f)
  `(the-single-float (cl:sin (the-single-float ,f))))
(define-syntax (prim.cos-float f)
  `(the-single-float (cl:cos (the-single-float ,f))))
(define-syntax (prim.tan-float f)
  `(the-single-float (cl:tan (the-single-float ,f))))
(define-syntax (prim.asin-float f)
  `(the-single-float (cl:asin (the-single-float ,f))))
(define-syntax (prim.acos-float f)
  `(the-single-float (cl:acos (the-single-float ,f))))
(define-syntax (prim.atan-float f)
  `(the-single-float (cl:atan (the-single-float ,f))))
(define-syntax (prim.sinh-float f)
  `(the-single-float (cl:sinh (the-single-float ,f))))
(define-syntax (prim.cosh-float f)
  `(the-single-float (cl:cosh (the-single-float ,f))))
(define-syntax (prim.tanh-float f)
  `(the-single-float (cl:tanh (the-single-float ,f))))
(define-syntax (prim.asinh-float f)
  `(the-single-float (cl:asinh (the-single-float ,f))))
(define-syntax (prim.acosh-float f)
  `(the-single-float (cl:acosh (the-single-float ,f))))
(define-syntax (prim.atanh-float f)
  `(the-single-float (cl:atanh (the-single-float ,f))))


(define-syntax (prim.exp-double f)
  `(the-double-float (cl:exp (the-double-float ,f))))
(define-syntax (prim.log-double f)
  `(the-double-float (cl:log (the-double-float ,f))))
(define-syntax (prim.sqrt-double f)
  `(the-double-float (cl:sqrt (the-double-float ,f))))
(define-syntax (prim.sin-double f)
  `(the-double-float (cl:sin (the-double-float ,f))))
(define-syntax (prim.cos-double f)
  `(the-double-float (cl:cos (the-double-float ,f))))
(define-syntax (prim.tan-double f)
  `(the-double-float (cl:tan (the-double-float ,f))))
(define-syntax (prim.asin-double f)
  `(the-double-float (cl:asin (the-double-float ,f))))
(define-syntax (prim.acos-double f)
  `(the-double-float (cl:acos (the-double-float ,f))))
(define-syntax (prim.atan-double f)
  `(the-double-float (cl:atan (the-double-float ,f))))
(define-syntax (prim.sinh-double f)
  `(the-double-float (cl:sinh (the-double-float ,f))))
(define-syntax (prim.cosh-double f)
  `(the-double-float (cl:cosh (the-double-float ,f))))
(define-syntax (prim.tanh-double f)
  `(the-double-float (cl:tanh (the-double-float ,f))))
(define-syntax (prim.asinh-double f)
  `(the-double-float (cl:asinh (the-double-float ,f))))
(define-syntax (prim.acosh-double f)
  `(the-double-float (cl:acosh (the-double-float ,f))))
(define-syntax (prim.atanh-double f)
  `(the-double-float (cl:atanh (the-double-float ,f))))


(define-integrable prim.pi-float (cl:coerce cl:pi 'cl:single-float))

(define-integrable prim.pi-double (cl:coerce cl:pi 'cl:double-float))


;;; Assumes rationals are represented as a 2-tuple of integers

(define (prim.rational-to-float x)
  (let ((n (tuple-select 2 0 x))
	(d (tuple-select 2 1 x)))
    (if (eqv? d 0)
	(haskell-runtime-error "Divide by 0.")
	(prim.rational-to-float-aux n d))))

(define (prim.rational-to-float-aux n d)
  (declare (type integer n d))
  (/ (cl:coerce n 'cl:single-float)
     (cl:coerce d 'cl:single-float)))

(define (prim.rational-to-double x)
  (let ((n (tuple-select 2 0 x))
	(d (tuple-select 2 1 x)))
    (if (eqv? d 0)
	(haskell-runtime-error "Divide by 0.")
	(prim.rational-to-double-aux n d))))

(define (prim.rational-to-double-aux n d)
  (declare (type integer n d))
  (/ (cl:coerce n 'cl:double-float)
     (cl:coerce d 'cl:double-float)))

(define (prim.float-to-rational x)
  (let ((r  (cl:rational (the cl:single-float x))))
    (declare (type rational r))
    (make-tuple (cl:numerator r) (cl:denominator r))))

(define (prim.double-to-rational x)
  (let ((r  (cl:rational (the cl:double-float x))))
    (declare (type rational r))
    (make-tuple (cl:numerator r) (cl:denominator r))))


(define-integrable prim.float-1 (cl:coerce 1.0 'cl:single-float))
(define-integrable prim.double-1 (cl:coerce 1.0 'cl:double-float))

(define-integrable prim.float-digits
  (cl:float-digits prim.float-1))

(define-integrable prim.double-digits
  (cl:float-digits prim.double-1))

(define-integrable prim.float-radix
  (cl:float-radix prim.float-1))

(define-integrable prim.double-radix
  (cl:float-radix prim.double-1))


;;; Sometimes least-positive-xxx-float is denormalized.

(define-integrable prim.float-min-exp
  (multiple-value-bind (m e)
      (cl:decode-float
        #+lucid lcl:least-positive-normalized-single-float
	#-lucid cl:least-positive-single-float)
    (declare (ignore m))
    e))

(define-integrable prim.double-min-exp
  (multiple-value-bind (m e)
      (cl:decode-float
        #+lucid lcl:least-positive-normalized-double-float
	#-lucid cl:least-positive-double-float)
    (declare (ignore m))
    e))

(define-integrable prim.float-max-exp
  (multiple-value-bind (m e)
      (cl:decode-float cl:most-positive-single-float)
    (declare (ignore m))
    e))

(define-integrable prim.double-max-exp
  (multiple-value-bind (m e)
      (cl:decode-float cl:most-positive-double-float)
    (declare (ignore m))
    e))

(define-integrable (prim.float-range x)
  (declare (ignore x))
  (make-haskell-tuple2 prim.float-min-exp prim.float-max-exp))

(define-integrable (prim.double-range x)
  (declare (ignore x))
  (make-haskell-tuple2 prim.double-min-exp prim.double-max-exp))


;;; *** I'm not sure if these are correct.  Should the exponent value
;;; *** be taken as the value that cl:integer-decode-float returns,
;;; *** or as the value that cl:decode-float returns?  (They're
;;; *** not the same because the significand is scaled differently.)
;;; *** I'm guessing that Haskell's model is to use the actual numbers
;;; *** that are in the bit fields 

;;; jcp - I removed this since Haskell requires an integer instead of a
;;; fractional mantissa.  My theory is that integer-decode-float returns
;;; what Haskell wants without fiddling (except sign reattachment)

(define (exponent-adjustment m)
  (if (eqv? prim.float-radix 2)
      ;; the usual case -- e.g. IEEE floating point
      (cl:integer-length m)
      (cl:ceiling (cl:log m prim.float-radix))))

(define (prim.decode-float f)
  (multiple-value-bind (m e s)
      (cl:integer-decode-float (the-single-float f))
    (make-haskell-tuple2 (* (the-integer m) (the-fixnum s))
			 (the-fixnum e))))

(define (prim.decode-double f)
  (multiple-value-bind (m e s)
      (cl:integer-decode-float (the-double-float f))
    (make-haskell-tuple2 (* (the-integer m) (the-fixnum s))
			 (the-fixnum e))))

(define (prim.encode-float m e)
  (cl:scale-float (cl:coerce m 'cl:single-float) (the-fixnum e)))

(define (prim.encode-double m e)
  (cl:scale-float (cl:coerce m 'cl:double-float) (the-fixnum e)))


;;; Integral

(define-syntax (prim.eq-int i1 i2)
  `(= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-eq-int i1 i2)
  `(not (= (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.le-int i1 i2)
  `(<= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-le-int i1 i2)
  `(> (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.not-lt-int i1 i2)
  `(>= (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.lt-int i1 i2)
  `(< (the-fixnum ,i1) (the-fixnum ,i2)))
(define-syntax (prim.int-max i1 i2)
  `(the-fixnum (max (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.int-min i1 i2)
  `(the-fixnum (min (the-fixnum ,i1) (the-fixnum ,i2))))

(define-syntax (prim.eq-integer i1 i2)
  `(= (the-integer ,i1) (the-integer ,i2)))
(define-syntax (prim.not-eq-integer i1 i2)
  `(not (= (the-integer ,i1) (the-integer ,i2))))
(define-syntax (prim.le-integer i1 i2)
  `(<= (the-integer ,i1) (the-integer ,i2)))
(define-syntax (prim.not-le-integer i1 i2)
  `(> (the-integer ,i1) (the-integer ,i2)))
(define-syntax (prim.not-lt-integer i1 i2)
  `(>= (the-integer ,i1) (the-integer ,i2)))
(define-syntax (prim.lt-integer i1 i2)
  `(< (the-integer ,i1) (the-integer ,i2)))
(define-syntax (prim.integer-max i1 i2)
  `(the-integer (max (the-integer ,i1) (the-integer ,i2))))
(define-syntax (prim.integer-min i1 i2)
  `(the-integer (min (the-integer ,i1) (the-integer ,i2))))


(define-syntax (prim.plus-int i1 i2)
  `(the-fixnum (+ (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.minus-int i1 i2)
  `(the-fixnum (- (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.mul-int i1 i2)
  `(the-fixnum (* (the-fixnum ,i1) (the-fixnum ,i2))))
(define-syntax (prim.neg-int i)
  `(the-fixnum (- (the-fixnum ,i))))
(define-syntax (prim.abs-int i)
  `(the-fixnum (cl:abs (the-fixnum ,i))))

(define-integrable prim.minint cl:most-negative-fixnum)
(define-integrable prim.maxint cl:most-positive-fixnum)

(define-syntax (prim.plus-integer i1 i2)
  `(the-integer (+ (the-integer ,i1) (the-integer ,i2))))
(define-syntax (prim.minus-integer i1 i2)
  `(the-integer (- (the-integer ,i1) (the-integer ,i2))))
(define-syntax (prim.mul-integer i1 i2)
  `(the-integer (* (the-integer ,i1) (the-integer ,i2))))
(define-syntax (prim.neg-integer i)
  `(the-integer (- (the-integer ,i))))
(define-syntax (prim.abs-integer i)
  `(the-integer (cl:abs (the-integer ,i))))


(define (prim.div-rem-int i1 i2)
  (multiple-value-bind (q r)
      (cl:truncate (the-fixnum i1) (the-fixnum i2))
    (make-tuple (box (the-fixnum q)) (box (the-fixnum r)))))

(define (prim.div-rem-integer i1 i2)
  (multiple-value-bind (q r)
      (cl:truncate (the-integer i1) (the-integer i2))
    (make-tuple (box (the-integer q)) (box (the-integer r)))))

(define (prim.integer-to-int i)
  (if (is-fixnum? i)
      (the-fixnum i)
      (haskell-runtime-error "Integer -> Int overflow.")))

(define-syntax (prim.int-to-integer i)
  i)

;;; Binary

(define prim.nullbin '())

(define (prim.is-null-bin x)
  (null? x))

(define (prim.show-bin-int i b)
  (cons i b))

(define (prim.show-bin-integer i b)
  (cons i b))

(define (prim.show-bin-float f b)
  (cons f b))

(define (prim.show-bin-double f b)
  (cons f b))

(define (prim.bin-read-error)
  (haskell-runtime-error "Error: attempt to read from an incompatible Bin."))

(define (prim.read-bin-int b)
  (if (or (null? b) (not (is-fixnum? (car b))))
      (prim.bin-read-error)
      (make-haskell-tuple2 (car b) (cdr b))))

(define (prim.read-bin-integer b)
  (if (or (null? b) (not (is-integer? (car b))))
      (prim.bin-read-error)
      (make-haskell-tuple2 (car b) (cdr b))))

(define (prim.read-bin-float b)
  (if (or (null? b) (not (is-single-float? (car b))))
      (prim.bin-read-error)
      (make-haskell-tuple2 (car b) (cdr b))))

(define (prim.read-bin-double b)
  (if (or (null? b) (not (is-double-float? (car b))))
      (prim.bin-read-error)
      (make-haskell-tuple2 (car b) (cdr b))))

(define (prim.read-bin-small-int b m)
  (if (or (null? b)
	  (not (is-fixnum? (car b)))
	  (> (the-fixnum (car b)) (the-fixnum m)))
      (prim.bin-read-error)
      (make-haskell-tuple2 (car b) (cdr b))))

(define (prim.append-bin x y)
  (append x y))


;;; String primitives

;;; Calls to prim.string-eq are generated by the CFN to pattern match
;;; against string constants.  So normally one of the arguments will be
;;; a constant string.  Treat this case specially to avoid consing up
;;; a haskell string whenever it's called.
;;; This function is strict in both its arguments.

(define-syntax (prim.string-eq s1 s2)
  (cond ((and (pair? s1)
	      (eq? (car s1) 'make-haskell-string))
	 `(prim.string-eq-inline ,(cadr s1) 0 ,(string-length (cadr s1)) ,s2))
	((and (pair? s2)
	      (eq? (car s2) 'make-haskell-string))
	 `(prim.string-eq-inline ,(cadr s2) 0 ,(string-length (cadr s2)) ,s1))
	(else
	 `(prim.string-eq-notinline ,s1 ,s2))))

(define (prim.string-eq-inline lisp-string i n haskell-string)
  (declare (type fixnum i n))
  (cond ((eqv? i n)
	 ;; Reached end of Lisp string constant -- better be at the end
	 ;; of the Haskell string, too.
	 (if (null? haskell-string) '#t '#f))
	((null? haskell-string)
	 ;; The Haskell string is too short.
	 '#f)
	((eqv? (the fixnum (char->integer (string-ref lisp-string i)))
	       (the fixnum (force (car haskell-string))))
	 ;; Next characters match, recurse
	 (prim.string-eq-inline
	   lisp-string (the fixnum (+ i 1)) n (force (cdr haskell-string))))
	(else
	 ;; No match
	 '#f)))

(define (prim.string-eq-notinline s1 s2)
  (cond ((null? s1)
	 ;; Reached end of first string.
	 (if (null? s2) '#t '#f))
	((null? s2)
	 ;; Second string too short.
	 '#f)
	((eqv? (the fixnum (force (car s1))) (the fixnum (force (car s2))))
	 (prim.string-eq-notinline (force (cdr s1)) (force (cdr s2))))
	(else
	 '#f)))

  
;;; List primitives


;;; The first argument is strict and the second is a delay.

(define-syntax (prim.append l1 l2)
  (cond ((and (pair? l1)
	      (eq? (car l1) 'make-haskell-string))
	 `(make-haskell-string-tail ,(cadr l1) ,l2))
	((equal? l1 ''())
	 `(force ,l2))
	((equal? l2 '(box '()))
	 l1)
	;; *** could also look for
	;; *** (append (cons x (box y)) z) => (cons x (box (append y z)))
	;; *** but I don't think this happens very often anyway
	(else
	 `(prim.append-aux ,l1 ,l2))))

(define (prim.append-aux l1 l2)
  (cond ((null? l1)
	 (force l2))
	((and (forced? l2) (eq? (unbox l2) '()))
	 ;; Appending nil is identity.
	 l1)
	((forced? (cdr l1))
	 ;; Append eagerly if the tail of the first list argument has 
         ;; already been forced.
	 (cons (car l1)
	       (if (null? (unbox (cdr l1)))
		   l2  ; don't force this!!
		   (box (prim.append-aux (unbox (cdr l1)) l2)))))
	(else
	 (cons (car l1) (delay (prim.append-aux (force (cdr l1)) l2))))
	))


;;; Both arguments are forced here.  Have to be careful not to call
;;; recursively with an argument of 0.
;;; *** This is no longer used.

(define (prim.take n l)
  (declare (type fixnum n))
  (cond ((not (pair? l))
	 '())
	((eqv? n 1)
	 ;; Only one element to take.
	 (cons (car l) (box '())))
	((forced? (cdr l))
	 ;; Take eagerly if the tail of the list has already been forced.
	 (cons (car l) (box (prim.take (- n 1) (unbox (cdr l))))))
	(else
	 (cons (car l) (delay (prim.take (- n 1) (force (cdr l))))))
	))
      

;;; The optimizer gets rid of all first-order calls to these functions.

(define (prim.foldr k z l)
  ;; k and z are nonstrict, l is strict
  (if (null? l)
      (force z)
      (funcall (force k)
	       (car l)
	       (delay (prim.foldr k z (force (cdr l)))))))

(define (prim.build g)
  ;; g is strict
  (funcall g
	   (box (function make-cons-constructor))
	   (box '())))
