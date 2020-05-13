(define-syntax (set-strictness-vars)
  (let ((res '()))
    (do ((i 0 (+ i 1)))
        ((>= i *pre-defined-strictness-vars*) '#f)
       (push `(setf (dynamic ,(vector-ref *pre-defined-strictness-names* i))
		    (vector-ref *pre-defined-strictness-table* ',i))
	     res))
    `(begin ,@res)))

(define-syntax (setup-gtyvar-vars)
 (let ((res '()))
   (do ((i 0 (+ i 1)))
       ((>= i *num-saved-gtyvars*) '#f)
     (push `(setf (dynamic ,(vector-ref *saved-gtyvar-varnames* i))
		  (vector-ref *saved-gtyvars* ',i))
	   res))
   `(begin ,@res)))

(define-syntax (assq/insert x table)
  `(let ((res (assq ,x ,table)))
     (if (eqv? res '#f)
	 (begin
	   (let ((new-pair (cons ,x '#f)))
	     (push new-pair ,table)
	     new-pair))
	 res)))

(define-syntax (assq/insert-l x table)
  `(let ((res (assq ,x ,table)))
     (if (eqv? res '#f)
	 (begin
	   (let ((new-pair (cons ,x '())))
	     (push new-pair ,table)
	     new-pair))
	 res)))




