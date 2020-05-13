(define *num-saved-gtyvars* 19)
(define *pre-defined-strictness-size* 7)  ; length of max strictness list
(define *pre-defined-strictness-table* '())
(define *pre-defined-strictness-vars* 32) ; number of global vars
(define *pre-defined-strictness-names*
  (make-vector *pre-defined-strictness-vars*))

;(dotimes (i *pre-defined-strictness-vars*)
  ;(declare (type unsigned-byte i))
;  (setf (vector-ref *pre-defined-strictness-names* i)
;	(string->symbol (format '#f "SAVED-STRICTNESS-~A" i))))

(do ((i 0 (+ i 1)))
    ((>= i *pre-defined-strictness-vars*) '#f)
  (setf (vector-ref *pre-defined-strictness-names* i)
          (string->symbol (format '#f "SAVED-STRICTNESS-~A" i))))

(define *saved-gtyvars* '())
(define *saved-gtyvar-varnames* (make-vector *num-saved-gtyvars*))
;(dotimes (i *num-saved-gtyvars*)
;  (setf (vector-ref *saved-gtyvar-varnames* i)
;	(string->symbol (format '#f "SAVED-GTYVAR-NAME~A" i))))

(do ((i 0 (+ i 1)))
    ((>= i *num-saved-gtyvars*) '#f)
  (setf (vector-ref *saved-gtyvar-varnames* i)
          (string->symbol (format '#f "SAVED-GTYVAR-NAME~A" i))))
