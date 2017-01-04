(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (apply-generic op arg) (arg op))

;Note that the value returned by make-from-real-imag is
;a procedure -- the internal dispatch procedure. This is
;the procedure that is invoked when apply-generic
;requests an operation to be performed.

;This style of programming is called message passing. 
;The name comes from the image that a data object is an 
;entity that receives the requested operation name as 
;a ``message.'' 


