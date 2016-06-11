(define (cons x y)
  (lambda (m) (m x y)))

; cons returns a lambda that takes a procedure as argument and 
; calls the procedure passing it x and y as arguments.

(define (car z)
  (z (lambda (p q) p)))

; car calls the procedure returned by cons, passing it a procedure
; that returns the 1st of it's 2 arguments

; car calls the procedure returned by cons, passing it a procedure
; that returns the 2nd of it's 2 arguments


(define pair (cons 1 2))
(car pair)
1
(cdr pair)
2


; Using substitution model
(car pair)
(car (cons 1 2))
(car (lambda (m) (m 1 2)))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p) 1 2)
(1)

