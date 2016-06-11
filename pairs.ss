(define (cons1 x y)
  (lambda (m) (m x y)))
; cons returns a function f(m) that takes a function m as argument ; and whose body calls m passing in constants x,y
(define (car1 z)
  (z (lambda (p q) p)))
; cdr of z calls function z passing in lambda (p q) p
(define (cdr1 z)
  (z (lambda (p q) q)))
;(car1 (cons1 0 1))
;(car1 (lambda (m) (m 0 1)))
((lambda (m) (m 0 1)) (lambda (p q) p))
((lambda (m) (m 0 1)) (lambda (p q) q))





