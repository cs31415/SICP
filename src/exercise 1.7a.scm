(define (average x y)
(/ (+ x y) 2))

(define (improve guess x)
(average guess (/ x guess)))
 
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))
 
(define (good-enough? guess guessprev)
(< (/ (abs (- guess guessprev)) guess) 0.000001))
 
(define (sqrt-iter guess x guessprev)
(if (good-enough? guess guessprev)
guess
(sqrt-iter (improve guess x) x guess)))
 
(define (sqrt2 x)
(sqrt-iter 1.0 x 0))
 
(define (square x) (* x x))
 
; test
(sqrt2 9)
(sqrt2 (+ 100 37))
(sqrt2 (+ (sqrt2 2) (sqrt2 3)))
(square (sqrt2 1000))
 
; very small numbers
(sqrt2 0.0001)
; 0.03230844833048122
(sqrt 0.0001)
; 0.01
 
; very large numbers
(sqrt 4.2e50)
(sqrt2 4.2e50)
