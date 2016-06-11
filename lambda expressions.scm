Consider the procedure,
(define (addfour x) (+ x 4))

;Replace the define by lambda, eliminate the procedure name,  and you have
;the lambda representation:

(lambda (x) (+ x 4))

;Here is the call to addfour
(addfour 4)

; The lambda equivalent of this is (obtained by replacing addfour by the equivalent lambda definition):
((lambda (x) (+ x 4)) 4)

; Thus lambda is used to define a one-time use, throwaway procedure


; Lambda expressions can also be used to define local variables using 'let'
; To calculate the function:
; f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
; Let a = (1+xy), b=(1-y)
; f(x,y) = xa^2 + by + ab

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a)) (* b y) (* a b)))
  (f-helper (+ 1 (* x y)) (- 1 y)))

; Rewriting to use lambda expression instead of f-helper,

(define (f x y)
  ((lambda (a b)
    (+ (* x (square a)) (* b y) (* a b)))
   (+ 1 (* x y)) (- 1 y)))

; This is equivalent to,

(define (f x y)
  (let ((a (+ 1 (* x y))) (b (- 1 y)))
    (+ (* x (square a)) (* b y) (* a b))))

; i.e. the let construct is translated to the above lambda expression.
; No special compiler mechanism is needed to process let other than lambda
; expressions.

