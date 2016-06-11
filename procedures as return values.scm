; Procedures as returned values

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))


(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

; We can redefine the sqrt calculation by fixed point method using
; the notion of average damping

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y)))
;               1.0))

; The new definition
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 4)

; The sqrt formulation can be largely reused to calc cube roots.
; A cube root of x is a fixed point of the function y -> x/y^2
; |-> (pronounced ``maps to'') is the mathematician's way of writing lambda. 
; y |-> x/y means (lambda(y) (/ x y)), that is, the function whose value
; at y is x/y.


(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))



; Implementing Newton's method as a procedure

; if x->g(x) is a differentiable function, then a root of g(x) is a fixed point of 
; the function x->f(x), where
; f(x) = x - g(x)/g'(x)
; where g'(x) is the derivative of g(x) 

; g'(x) = (g(x+dx)-g(x))/dx for very small dx
; Thus,

(define dx 0.000001)
(define (deriv g)
  (lambda(x) (/ (- (g (+ x dx)) (g x)) dx)))

; deriv is a procedure that takes a procedure as argument and returns a procedure

(define (cube x) (* x x x))

((deriv cube) 5)
;75.00001501625775

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))


(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt2 x)
  (newton-method (lambda(y) (- (square y) x)) 1.0))

(sqrt2 4)

  

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda(y) (/ x y)) average-damp 1.0))

(sqrt3 4)

(define (sqrt4 x)
  (fixed-point-of-transform (lambda(y) (- (square y) x)) newton-transform 1.0))

(sqrt4 4)
