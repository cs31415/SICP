; A fixed point of a function f satisfies the equation f(x)=x

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

; We can use this method to approximate the fixed point of the cosine function
(fixed-point cos 1.0)

; We can also use this method to solve the equation y=sin y + cos y
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; We can also use this method to calculate square root.
; y^2=x,  thus, y=x/y. i.e. if we find y, a fixed point of x/y, we
; will have found the square root of x.

;(define (sqrt x)
;  (fixed-point (lambda (y) (/ x y)) 1.0))

; (sqrt 4)  -- infinite loop since y2=x/y1, y3=x/y2=x/x/y1=y1

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average x y) (/ (+ x y) 2))
; y=x/y
; y/2 = x/2y
; y/2+y/2 = y/2+x/2y
; y = (y+x/y)/2
; This approach of averaging successive approximations to a solution, a technique we 
; that we call *average damping*, often aids the convergence of fixed-point searches.


(sqrt 4)
