; Copying over newton-method and it's dependencies from previous problems

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


; deriv is a procedure that takes a procedure as argument and returns a procedure
(define dx 0.000001)
(define (deriv g)
  (lambda(x) (/ (- (g (+ x dx)) (g x)) dx)))


(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))


(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newton-method (cubic 1 2 3) 1)
;-1.2756822036508881

(newton-method (cubic 1 1 1) 1)
;-0.999999999999977

; Verifying
((cubic 1 2 3) -1.2756822036508881)
;4.192202140984591e-13

((cubic 1 1 1) -0.999999999999977)
;4.596323321948148e-14

