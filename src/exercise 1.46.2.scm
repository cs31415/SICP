(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)


; sqrt in terms of iterative-improve
(define tolerance 0.0001)

(define (sqrt x) 
  ((iterative-improve 
    (lambda (guess) (< (abs (- x (square guess))) tolerance))
    (lambda (guess) (average guess (/ x guess)))) 1.0))

(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))

(sqrt 25)
;5.000000000053722



; fixed-point in terms of iterative-improve
(define (fixed-point f first-guess)
  ((iterative-improve 
    (lambda (guess) (< (abs (- (f guess) guess)) tolerance)) 
    (lambda (guess) (f guess))) 
   first-guess))

(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt2 25)


