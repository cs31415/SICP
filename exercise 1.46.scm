;Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve f x)
  (define (try guess)
    (if (good-enough? (f guess) x)
        guess
        (try (improve guess))))
  (lambda (guess) (try guess)))

(define (good-enough? v1 v2)
  (< (abs (- v1 v2)) 0.001))

(define (average a b) (/ (+ a b) 2))

; --- sqrt definition ---
(define (square x) (* x x))

(define (sqrt x)
  ((iterative-improve good-enough? (lambda (guess) (average guess (/ x guess))) square x) 1.0))


(sqrt 25)

; --- fixed-point definition ---
(define tolerance 0.00001)

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next 
;          (try next))))
;  (try first-guess))

(define (fixed-point f first-guess)
  ((iterative-improve good-enough? (lambda (guess) (f guess)) f first-guess) first-guess))

(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               2.0))

(sqrt2 25)