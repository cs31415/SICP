;Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2^a 3^b. Give the corresponding definitions of the procedures cons, car, and cdr.

; copying repeated from exercise 1.43
(define (compose f g)
  (lambda (x) (f (g x))))
 
(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f
      ))


; define expt in terms of repeated multiplication by base
; repeated is phenomenally powerful!
(define (expt b p)
  ((repeated (lambda (x) (* x b)) (- p 1)) b))

(define (is-divisible? dividend divisor)
  (= (remainder dividend divisor) 0))

; define times-divisible as the number of times a number x is 
; divisible by a divisor
(define (times-divisible x divisor)
  (define (try dividend n)
    (if (is-divisible? dividend divisor)
        (try (/ dividend divisor) (+ n 1))
        n
    ))
  (try x 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

; car is simply the number of times z can be divided by 2
(define (car z)
  (times-divisible z 2))

; cdr is the number of times z can be divided by 3
(define (cdr z)
  (times-divisible z 3))


(define z (cons 4 5))
(car z)
4

(cdr z)
5