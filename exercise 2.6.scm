(define zero (lambda (f) (lambda (x) x)))

; zero is defined as a procedure that takes any procedure f as 
; input  and returns a procedure that returns it's own input 
; (i.e. an identity function)

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; add-1 takes a number n (a procedural representation of a 
; number n) and returns a procedure that takes a procedure f and
; returns a procedure that takes x and returns (f ((n f) x)).
; Since my head is about to explode, let's try the experimental 
; approach and see what we get when we call zero and add-1.

(define (square x) (* x x))
(define (cube x) (* x x x))

((zero square) 4)
4
((zero cube) 4)
4

; Whatever you pass to zero, it always returns the identity 
; function i.e. zero is a procedure that ignores the function 
; passed it and returns it's input as-is, or in other words, 
; applies a function zero times.

(define one (add-1 zero))

((one square) 4)
16
; one is a procedure that applies a function once

(define two (add-1 one))
((two square) 4)
256
; two is a procedure that applies a function twice

; We can use the repeated procedure from exercise 1.43 to 
; express this
(define (compose f g)
  (lambda (x) (f (g x))))
 
(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f
      ))


(define (one f) (repeated f 1))

((one square) 4)
16

(define (two f) (repeated f 2))

((two square) 4)
256

; This implementation is so compact, it almost feels like
; cheating.
; Thus, Church expresses numerals (from the set of whole 
; numbers) as the number of times a procedure is applied to a given
; input! A fantastic scheme indeed!


; In retrospect, looking at the definition of add-1 above should 
; have provided an instant clue as to Church's scheme.  
; Adding 1 n times results in a single application of f to 
; ((n f) x).
; i.e. (n f) is how Church expresses a number n as a procedure 
; that applies f n times.  
; Adding 1 to this results in one more application of f.


(define (+ a b)
  (lambda (x) (a (b x))))

(((+ zero one) square) 4)
4

(((+ one one) square) 4)
16

(((+ one two) square) 4)
256