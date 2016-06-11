(include "exercise 2.7.scm")

; This is the solution to the exercise (denominator spans zero)
(define (div-interval x y)
  (if (and (zero? (lower-bound y)) (zero? (upper-bound y)))
      (display "Cannot divide by zero")
      (mul-interval x (reciprocal-interval y))))


; However, it breaks down when the denominator doesn't span zero but 
; is bounded on either end by zero
; Hence, I think the correct solution is to replace and by or
; in the definition above.
(define (div-interval x y)
  (if (or (zero? (lower-bound y)) (zero? (upper-bound y)))
      (display "Cannot divide by zero")
      (mul-interval x (reciprocal-interval y))))


(define r1 (make-interval 1 3))
(define r2 (make-interval 2 6))
(div-interval r1 r2)
;(0.16666666666666666 . 1.5)

(define r1 (make-interval 1 3))
(define r2 (make-interval 0 0))
(div-interval r1 r2)
;Cannot divide by zero

(define r1 (make-interval 1 3))
(define r2 (make-interval 0 6))
(div-interval r1 r2)
;Cannot divide by zero

(define r1 (make-interval 1 3))
(define r2 (make-interval -2 0))
(div-interval r1 r2)
;Cannot divide by zero


(define r1 (make-interval 1 3))
(define r2 (make-interval -2 6))
(div-interval r1 r2)
;(-1.5 . 0.5)