;Exercise 1.21.  Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.
         
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))

(define (divides? d n)
  (= 0 (remainder n d)))

(define (square x) (* x x))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

