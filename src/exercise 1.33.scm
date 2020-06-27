;Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
;
;a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
;
;b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).


(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (identity a) a)
(define (addone a) (+ a 1))

; test
(filtered-accumulate + even? 0 identity 1 addone 10)
;30
; validate
(+ 2 4 6 8 10)
;30

;a. 
; Copying over prime? from exercise 1.22
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))
(define (divides? d n)
  (= 0 (remainder n d)))
(define (square x) (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))


(filtered-accumulate + prime? 0 identity 1 addone 10)
;18
; validate
(+ 1 2 3 5 7)
;18

;b. 
; Copying over Euclid's algorithm from section 1.25
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relprime? n) 
  (define (relprime2? i) (and (< i n) (= (gcd i n) 1)))
  relprime2?)

; version that uses lambda
;(lambda (n) (lambda (i) (and (< i n) (= (gcd i n) 1))))
;(filtered-accumulate * ((lambda (n) (lambda (i) (and (< i n) (= (gcd i n) 1)))) 10) 1 identity 1 addone 10)

(filtered-accumulate * (relprime? 10) 1 identity 1 addone 10)
;189

;validation
;(gcd 1 10) x
;(gcd 2 10)
;(gcd 3 10) x
;(gcd 4 10)
;(gcd 5 10)
;(gcd 6 10)
;(gcd 7 10) x
;(gcd 8 10)
;(gcd 9 10) x

;3*7*9=189