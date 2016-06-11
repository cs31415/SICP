;Fermat's Little Theorem:
;if n is a prime number and a is a non-negative integer < n,
;   then a^n modulo n = a (a^n is congruent to a, modulo n)
;
;If n is not prime, then the above is not true.
;
;Thus, a^n modulo n = a, is a test of primality.
;
;e.g. n = 7
;a = 2
;2^7 modulo 7 = 128 modulo 7 = 2
;Thus, n is prime
;
;if n = 6
;a = 2
;2^6 modulo 6 = 64 modulo 6 = 4 <> 2
;Thus, n is not prime



;(define (fermat-test n)
;  (= (remainder (expt 2 n) n) 2))

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

(expmod 2 7 7)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 29)


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 100)

; Carmichael numbers fool the Fermat test. i.e. they satisfy the property a^n mod n = a for a<n, yet are not prime.
; There are 255 such numbers below 100,000,000.