(define (smallest-divisor n)
  (find-divisor n 2))

; The end test for find-divisor is based on the result that if n is non-prime, 
; it's divisors must be <= sqrt(n)
; Proof using reduction ad absurdum:
; if d is a divisor, then n/d is also a divisor. 
; if the smallest divisor d is greater than sqrt(n), then n/d must be less than 
; sqrt(n). In other words, there exists a divisor smaller than d. This is a 
; contradiction.
; Hence the smallest divisor d can never be greater than sqrt(n).
; n = 36, d = 4. n/d = 9 which is > sqrt(36).

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))

(define (divides? d n)
  (= 0 (remainder n d)))

(define (square x) (* x x))

(smallest-divisor 2711)

; since find-divisor is called at most sqrt(n) times, this algorithm is 
; theta(sqrt(n))

; smallest-divisor can be used as a prime number detector.  If a number's smallest 
; divisor is the number itself, then it must be prime.
(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 27111)