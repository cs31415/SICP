;Exercise 1.28.  One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat's Little Theorem, which states that if n is a prime number and a is any positive integer less than n, then a raised to the (n - 1)st power is congruent to 1 modulo n. To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and raise a to the (n - 1)st power modulo n using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a ``nontrivial square root of 1 modulo n,'' that is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n, computing a^n-1 in this way will reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.

; Miller-Rabin Test:
;    if n is prime, for any random a < n,  a^(n-1) = 1 modulo n
;
; if n=15, a=n-1=14, then (n-1)^2 = 14*14 = 196
; 196 mod 15 = 1.  In this case, n-1 is a trivial square root of 1 modulo n.
; (n-1)^2 = (n-1) * (n-1) = 1 modulo n, since (n-1) = 1 modulo n 
; (dividing n-1 by n yields remainder 1)

; If there exists a non-trivial square root of 1 mod n, then n has to be composite.
; e.g. 4^2 = 16 = 1 mod 15.  Thus 15 is composite, since 4 is a non-trivial 
; square root of 1 mod 15.

; Thus, the Miller-Rabin test can also be evaluated by looking for non-trivial
; square root(s) of 1 modulo n. If such a square root(s) exists, then n is 
; composite.

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m)
         (let* ((x (expmod base (/ exp 2) m)) (r (remainder (square x) m)))
           (if (or (= x 0) (and (= r 1) (not (= x 1)) (not (= x (- m 1))))) 0 r) 
         )
         )
        (else
         (let* ((x (expmod base (- exp 1) m)) (r (remainder (* base x) m)))
            (if (= x 0) 0 r)))))  

(define (miller-rabin-test n)
  (define (try-it a)
     (begin
      (display (string-append "a=" (number->string a) " * ")) 
;      (expmod a (- n 1) n)))
      (= (expmod a (- n 1) n) 1)))
  (try-it (+ 1 (random (- n 1)))))


"prime"
(miller-rabin-test 7)
(miller-rabin-test 13)
(miller-rabin-test 29)
(miller-rabin-test 53)
(miller-rabin-test 31)
(miller-rabin-test 6203)
(miller-rabin-test 4229)
(miller-rabin-test 7349)
(miller-rabin-test 10007)

"non-prime"
(miller-rabin-test 100)
(miller-rabin-test 121)
(miller-rabin-test 27)
(miller-rabin-test 99)
(miller-rabin-test 87)

"carmichael"
(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)


; Thus, it seems that the Miller-Rabin test reliably detects primes, but also
; reports false positives.
