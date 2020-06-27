;Exercise 1.27.  Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.

;Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In testing primality of very large numbers chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the chance that cosmic radiation will cause the computer to make an error in carrying out a ``correct'' algorithm. Considering an algorithm to be inadequate for the first reason but not for the second illustrates the difference between mathematics and engineering.

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  
(define (fermat-test n)
  (define (try-it a)
    (begin
      ;(display (string-append "try-it " (number->string a)))
      ;(newline)
    (if (< a n) 
        (if (= (expmod a n n) a)
            (try-it (+ a 1))
            #f)
        #t)))

  (try-it 1))

(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)
#t
#t
#t
#t
#t
#t

; Thus, Carmichael numbers really do fool the Fermat test. i.e. they are reported by the theorem as
; prime, but are not in reality prime.

