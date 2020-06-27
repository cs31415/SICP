;Exercise 1.22.  Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.
;
;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime (- (runtime) start-time))))
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))
;
;Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of (n), you should expect that testing for primes around 10,000 should take about 10 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the n prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?

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



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;(timed-prime-test 5623)



(define (search-for-primes a b)
  (begin
   (timed-prime-test a)
   (if (< a b)
      (search-for-primes (+ a 1) b))))


;(search-for-primes 1000 1020)
;1009 *** 0.01611328125
;1013 *** 0.01513671875
;1019 *** 0.01611328125

;(search-for-primes 10000 10050)
;10007 *** 0.046875
;10009 *** 0.0458984375
;10037 *** 0.044921875

;(search-for-primes 1000000 1000050)
;1000003 *** 0.4150390625
;1000033 *** 0.537109375
;1000037 *** 0.44921875

;
;t1000 ~ theta(sqrt(1000)) ~ theta(10*sqrt(10))
;t10000 ~ theta(sqrt(10000)) ~ theta(10*10) ~ theta(10*sqrt(10)*sqrt(10)) ~ sqrt(10)*t1000
;t1000000 ~ theta(sqrt(1000000)) ~ theta(1000) ~ theta(10*100) ~ 10*theta(100) ~ 10*t10000

; t1000 = (average 0.01611328125 0.01513671875 0.01611328125) = 0.015787760416666668

; t10000 = (average 0.046875 0.0458984375 0.044921875) = 0.0458984375
; sqrt(10)*t1000 = sqrt(10)*0.015787760416666668 = 0.04992528206972

; t1000000 = (average 0.4150390625 0.537109375 0.44921875) = 0.4671223958333333
; 10*t10000 = 10*0.0458984375 = 0.458984375

(define (average a b c)
  (/ (+ a b c) 3))

; t10000 takes sqrt(10) times as long as t1000, and t1000000 takes 10 times as long as t10000.
; The observational results agree closely with the expected results.
; The results are compatible with the notion that programs on my machine run in time 
; proportional to the number of steps required for the computation.
