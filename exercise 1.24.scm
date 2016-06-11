;Exercise 1.24.  Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has (log n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?
(define (even? n)
  (= (remainder n 2) 0))

; *** entry point
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (let ((num-calls 0) (out-str ""))
      ; *** procedure to be timed
      (define (fast-prime? n times)
        (define (fermat-test n)
          (define (expmod base exp m)
            (begin
                ;(set! num-calls (+ num-calls 1))
                (cond ((= exp 0) 1)
                    ((even? exp)
                     (remainder (square (expmod base (/ exp 2) m))
                                m))
                    (else
                     (remainder (* base (expmod base (- exp 1) m))
                                m)))))  
          
          
          (define (try-it a)
            (= (expmod a n n) a))
          
            (try-it (+ 1 (random (- n 1)))))
            ;(try-it 2))
        
          
        (cond ((= times 0) true)
              ((fermat-test n) (fast-prime? n (- times 1)))
              (else false)))
      
      (define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time))
      
      (define (square x) (* x x))
      
      (if (fast-prime? n 20)
          (report-prime (- (current-inexact-milliseconds) start-time)))
      (display (string-append " *** " (number->string num-calls) " calls"))
      (display out-str)
      ))
  
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(timed-prime-test 1008) ; ** dummy call to absorb compile overhead

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)


;1008 *** 16 calls
;1009 *** 0.184814453125 *** 340 calls
;1013 *** 0.19384765625 *** 360 calls
;1019 *** 0.202880859375 *** 380 calls
;10007 *** 0.23681640625 *** 440 calls
;10009 *** 0.22705078125 *** 420 calls
;10037 *** 0.23583984375 *** 440 calls
;1000003 *** 0.416015625 *** 580 calls
;1000033 *** 0.4150390625 *** 580 calls
;1000037 *** 0.431884765625 *** 600 calls


; fermat-test has log n growth.  
; Hence, t1000000 ~ 2*t1000
; t1000003/t1009 = 0.416015625/0.1848144531255 = 2.25
; t1000033/t1013 = 0.4150390625/0.19384765625 = 2.14
; t1000037/t1019 = 0.431884765625/0.202880859375 = 2.12
; Thus, the observed ratios are slightly higher than 2.


; Could it be that using a random exponent is causing this variance?
; Let's try using a fixed exponent of 2 and see what we get.

;1008 *** 16 calls
;1009 *** 0.171875 *** 340 calls
;1013 *** 0.179931640625 *** 360 calls
;1019 *** 0.194091796875 *** 380 calls
;10007 *** 0.22216796875 *** 440 calls
;10009 *** 0.215087890625 *** 420 calls
;10037 *** 0.221923828125 *** 440 calls
;1000003 *** 0.366943359375 *** 580 calls
;1000033 *** 0.35888671875 *** 580 calls
;1000037 *** 0.373046875 *** 600 calls

;t1000003/t1009 = 0.366943359375/0.171875 = 2.135
;t1000033/t1013 = 0.35888671875/0.179931640625 = 1.99
;t1000037/t1019 = 0.373046875/0.194091796875 = 1.92

; The ratios seem closer to 2 now.  Possibly using a random exponent is 
; causing some variability in expmod's runtimes.  


; fast-prime? calls fermat-test, which calls expmod and random
; either of which may have runtime that increases as n increases.
; This might explain the slight observed discrepancy.
; 
; Let's try replacing the random base with a fixed base
; Thus, we replace (try-it (+ 1 (random (- n 1)))) with (try-it 2)
; The runtimes are:
;1008 *** 16 calls
;1009 *** 0.177001953125 *** 340 calls
;1013 *** 0.18408203125 *** 360 calls
;1019 *** 0.194091796875 *** 380 calls
;10007 *** 0.22705078125 *** 440 calls
;10009 *** 0.218017578125 *** 420 calls
;10037 *** 0.22509765625 *** 440 calls
;1000003 *** 0.3740234375 *** 580 calls
;1000033 *** 0.363037109375 *** 580 calls
;1000037 *** 0.463134765625 *** 600 calls

; Thus, t1000003/t1009 = 0.374023437/0.177001953125

;(define (random-test n times)
;  (if (> times 0)
;      (begin
;        (+ 1 (random (- n 1)))
;        (random-test n (- times 1)))))
;(define (time-random n times)
;   (let ((start-time (current-inexact-milliseconds)))
;      (begin
;        (random-test n times)
;        (newline)
;        (display (string-append "n = " (number->string n)))
;        (display " *** ")   
;        (display (- (current-inexact-milliseconds) start-time)))))
        

;(time-random 1009 20)
;(time-random 1009 20)
;(time-random 1000003 20)
;n = 1009 *** 0.09912109375
;n = 1000003 *** 0.093017578125
; Nothing here that explains the discrepancy
; Let's move on to expmod

;(define (expmod-test a n)
;    (if (< a n)
;        (begin
;          (expmod a n n)
;          (expmod-test (+ a 1) n n))))
;(define (time-expmod n)
;   (let ((start-time (current-inexact-milliseconds)))
;      (begin
;        (expmod-test n 2)
;        (newline)
;        (display (string-append "n = " (number->string n)))
;        (display " *** ")   
;        (display (- (current-inexact-milliseconds) start-time)))))

  
;(time-expmod 1009)
;(time-expmod 1009)
;(time-expmod 1000003)
;n = 1009 *** 0.0869140625
;n = 1000003 *** 0.0810546875
; Nothing here that indicates that expmod takes longer for larger n

; We have to put it down to 