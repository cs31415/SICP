;Exercise 1.23.  The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n) 
        ((divides? d n) d)
        (else (find-divisor n (next d)))))
(define (divides? d n)
  (= 0 (remainder n d)))
(define (square x) (* x x))
;(define (next x) 
;  (if (= x 2) 
;      3
;      (+ x 2)))
;(define (next x) (+ x 1))
(define (next x) (+ x 2))

;1009
;2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31 = 30
;2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31 = 16

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

; ** dummy calls to exclude the compilation overhead
; from subsequent calls
(timed-prime-test 1007) 
(timed-prime-test 1008) 

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

; A: with (find-divisor n (+ d 1)) ; actually with (define (next x) (+ x 1)) to
; exclude the overhead of procedure call from the comparison
;1009 *** 0.017822265625
;1013 *** 0.01708984375
;1019 *** 0.017822265625
;10007 *** 0.055908203125
;10009 *** 0.050048828125
;10037 *** 0.050048828125
;1000003 *** 0.480224609375
;1000033 *** 0.481201171875
;1000037 *** 0.48193359375

; B: with (find-divisor n (next d))
;1009 *** 0.011962890625
;1013 *** 0.010986328125
;1019 *** 0.010986328125
;10007 *** 0.02880859375
;10009 *** 0.029052734375
;10037 *** 0.02880859375
;1000003 *** 0.278076171875
;1000033 *** 0.27294921875
;1000037 *** 0.272216796875

; B/A
; The runtimes for B are a little over 1/2 the runtimes for A
; 
;1009	0.671232877
;1013	0.642857143
;1019	0.616438356
;10007	0.515283843
;10009	0.580487805
;10037	0.575609756
;1000003	0.579054398
;1000033	0.567224759
;1000037	0.564842958

; C with (define (next x) (+ x 2))
;1009 *** 0.010009765625
;1013 *** 0.010009765625
;1019 *** 0.009033203125
;10007 *** 0.027099609375
;10009 *** 0.030029296875
;10037 *** 0.02685546875
;1000003 *** 0.2470703125
;1000033 *** 0.24609375
;1000037 *** 0.2490234375

; C/A: The numbers are a lot closer to 0.5. This probably means that the small overhead of the call to 
; (if (= x 2) is causing the variance in case B.
;1009	0.561643836
;1013	0.585714286
;1019	0.506849315
;10007	0.484716157
;10009	0.6
;10037	0.536585366
;1000003	0.51448907
;1000033	0.511415525
;1000037	0.516717325

;(define (find-divisor n d c)
;  (cond ((> (square d) n) 
;         (begin
;           (display (string-append " *** " (number->string c) " calls "))
;           n))
;        ((divides? d n) d)
;        (else (find-divisor n (next d) (+ c 1)))))

; Analysis of number of calls to find-divisor for case A
;1009 *** 31 calls  *** 0.031005859375
;1013 *** 31 calls  *** 0.095947265625
;1019 *** 31 calls  *** 0.024169921875
;10007 *** 100 calls  *** 0.129150390625
;10009 *** 100 calls  *** 0.060791015625
;10037 *** 100 calls  *** 0.822021484375
;1000003 *** 1000 calls  *** 0.5419921875
;1000033 *** 1000 calls  *** 0.64794921875
;1000037 *** 1000 calls  *** 0.549072265625


; Analysis of number of calls to find-divisor for case B
;1009 *** 17 calls  *** 0.02197265625
;1013 *** 17 calls  *** 0.08203125
;1019 *** 17 calls  *** 0.02099609375
;10007 *** 51 calls  *** 0.109130859375
;10009 *** 51 calls  *** 0.04296875
;10037 *** 51 calls  *** 0.10791015625
;1000003 *** 501 calls  *** 0.306884765625
;1000033 *** 501 calls  *** 0.381103515625
;1000037 *** 501 calls  *** 0.303955078125
