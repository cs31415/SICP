;Exercise 1.25.  Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have simply written
;
;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))
;
;Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (let ((num-calls 0) (out-str ""))
      ; *** procedure to be timed
      (define (fast-prime? n times)
        (define (fermat-test n)
          (define (expmod base exp m)
            (remainder (fast-expt base exp) m))
          (define (fast-expt b n)
            (cond ((= n 0) 1)
                  ((even? n) (square (fast-expt b (/ n 2))))
                  (else (* b (fast-expt b (- n 1))))))
          (define (even? n)
            (= (remainder n 2) 0))
          (define (square x) (* x x))
          
;          (define (expmod base exp m)
;            (begin
;                (set! num-calls (+ num-calls 1))
;                (cond ((= exp 0) 1)
;                    ((even? exp)
;                     (remainder (square (expmod base (/ exp 2) m))
;                                m))
;                    (else
;                     (remainder (* base (expmod base (- exp 1) m))
;                                m)))))  
          
          
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

;1008 *** 0 calls
;1009 *** 0.19091796875 *** 0 calls
;1013 *** 0.2021484375 *** 0 calls
;1019 *** 0.218017578125 *** 0 calls
;10007 *** 0.375 *** 0 calls
;10009 *** 0.35205078125 *** 0 calls
;10037 *** 0.5869140625 *** 0 calls
;1000003 *** 13.808837890625 *** 0 calls
;1000033 *** 116.537841796875 *** 0 calls
;1000037 *** 12.490966796875 *** 0 calls

; This version of expmod is taking an order of magnitude longer