(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  
       
;(expmod 2 10 7)
;(remainder (square (expmod 2 5 7)) 7)
;(remainder (square (remainder (* 2 (expmod 2 4 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (expmod 2 2 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder (square (expmod 2 1 7)) 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder (square (remainder (* 2 (expmod 2 0 7))  7)) 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder (square (remainder (* 2 1)  7)) 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder (square (remainder 2  7)) 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder (square 2) 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square (remainder 4 7)) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder (square 4) 7)) 7)) 7)
;(remainder (square (remainder (* 2 (remainder 16 7)) 7)) 7)
;(remainder (square (remainder (* 2 2) 7)) 7)
;(remainder (square (remainder 4 7)) 7)
;(remainder (square 4) 7)
;(remainder 16 7)
;(2)

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (square x) (* x x))

;(expmod2 2 10 7)
;(remainder (fast-expt 2 10) 7)
;(remainder (square (fast-expt 2 5)) 7)
;(remainder (square (* 2 (fast-expt 2 4))) 7)
;(remainder (square (* 2 (square (fast-expt 2 2)))) 7)
;(remainder (square (* 2 (square (square (fast-expt 2 1))))) 7)
;(remainder (square (* 2 (square (square (* 2 (fast-expt 2 0)))))) 7)
;(remainder (square (* 2 (square (square (* 2 1))))) 7)
;(remainder (square (* 2 (square (square 2)))) 7)
;(remainder (square (* 2 (square 4))) 7)
;(remainder (square (* 2 16)) 7)
;(remainder (square 32) 7)
;(remainder 1024 7)
;(2)


(define (expmod-tester base exp m)
  (begin
    (let ((start-time (current-inexact-milliseconds)))
      (iter base exp m 1000)
      (newline)
      (display (- (current-inexact-milliseconds) start-time)))))
(define (iter base exp m c)
  (if (> c 0)
      (begin
        (expmod base exp m)
        (iter base exp m (- c 1)))))

(define (expmod2-tester base exp m)
  (begin
    (let ((start-time (current-inexact-milliseconds)))
      (iter2 base exp m 1000)
      (newline)
      (display (- (current-inexact-milliseconds) start-time)))))
(define (iter2 base exp m c)
  (if (> c 0)
      (begin
        (expmod2 base exp m)
        ;(fast-expt base exp)
        (iter2 base exp m (- c 1)))))

(expmod-tester 2 1000 7)
(expmod-tester 2 1000000 7)
(expmod2-tester 2 1000 7)
(expmod2-tester 2 1000000 7)


;8.240966796875
;14.2900390625
;8.430908203125
;410.27392578125


; As we can see, expmod is theta(log10(n)).  However, expmod2, the version that uses fast-expt, looks to be much greater than theta(log10(n))
; It is a mystery at the moment why this is so, since fast-expt is supposed to be theta(log2(n)).
; It may be that since we are dealing with extraordinarily large numbers here, there is some non-linearity in multiplying these numbers.
; This is pure speculation.

; Looking at the process evolution, it looks like expmod2 may be wasting time actually carrying 
; out the full exponentiation(which involves multiplication of larger numbers), and then calculating the remainder, whereas
; expmod is sucessively calculating square of remainders, which are smaller numbers.

;(fast-expt 2 1000)
;10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376

; Here is the answer.  fast-expt computes the exponential and then calculates the remainder.  
; Even for an exponent value of 1000, the resulting exponential value is absurdly large.
; Hence, the astronomical runtimes for expmod2 and the consequent infeasibility of using fast-expt
; for testing primality.


;(fast-expt 2 1000000)
