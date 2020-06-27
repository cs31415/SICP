;Exercise 1.17.  The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:
;
;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))
;
;This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.
;



;a * b = (a + a + .... b times) 
;= (double a) b/2 times if b is even, 
;= a + (double a) (b-1)/2 times if b id odd


; For reference
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (multi a b)
  (begin
    (display (string-append "(multi " (number->string a) " " (number->string b) ")"))
    (newline)
    (cond ((= b 0) 0)
          ((even? b) (multi (double a) (halve b)))
          (else (+ a (multi (double a) (halve (- b 1))))))))

(multi 3 9)