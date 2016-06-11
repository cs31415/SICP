(define (square x) (* x x))
;
;(define (fast-expti b n a)
;  (begin
;    (display (string-append "(fast-expti " (number->string b) " " (number->string n) " " (number->string a) ")"))
;    (newline)
;    (cond ((= n 0) a)
;          ((= n 1) b)
;          ((even? n) (fast-expti (square b) (quotient n 2) (square b)))
;          (else (* b (fast-expti (square b) (quotient (- n 1) 2) (square b)))))))

(define (fast-expti b n a)
  (begin
    (display (string-append "(fast-expti " (number->string b) " " (number->string n) " " (number->string a) ")"))
    (newline)
    (cond ((= n 0) a)
          ((even? n) (fast-expti (square b) (quotient n 2) a))
          (else (fast-expti (square b) (quotient (- n 1) 2) (* a b))))))


;(quotient 1 2)
  
(fast-expti 2 10 1)
;
;b^n = (b^n/2)^2 = (b^2)^n/2
;Invariant = ab^n
;
;b^12 
;= (b^2)^6 
;= (b^4)^3
;= b^4 * (b^4)^2
;
;b^8
;= (b^2)^4
;= (b^4)^2

