(define (gcd a b n)
  (begin
    ;(display (string-append "(gcd " (number->string a) " " (number->string b) ")"))
    ;(newline)
    (if (= 0 b)
      (begin
        (display (string-append (number->string n) " calls"))
        (newline)
        a)
      (gcd b (remainder a b) (+ n 1)))))


;(gcd 8 3)
;(gcd 3 2)
;(gcd 2 1)
;(gcd 1 0)
;
;= 4 calls


;(gcd 16 6) ; 4 calls
;(gcd 32 6) ; 3 calls
;(gcd 64 6) ; 4 calls
;(gcd 128 6) ; 3 calls
;(gcd 256 6 0)

(define (gcd-analysis a b)
  (if (> b 1)
      (begin
        (display (string-append "b = " (number->string b) ", "))
        (gcd a b 0)
        (gcd-analysis a (round (/ b 1.618))))))

(gcd-analysis 5440 2402)

