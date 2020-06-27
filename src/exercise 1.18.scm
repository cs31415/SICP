;Exercise 1.18.  Using the results of exercises 1.16 and 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.

; ab+i = invariant 

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (multi a b i)
  (begin
    (display (string-append "(multi " (number->string a) " " (number->string b) " " (number->string i) ")"))
    (newline)
    (cond ((= b 0) i)
          ((even? b) (multi (double a) (halve b) i))
          (else (multi (double a) (halve (- b 1)) (+ i a))))))

(multi 3 9 0)