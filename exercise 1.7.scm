(define (average x y)
(/ (+ x y) 2))

(define (improve guess x)
(average guess (/ x guess)))
 
(define (good-enough? guess x)
  (begin
    (display (string-append "(good-enough? " (number->string guess) " " (number->string x) ")"))
    (newline)
    (< (abs (- (square guess) x)) 0.001)))
 
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt2 x)
(sqrt-iter 1.0 x))
 
(define (square x) (* x x))
 
; test
;(sqrt2 9)
;(sqrt2 (+ 100 37))
;(sqrt2 (+ (sqrt2 2) (sqrt2 3)))
;(square (sqrt2 1000))
 
; very small numbers
;(sqrt2 0.0001)
; 0.03230844833048122
; (square 0.0323) is 0.00104383583352
; The acceptance threshold for the square of guess is 0.001.  i.e. the acceptance threshold for guess is (sqrt 0.001) or 0.0316.
; If x is a small value like 0.0001, then (sqrt 0.0001) is 0.01. Thus guess values in the range 0.01 +- 0.0316, i.e. (|0.0216|, |0.0416|)
; can potentially be accepted.  This is quite a large variation as a percentage of x, since x is small. 
; Hence, the output of sqrt2 may not be very accurate for small numbers.

(sqrt 0.0001)
; 0.01
 
; very large numbers
(sqrt 4.2e30) ; 2049390153191919.8
;(sqrt2 4.2e30) ; goes into a loop 
; Large numbers are represented imprecisely with a precision, scale and exponent.  The precision is the number of 
; significant digits to the right of the decimal. The scale is the total number of digits including those before
; the decimal point (e.g. (4,5) implies scale of 9 and precision of 5. Exponent is the number of digits the number has.
; e.g. 4.2e50 indicates the number has 50 digits in all including the most significant digit 4.
; Since the accuracy with which large numbers are represented in the system are limited by the precision of floating
; point arithmetic, there is scope for a large variation in the values of guess as a percentage of x.  


; (improve 2049390153191919.5 4.2e30) output is 2049390153191919.5, same as input, since (square 2049390153191919.5) is rounded to 4.2e30
; due to the imprecise storage of large floating point numbers. This results in an infinite loop.

