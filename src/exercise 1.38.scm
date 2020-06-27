;Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.

; using the iterative version of cont-frac from exercise 1.37
(define (cont-frac n d k)
  (define (term n d i result)
    (if (= i 0)
        result
        (term n d (- i 1) (/ (n i) (+ (d i) result)))))
    (term n d k 0))

;d
;1=1    
;2=2    
;3=1   
;4=1
;5=4   d2+2  
;6=1
;7=1
;8=6   d5+2
;...
;
;
;if i-2 is divisible by 3, then its value is 2 * (1 + (x-1)/3)

; tester for the denominator lambda expression
(define (tester f n)
  (define (testf i)
    (if (<= i n)    
        (begin
         (display (string-append 
                   "(" 
                   (number->string i) 
                   "," 
                   (number->string (f i)) 
                   ")"))
         (newline)
         (testf (+ i 1)))
        0)) 

  (testf 1))

(tester (lambda (x) 
           (let ((rem (remainder (- x 2) 3)))
           (if (= rem 0) 
               (* 2 (+ 1 (/ (- x 2) 3)))
               1))) 
        20)

; Now, plug in the lambda to cont-frac
(cont-frac (lambda (x) 1) 
           (lambda (x) 
             (let ((rem (remainder (- x 2) 3)))
               (if (= rem 0) 
                   (* 2 (+ 1 (/ (- x 2) 3)))
                   1)))
           10.0)
