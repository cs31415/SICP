(define (fibonacci x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci (- x 1))
         (fibonacci (- x 2)))))

;(map fibonacci (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
  
;(fibonacci 37)

; recursion  


(* 3 6)

(/ 24 2)

(/ (* (* (+ 3 1) 18) 17) 3)


(define a 2)
(define b 3)
(* a b)

(define e 9)
(define y 7)
(* e y)

(define (range n)
  (if (= n 1)
      (list 1)
      (append (range (- n 1)) (list n))))

(range 10)

(map fibonacci (range 20))

; multiplication is repeated addition
(define (multiply x y)
  (if (= y 1)
      x
      (+ x (multiply x (- y 1)))))

(multiply 6 4)

;(+ 6 (multiply 6 3))
;(+ 6 (+ 6 (multiply 6 2))
;(+ 6 (+ 6 (+ 6 (multiply 6 1))))
;(+ 6 (+ 6 (+ 6 6)))
;(+ 6 (+ 6 12))
;(+ 6 18)
;24

(define (add l1 l2)
  (let ((f1 (if (pair? l1) (car l1) 0))
        (f2 (if (pair? l2) (car l2) 0))
        (r1 (if (pair? l1) (cdr l1) '()))
        (r2 (if (pair? l2) (cdr l2) '())))
    (if (not (or (pair? l1) (pair? l2)))
        '()
        (append (list (+ f1 f2)) 
                (add r1 r2)))))
  
(add (list 1 2 3 4 ) (list 1 2 3 4 5))

; division is repeated subtraction
; returns (quotient remainder)
(define (divide x y)
  (if (< x y) 
      (list 0 x)
      (add (list 1 0) (divide (- x y) y))))

(divide 3 7)

(divide 24 6)



(define (quotient x y) (car (divide x y)))
(define (remainder x y) (cadr (divide x y)))


(define (deconstruct n)
  (define (place x)
    (cond ((= x 1) 'ones)
          ((= x 10) 'tens)
          ((= x 100) 'hundreds)
          ((= x 1000) 'thousands)
          ((= x 10000) 'tenthousands)
          ((= x 100000) 'hundredthousands)
          ((= x 1000000) 'millions)))
  (define (trydeconstruct n multipleof10)
    (if (< n 10)
        (list (list n (place multipleof10)))
        (append (trydeconstruct (quotient n 10) (* 10 multipleof10)) 
                (list (list (remainder n 10) (place multipleof10))))))
  (trydeconstruct n 1))

(deconstruct 2461357)