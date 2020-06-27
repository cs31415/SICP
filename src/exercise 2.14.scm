; including definitions from exercise 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (reciprocal-interval y)
  (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))

(define (div-interval x y)
  (mul-interval x (reciprocal-interval y)))

(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))


; defining intervals for parallel resistor calc
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define one (make-interval 1 1))
(define r1 (make-interval 1.1 1.2))
(define r2 (make-interval 4.1 4.2))

"par1"
(par1 r1 r2)
;(0.8351851851851851 . 0.9692307692307695)

"par2"
(par2 r1 r2)
;(0.8673076923076922 . 0.9333333333333333)

; Lem is right. Seemingly equivalent algebraic expressions are
; yielding slightly different results:
;(4.51 5.04)/(5.2 5.4) = (0.8351851851851851 . 0.9692307692307695)
;(1 1)/(1.0714285714285714 . 1.1529933481152994) 
; = (0.8673076923076922 . 0.9333333333333333)

; If we set the interval width to 0 and recalculate, 
; par1 and par2 are identical. 
;"par1"
;(0.8 . 0.8)
;"par2"
;(0.8 . 0.8)
; Thus, it seems like the differences in values between
; different algebraic expressions of the same quantity are due to 
; non-zero interval width

; Reducing the interval width by a factor of 10, the variance
; between par1 and par2 also reduces by a factor of 10
;(define r1 (make-interval 1.01 1.02))
;(define r2 (make-interval 4.01 4.02))
;"par1"
;(0.8035912698412699 . 0.816812749003984)
;"par2"
;(0.8067928286852589 . 0.8135714285714285)



(define A (make-interval 1.01 1.02))
(define B (make-interval 1.05 1.06))

; Equivalent algebraic expressions to test -
"A"
A
;(1.01 . 1.02)
"1/(1/A)"
(div-interval one (div-interval one A))
;(1.01 . 1.02)

; A/B and 1/(B/A)
"A/B"
(div-interval A B)
;(0.9528301886792452 . 0.9714285714285714)
"1/(B/A)"
(div-interval one (div-interval B A))
;(0.9528301886792453 . 0.9714285714285715)

; A + B and A*(1 + B/A)
"A+B"
(add-interval A B)
;(2.06 . 2.08)
"A*(1 + B/A)"
(mul-interval A (add-interval one (div-interval B A)))
;(2.0497058823529413 . 2.0904950495049506)

; AB and A*(1/B)
"AB"
(mul-interval A B)
;(1.0605 . 1.0812000000000002)
"A*(1/B)"
(mul-interval A (div-interval one B))
;(0.9528301886792452 . 0.9714285714285714)

; 1 and A/A
"1"
one
"A/A"
(div-interval A A)
;(0.9901960784313725 . 1.00990099009901)

"A*(1/A)"
(mul-interval A (div-interval one A))
;(0.9901960784313725 . 1.00990099009901)

; Dividing an interval with non-zero width by itself doesn't yield
; the interval (1 1)!
; Aha! This is possibly why the rules of algebra don't apply to
; intervals exactly.


; Examine the results of the computation (which computation?) in 
; center percent form
; Copying the relevant definitions from exercise 2.12
(define (make-center-percent c tolerancePct)
  (let ((w (* tolerancePct c 0.01)))
    (make-interval (- c w) (+ c w))))

(define A (make-center-percent 1 1))
(define B (make-center-percent 1 1))
"A*(1/A)"
(mul-interval A (div-interval one A))
;(0.9801980198019802 . 1.0202020202020203); at 1%
;(0.8181818181818181 . 1.2222222222222223); at 10%
;(0.6666666666666667 . 1.5); at 20%
; Thus, the variance between 1 and A/A or A(1/A) is 
; proportional to ~2*tolerance pct

"AB"
(mul-interval A B)
;(0.9801 . 1.0201)   ; at 1%
;(0.81 . 1.2100000000000002) ; at 10%
;(0.25 . 2.25); at 50%
;(0.00010000000000000018 . 3.9601) ; at 99%
"A*(1/B)"
(mul-interval A (div-interval one B))
;(0.9801980198019802 . 1.0202020202020203) ; at 1%
;(0.8181818181818181 . 1.2222222222222223) ; at 10%
;(0.3333333333333333 . 3.0) ; at 50%
;(0.005025125628140708 . 198.99999999999983); at 99%
