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


; from exercise 2.8
(define (sub-interval x y)
  (add-interval x 
                (make-interval 
                 (* -1 (lower-bound y)) 
                 (* -1 (upper-bound y)))))

(define one (make-interval 1 1))
(define A (make-interval 1.01 1.02))
(define B (make-interval 1.05 1.06))

; Expressions where an uncertain term is not repeated -
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

; Expressions where uncertain term(s) is(are) repeated

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

; 0 and A-A
"A-A"
(sub-interval A A)
;(-0.010000000000000009 . 0.010000000000000009)

; Thus, Eva Lu Ator is right.  Expressions with no repeated
; terms will be more accurate than expressions with repeated terms.


; The reason I think is that algebraic identities such as 
; A/A=1, A-A=0 etc. when applied to uncertain numbers, yield 
; uncertain numbers.  Expressions involving uncertain numbers
; will be more accurate if these identities can be avoided 
; altogether.


