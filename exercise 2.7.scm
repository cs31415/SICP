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

; If we define lower-bound and upper-bound positionally 
;(define (lower-bound i)
;  (car i))
;
;(define (upper-bound i)
;  (cdr i))

; This won't work if we're dealing with negative numbers
;(define r3 (make-interval -6.12 7.48))
;(0.1336898395721925 . -0.16339869281045752)
;
;(reciprocal-interval r3)
;
;(upper-bound (reciprocal-interval r3))
;-0.16339869281045752 ; This is wrong!

; Hence, we need to redefine our upper-bound and lower-bound 
; to not be positional

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

(define r3 (make-interval -6.12 7.48))

(reciprocal-interval r3)

(upper-bound (reciprocal-interval r3))
0.1336898395721925

(lower-bound (reciprocal-interval r3))
-0.16339869281045752

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 4.465 4.935))
(add-interval r1 r2)
;(10.585 . 12.415)

(mul-interval r1 r2)
;(27.3258 . 36.9138)

