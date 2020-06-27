;Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.'' Rewrite this procedure using Ben's suggestion.
         
; From exercise 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))


; Let's reexamine the mul-interval definition from 2.7
;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))
;

; We are doing 4 multiplications for all intervals 
; an interval can be all positive, all negative or 
; half negative and half positive.
; since we have 2 intervals, the number of combinations
; is 3*3 = 9
; Enumerating the possibilities alongside the product p:
; 1. pos, pos    ; p = (p1, p4) = pos  
; 2. pos, neg    ; p = (p2, p3) = neg
; 3. pos, both   ; p = (p2, p4) = both; 
; 4. neg, pos    ; p = (p2, p3) = neg
;     (-4,-2) * (1, 4) = (-4*4, -2*1)
; 5. neg, neg    ; p = (p4, p1) = pos
; 6. neg, both   ; p = (p2, p1) = neg
;     (-4,-2) * (-1, 4) = (-4*4, -4*-1)
; 7. both, pos   ; p = (p2, p4) = both
;     (-1, 4) * (1, 5) = (-1*5, 4*5)
;     (-1, 4) * (0, 5) = (-1*5, 4*5)
; 8. both, neg   ; p = (p2, p1) = neg
;     (-1, 4) * (-5, -1) = (4*-5, -1*-5)
; 9. both, both  ; p = (min p2 p3, p1) = both; 
;     (-1, 4) * (-2, 5) = (min (-1*5, 4*-2), -1*-2)
;     (-1, 4) * (-2, 9) = (min (-1*9, 4*-2), -1*-2)
; in scenario 9, we have to do 4 multiplications

(define (pos? x)
  (or (positive? x) (zero? x)))

(define (neg? x)
  (or (negative? x) (zero? x)))

(define (sign-interval x)
  (cond ((pos? (lower-bound x)) 1)
        ((neg? (upper-bound x)) -1)
        (else 0)))

;(sign-interval (make-interval 1 2))
;(sign-interval (make-interval -1 -2))
;(sign-interval (make-interval -1 2))
;(sign-interval (make-interval 0 2))
;(sign-interval (make-interval -1 0))

(define (mul-interval x y)
  (let  ((sx (sign-interval x))
         (sy (sign-interval y))
         (lx (lower-bound x))
         (ux (upper-bound x))
         (ly (lower-bound y))
         (uy (upper-bound y))
         )
    (cond ((and (= sx 1) (= sy 1)) 
           (make-interval (* lx ly) (* ux uy)))
          ((and (= sx -1) (= sy -1)) 
           (make-interval (* ux uy) (* lx ly)))
          ((and (= sx 1) (= sy -1)) 
           (make-interval (* lx uy) (* ux ly)))
          ((and (= sx -1) (= sy 1)) 
           (make-interval (* ly ux) (* uy lx)))
          ((and (= sx 0) (= sy 1)) 
           (make-interval (* lx uy) (* ux uy)))
          ((and (= sx 1) (= sy 0)) 
           (make-interval (* ly ux) (* uy ux)))
          ((and (= sx 0) (= sy -1)) 
           (make-interval (* ux ly) (* lx ly)))
          ((and (= sx -1) (= sy 0)) 
           (make-interval (* uy lx) (* ly lx)))
          (else 
           (let ((p1 (* lx ly))
                 (p2 (* ux uy))
                 (p3 (* lx uy))
                 (p4 (* ux ly)))
            (make-interval (min p1 p2 p3 p4) 
                           (max p1 p2 p3 p4)))))))

  
  
(mul-interval (make-interval 1 2) (make-interval 3 4))
(mul-interval (make-interval -1 -2) (make-interval -3 -4))
(mul-interval (make-interval -1 -2) (make-interval 3 4))
(mul-interval (make-interval -1 -2) (make-interval -3 4))
(mul-interval (make-interval -1 0) (make-interval -3 -4))
(mul-interval (make-interval 1 2) (make-interval -1 -2))
(mul-interval (make-interval 1 2) (make-interval -2 0))
(mul-interval (make-interval 0 2) (make-interval -2 0))
(mul-interval (make-interval -1 1) (make-interval -2 2))
(mul-interval (make-interval -1 1) (make-interval -2 3))
(mul-interval (make-interval -1 2) (make-interval -2 2))

;(3 . 8)
;(8 . 3)
;(-8 . -3)
;(6 . -8)
;(4 . 0)
;(-1 . -4)
;(0 . -4)
;(0 . -4)
;(-2 . 2)
;(-3 . 3)
;(-4 . 4)
