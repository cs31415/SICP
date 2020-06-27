(include "exercise 2.7.scm")

(define (sub-interval x y)
  (add-interval x 
                (make-interval 
                 (* -1 (lower-bound y)) 
                 (* -1 (upper-bound y)))))

;-------------------------------------
;    -3   -2   -1   0   1   2   3          
; 
; If we only consider positive intervals,  there are 
; 3 possibilities:
;: no overlap e.g. (0,1) & (2,3)
; 0-2 = -2, 0-3 = -3, 1-2 = -1, 1-3 = -2
; thus the interval is -3,-1

;: complete overlap (0,3) and (1,2)
; 0-1 = -1, 0-2 = -2, 3-1 = 2, 3-2 = 1
; thus the interval is -2, 2

;: partial overlap (0,2) and (1,3)
; 0-1 = -1, 0-3 = -3, 2-1 = 1, 2-3 = -1
; thus the interval is -3, 1

; For negative intervals:
;: no overlap e.g. (0,-1) & (-2,-3)
; 0--2 = 2, 0--3 = 3, -1--2=1, -1--3=2
; thus the interval is 1,3

;: complete overlap (0,-3) and (-1,-2)
; 0--1=1, 0--2=2, -3--1=-2, -3--2=-1
; thus the interval is -2,2

;: partial overlap (0,-2) and (-1,-3)
; 0--1=1, 0--3=3, -2--1=-1, -2--3=1
; thus the interval is -1,3

; For a mixture of negative and positive intervals:
;: no overlap e.g. (0,1) & (-2,-3)
; 0--2=2, 0--3=3, 1--2=3, 1--3=4
; thus the interval is 2,4

;: partial overlap (0,4) and (-1,3)
; 0--1=1, 0-3=-3, 4--1=5, 4-3=1
; thus the interval is -3,5

; Validate that our sub-interval procedure works as 
; expected
(define r1 (make-interval 0 1))
(define r2 (make-interval 2 3))
(sub-interval r1 r2)
;(-3 . -1)

(define r1 (make-interval 0 3))
(define r2 (make-interval 1 2))
(sub-interval r1 r2)
;(-2 . 2)

(define r1 (make-interval 0 2))
(define r2 (make-interval 1 3))
(sub-interval r1 r2)
;(-3 . 1)

(define r1 (make-interval 0 -1))
(define r2 (make-interval -2 -3))
(sub-interval r1 r2)
;(1 . 3)


(define r1 (make-interval 0 -3))
(define r2 (make-interval -1 -2))
(sub-interval r1 r2)
;(-2 . 2)

(define r1 (make-interval 0 -2))
(define r2 (make-interval -1 -3))
(sub-interval r1 r2)
;(-1 . 3)

(define r1 (make-interval 0 1))
(define r2 (make-interval -2 -3))
(sub-interval r1 r2)
;(2 . 4)

(define r1 (make-interval 0 4))
(define r2 (make-interval -1 3))
(sub-interval r1 r2)
;(-3 . 5)