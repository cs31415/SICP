The width w of an interval a with lower bound l and 
upper bound u is defined as:
w = (u-1)/2
w1 = (u1 - l1)/2
w2 = (u2 - l2)/2
(add-interval a1 a2) = (l1+l2, u1+u2)
w1+2 
= ((u1+u2) - (l1+l2))/2
= (u1-l1)/2 - (u2-l2)/2
= w1 + w2
Thus, w1+2 is a function of w1 and w2

Similarly, for w1-2, 
(sub-interval a1 a2) 
= (add-interval a1 (make-interval (* -1 (lower-bound a2)) 
                                  (* -1 (upper-bound a2))))
= (add-interval a1 (make-interval (* -1 l2) 
                                  (* -1 u2)))
= (add-interval a1 a3) where a3 = (-u2, -l2)
= w1 + w3

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(mul-interval a1 a2) 
= (min (l1l2, l1u2, u1l2, u1u2), max (l1l2, l1u2, u1l2, u1u2))
= (min (0, 0, 2,3), max (0, 0, 2,3))

if a1 and a2 are positive, 
then (mul-interval a1 a2) = (make-interval l1l2 u1u2)
w12 = (u1u2 - l1l2)/2
This is not f(w1, w2)

for e.g.
a1 = (0,1)
a2 = (2,3)
a12 = (0, 3)
w1 = 1, w2 = 1
w12 = 4

a1 = (1,3)
a1 = (2,6)
a12 = (2,18)
w1 = 2, w2 = 4, w12 = 16

a1 = (-1,3)
a1 = (-2,6)
a12 = (-6,18)
w1 = 4, w2 = 8, w12 = 24

Thus, w12 is not a function of w1,w2


if a1 and a2 are positive, 
then (div-interval a1 a2) = (make-interval (/ 1 l1l2) (/ 1 u1u2))
w12 = ((/ 1 l1l2) - (/ 1 u1u2))/2
= (u1u2 - l1l2)/2l1l2u1u2
This is not f(w1, w2)

a1 = (1,3)
a1 = (2,6)
a1/2 = (1/6, 1/2)
w1 = 2, w2 = 4, w12 = (1/2 - 1/6) = 4/12 = 1/3

Thus, w1/2 is not a function of w1 and w2