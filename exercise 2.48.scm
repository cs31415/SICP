; assumption: vector origin is (0,0)
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
             (+ (ycor-vect v1) (ycor-vect v2))))

  
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v)) 
             (* s (ycor-vect v))))



(define (make-segment vstart vend)
  (cons vstart vend))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
  
; diagonal from bottom left to top right of unit square
(make-segment (make-vect 0 0) (make-vect 1 1))


; A vector whose origin is not (0,0) can be represented as 
; the difference between it's origin and destination vectors
; Say, a vector originating from (x1,y1) and ending in (x2,y2)
; can be represented as :
; (sub-vect (make-vect x2 y2) (make-vect x1 y1))