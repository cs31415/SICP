(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; Note: 
; From 2.47 make-frame definition and 2.48 make-vect definition, 
; A vector's definition assumes it originates from (0,0)
; However, edge1 and edge2 don't originate from (0,0).
; A vector whose origin is not (0,0) can be represented as 
; the difference between it's origin and destination vectors
; Say, a vector originating from (x,y) and ending in (x1,y1)
; can be represented as :
; (sub-vect (make-vect x1 y1) (make-vect x y))
; Thus, assuming frame origin is (x,y) and bottom right is (x1,y1)
; and top left is (x2,y2),
; (xcor-vect edge1) would return (- x1 x)  
; (ycor-vect edge1) would return (- y1 y)
; (xcor-vect edge2) would return (- x2 x)
; (ycor-vect edge2) would return (- y2 y)

;a.  The painter that draws the outline of the designated frame.

(define bottom-left (origin-frame f))
(define bottom-right (add-vect bottom-left (edge1-frame f)))
(define top-left (add-vect bottom-left (edge2-frame f)))
(define top-right (add-vect top-left 
                            (add-vect (edge1-frame f) 
                                      (edge2-frame f))))

(segments->painter 
 (list 
  (make-segment bottom-left bottom-right)
  (make-segment bottom-right top-right)
  (make-segment top-right top-left)
  (make-segment top-left bottom-left)))


;b.  The painter that draws an ``X'' by connecting opposite 
    corners of the frame.

(segments->painter 
 (list 
  (make-segment bottom-left top-right)
  (make-segment bottom-right top-left))

;c.  The painter that draws a diamond shape by connecting the 
;    midpoints of the sides of the frame.

; From exercise 2.48
 (define (make-segment vstart vend)
  (cons vstart vend))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((starting-point (start-segment segment))
        (ending-point   (end-segment segment)))
  (make-vect  (average (xcor-vect starting-point) 
                       (xcor-vect ending-point))
              (average (ycor-vect starting-point) 
                       (ycor-vect ending-point)))))

(define bottom-mid (midpoint-segment 
                    (make-segment bottom-left bottom-right)))
(define right-mid (midpoint-segment 
                   (make-segment bottom-right top-right)))
(define top-mid (midpoint-segment 
                 (make-segment top-right top-left)))
(define left-mid (midpoint-segment 
                  (make-segment top-left bottom-left)))


(segments->painter 
 (list 
  (make-segment bottom-mid right-mid)
  (make-segment right-mid top-mid)
  (make-segment top-mid left-mid)
  (make-segment left-mid bottom-mid)))

; d.  The wave painter.

; We can represent the line segments in the wave image as 
; a series of points starting from the left and moving in an 
; anti-clockwise direction

(segments->painter
 (list (make-segment a b)
       (make-segment b c)
       (make-segment c d)))

; Now, make this generic so that given a list of points(vectors), 
; we can generate the painter:

(define (make-segment a b) (cons a b))
(define (try li first)
  (cond ((null? li) 
         null)
        ((null? (cdr li)) 
         (list (make-segment (car li) first)))
        (else 
         (append 
          (list (make-segment (car li) (cadr li))) 
          (try (cdr li) first)))))

(try li (car li))

(define points (list a b c d e f))
(define (wave frame)
  (segments->painter
   (try points (car points))))
