;Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

; Copying  over from exercise 2.2
(define (make-segment starting-point ending-point)
  (cons starting-point ending-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


; 1st representation - define rect in terms of it's top left point
; and bottom right point
(define (make-rect top-left-point bottom-right-point)
  (cons top-left-point bottom-right-point))

(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

(define (bottom-left rect)
  (make-point (x-point (top-left rect)) (y-point (bottom-right rect))))

(define (top-right rect)
  (make-point (x-point (bottom-right rect)) (y-point (top-left rect))))

(define (width rect)
  (- (x-point (bottom-right rect)) (x-point (top-left rect))))

(define (height rect)
  (- (y-point (top-left rect)) (y-point (bottom-right rect))))

(define rect (make-rect (make-point 1 5) (make-point 10 1)))

; perimeter and area

(define (perimeter-rect rect)
  (* 2 (+ (width rect) (height rect))))
  
(define (area-rect rect)
  (* (width rect) (height rect)))


(perimeter-rect rect)
26

(area-rect rect)
36


; 2nd representation - define rect in terms of it's bottom left point and 
; width and height

(define (make-rect bottom-left width height)
  (cons bottom-left (cons width height)))

(define (width rect)
  (car (cdr rect)))

(define (height rect)
  (cdr (cdr rect)))

(define rect (make-rect (make-point 1 5) 9 4))

(perimeter-rect rect)
26

(area-rect rect)
36