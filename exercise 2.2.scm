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

(define (midpoint-segment segment)
  (let ((starting-point (start-segment segment))
        (ending-point   (end-segment segment)))
  (make-point (average (x-point starting-point) 
                       (x-point ending-point))
              (average (y-point starting-point) 
                       (y-point ending-point)))))

(define (average a b) (/ (+ a b) 2))

(define midpoint (midpoint-segment (make-segment 
                                    (make-point 1 1) 
                                    (make-point 2 2))))
(print-point midpoint)
(3/2,3/2)

