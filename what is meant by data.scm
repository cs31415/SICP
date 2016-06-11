(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument to cons must be 0 or 1"))))
  dispatch)

(define (car z)
  (z 0))

(define (cdr z)
  (z 1))

(car (cons 1 2))
(cdr (cons 1 2))


; Thus, we have defined cons, car and cdr without using any underlying
; data structures, only procedures.
; When we define (cons 1 2), we are defining a procedure that embeds
; the values supplied.
; The line between data and procedures is blurred here.  The pair
; data structures "lives" inside the procedure that cons returns!
; Are procedures and data really different then?
; This is profound.