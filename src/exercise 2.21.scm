(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))
;(1 4 9 16)

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4))
;(1 4 9 16)
