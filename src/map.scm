(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) 
            (map proc (cdr items)))))

(define (square x) (* x x))
(map square (list 1 2 3 4 5))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)


