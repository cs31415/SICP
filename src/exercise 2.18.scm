; We need to construct the following sequence:
;(cons 25 (cons 16 (cons 9 (cons 4 (cons 1 null)))))
 
(define (reverse items)
  (define (reverse-iter items prev)
    (if (null? items)
        prev
        (reverse-iter (cdr items) (cons (car items) prev))))
  (reverse-iter items null))
   
(reverse (list 1 4 9 16 25))
;(25 16 9 4 1)
 
(reverse ())
;()
 
(reverse (reverse (list 1 4 9 16 25)))
;(1 4 9 16 25)