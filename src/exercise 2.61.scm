;Theta(n)
;average case prop. to n/2
(define (adjoin-set x set)
  (cond ((null? set) (cons x null))
        ((< x (car set)) (cons x set))
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))

(adjoin-set 4 (list 1 2 3 5 6 7))
;(1 2 3 4 5 6 7)
(adjoin-set 4 (list 1 2 3))
;(1 2 3 4)
(adjoin-set 1 (list 2 3 4 5))
;(1 2 3 4 5)
