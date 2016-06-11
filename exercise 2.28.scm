(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (cond ((null? x) null)
        ((not (list? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(fringe x)
;(1 2 3 4)

(fringe (list x x))
;(1 2 3 4 1 2 3 4)

