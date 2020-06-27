(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items null))
(square-list (list 1 2 3 4))
;(16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (cons 
        (iter (cdr things) 
              (cons answer 
                    (square (car things))
                    ))))
  (iter items null))
(square-list (list 1 2 3 4))
;((((() . 1) . 4) . 9) . 16)
  
; Attempt 1 produces a list because we prepend the 1st element to a
; null-terminated pair, which produces a list. The next element is 
; prepended to this list, in turn producing a list, and so on.
  
; Attempt 2 doesn't work because we append the 1st element to a null
; element, which produces a pair. The next element is appended to 
; this pair, in turn producing a pair, and so on.
  
(cons 3 (cons 2 (cons 1 null)))
;(3 2 1)
(cons (cons 1 2) 3)
;((1 . 2) . 3)