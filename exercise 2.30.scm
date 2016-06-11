(define (square x) (* x x))

; without using higher order procedures
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
            (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))

; using higher order procedures
(define (square-tree tree)
  (map (lambda (subtree)
         (if (not (pair? subtree)) (square subtree)
             (square-tree subtree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
