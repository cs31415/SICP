(define (scale-tree tree factor)
  (map (lambda (subtree)
         (if (not (pair? subtree)) (* factor subtree)
             (scale-tree subtree factor)))
       tree))

(define (tree-map tree func)
  (cond ((null? tree) null)
        ((not (list? tree)) (func tree))
        (else (list (tree-map (car tree) func) 
                    (tree-map (car (cdr tree)) func)))))

(define (tree-map tree func)
  (map (lambda (subtree)
         (if (not (pair? subtree)) (func subtree)
             (tree-map subtree func)))
       tree))

(define tree (list (list 1 2) (list 3 (list 4 5))))
tree
(tree-map tree (lambda (x) (* x 10)))
(scale-tree tree 10)
