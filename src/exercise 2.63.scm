; a:
(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;(define tree (make-tree 7 
;                        (make-tree 3 
;                                   (make-tree 1 null null) 
;                                   (make-tree 5 null null)) 
;                        (make-tree 9 
;                                   null 
;                                   (make-tree 11 null null))))
;tree
;(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
;(tree->list-1 tree)
;;(1 3 5 7 9 11)
;(tree->list-2 tree)
;;(1 3 5 7 9 11)
;
;(copy-to-list (3 (1 () ()) (5 () ()))
;              (cons 7
;                    (copy-to-list (9 () (11 () ()))
;                                  result-list)))
;
;(copy-to-list (3 (1 () ()) (5 () ()))
;              (cons 7
;                    (list 9 11)))
;
;(copy-to-list (3 (1 () ()) (5 () ()))
;              (7 . (9 11)))
;
;(copy-to-list (1 () ())
;              (cons 3
;                    (copy-to-list (5 () ())
;                                  (7 . (9 11)))))
;
;(copy-to-list (1 () ())
;              (3 . (5 . (7 . (9 11)))))
;
;(1 . (3 . (5 . (7 . (9 11)))))
;   

(define tree 
  (make-tree 3 
             (make-tree 1 null null) 
             (make-tree 7 
                        (make-tree 5 null null) 
                        (make-tree 9 null 
                                   (make-tree 11 null null))))) 
tree
(tree->list-1 tree)
(tree->list-2 tree)
;(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))
;(1 3 5 7 9 11)
;(1 3 5 7 9 11)


(define tree (make-tree 5 
                        (make-tree 3 (make-tree 1 null null) null) 
                        (make-tree 9 
                                   (make-tree 7 null null) 
                                   (make-tree 11 null null)))) 
tree
(tree->list-1 tree)
(tree->list-2 tree)
;(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
;(1 3 5 7 9 11)
;(1 3 5 7 9 11)

(define tree 
  (make-tree 1 
             (make-tree 2 
                        (make-tree 3 
                                   (make-tree 4 null null) 
                                   (make-tree 7 null null)) 
                        (make-tree 8 
                                   (make-tree 9 null null) 
                                   (make-tree 10 null null))) 
             (make-tree 12 
                        (make-tree 13 
                                   (make-tree 14 null null) 
                                   (make-tree 17 null null)) 
                        (make-tree 18 
                                   (make-tree 19 null null) 
                                   (make-tree 20 null null)))))
(tree->list-1 tree)
(tree->list-2 tree)
;(4 3 7 2 9 8 10 1 14 13 17 12 19 18 20)
;(4 3 7 2 9 8 10 1 14 13 17 12 19 18 20)

; The two procedures produce the same result for the trees in 
; figure 2.16

; b:
; However, tree->list-1 uses append, which is theta(n), 
; whereas tree->list-2 uses cons which is theta(1).
; tree->list-1 includes 2 recursive calls with half the number 
; of items.
; So the number of passes becomes 
; n + (n + n + ... log n times) = n + nlogn = n(1 + logn)
; For large n, this tends to n logn
; Therefore, tree->list-1 is O(n log n)
; tree->list-2 grows more slowly than tree->list-1

