(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (begin
    (print "partial-tree ")
    (display elts)
    (print (string-append (number->string n) " "))
    (newline)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (begin
                  (let ((result (cons 
                                 (make-tree 
                                  this-entry 
                                  left-tree 
                                  right-tree)
                                 remaining-elts)))
                    (print (string-append "elts = "))
                    (display elts)
                    (print ", n = ")
                    (display n)
                    (print ", result = ")
                    (display result)
                    (newline)
                    result))))))))))

(list->tree (list 1 3 5 7 9 11))
;(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;"elts = "(3 5 7 9 11)", n = "1", result = "((3 () ()) 5 7 9 11)
;"elts = "(1 3 5 7 9 11)", n = "2", 
;  result = "((1 () (3 () ())) 5 7 9 11)
;"elts = "(7 9 11)", n = "1", result = "((7 () ()) 9 11)
;"elts = "(11)", n = "1", result = "((11 () ()))
;"elts = "(7 9 11)", n = "3", result = "((9 (7 () ()) (11 () ())))
;"elts = "(1 3 5 7 9 11)", n = "6", 
;  result = "((5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))

; a. This is a divide-and-conquer algorithm.
; It takes the middle value of the list, makes that the
; root node, and repeats the process recursively for left 
; and right sub-trees until a single-element list which is 
; returned as a sub-tree with empty left and right sub-trees.
  
; b. Order of growth of list->tree
; number of calls to partial-tree =
; 2 + 4 + 8 + ... n
; 2 + 2.2 + 2.2^2 + ... 2.2^(log n)-1
; Sum of a geometric series 
; a + ar + ar^2 + ... ar^(n-1)
; = a*(1-r^n)/(1-r) 
; Substituting a = 2, r = 2
; Sum = 2*(1-2^(log2 n))/(1-2) = 2*(2^(log2 n) - 1)
; = 2*(n-1)
; Thus, this is a Theta(n) algorithm

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(tree->list (list->tree (list 11 3 5 7 9 1)))