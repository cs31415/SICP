(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (try-decode bits branch tree)
      (cond ((null? branch) '())
            ((leaf? branch) 
             (append (symbols branch) (try-decode bits tree tree)))
            ((null? bits) '())
            ((= 0 (car bits)) 
             (try-decode (cdr bits) (left-branch branch) tree))
            ((= 1 (car bits)) 
             (try-decode (cdr bits) (right-branch branch) tree))
            (else (display "Bad input string "))))
    (try-decode bits tree tree))

  
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;(A D A B B C A)
