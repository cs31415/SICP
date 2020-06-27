(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append (enumerate-tree y) 
                         (list x))) 
   null 
   sequence))
(reverse '(1 2 3))
;(3 2 1)
  

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))
(reverse '(1 2 3))
;(iter (cons 1 null) '(2 3))
;(iter (cons 1 null) '(2 3))
;(iter (cons 2 (cons 1 null)) '(3))
;(iter (cons 3 (cons 2 (cons 1 null))) null)
;(cons 3 (cons 2 (cons 1 null)))
;(3 2 1)
