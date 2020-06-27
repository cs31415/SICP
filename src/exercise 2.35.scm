(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda (x) 1) (enumerate-tree t))))

(define tree (list (list 1 2) (list 3 4 (list 5 (list 6 (list 7 8))))))
(count-leaves tree)
;8



