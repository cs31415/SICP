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

(fold-right / 1 (list 1 2 3))
;(/ 1 (accumulate / 1 '(2 3)))
;(/ 1 (/ 2 (accumulate / 1 '(3))))
;(/ 1 (/ 2 (/ 3 (accumulate / 1 null))))
;(/ 1 (/ 2 (/ 3 1)))
;(/ 1 (/ 2 3))
;(/ 1 (0.666))
;1.5
;3/1/2/1=1.5

(fold-left / 1 (list 1 2 3))
;(iter 1 '(1 2 3))
;(iter (/ 1 1) '(2 3))
;(iter 1 '(2 3))
;(iter (/ 1 2) '(3))
;(iter 0.5 '(3))
;(iter (/ 0.5 3) null)
;(iter 0.1666 null)
;0.1666
;1/1/2/3=0.166

(fold-right list null (list 1 2 3))
;(1 (2 (3 ())))

(fold-left list null (list 1 2 3))
;(((() 1) 2) 3)

; What happens if list operator is substituted by cons?
(fold-right cons null (list 1 2 3))
;(cons 1 (cons 2 (cons 3 null)))
;(list 1 2 3)
;(1 2 3)

(list 3 null)
;is equivalent to
(cons 3 (cons null null))
;both generate:
;(3 ())

; op must satisfy the commutative property if fold-left and  
; fold-right are to generate the same values for a given sequence
(fold-left + 0 (list 1 2 3))
;6
(fold-right + 0 (list 1 2 3))
;6
