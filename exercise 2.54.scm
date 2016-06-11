(define (equal? l1 l2)
  (cond ((not (= (length l1) (length l2))) #f)
        ((null? l1) #t)
        (else 
          (and 
           (if (pair? (car l1))
               (equal? (car l1) (car l2))
               (eq? (car l1) (car l2)))
           (equal? (cdr l1) (cdr l2))))))


(equal? '() '())
;#t

(equal? '(a b c) '(a b c))
;#t

(equal? '(a b c) '(a b d))
;#f

(equal? '(a b c) '(a (b c)))
;#f

(equal? '(aa bb cc) '(aa bb cc))
;#t

(equal? '(this is a list) '(this is a list))
;#t

(equal? '(this is a list) '(this (is a) list))
;#f