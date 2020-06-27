;(cons (list 1 2 3) (list 4 5 6))
;
;(cons 1 (cons 2 (cons 3 null)))
;
;(cons 10 (cons (list 1 2 3) (list 4 5 6)))

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (if (null? x)
      0
      (if (not (pair? x))
          1
          (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(count-leaves x)
;4

(count-leaves ())
;0

(count-leaves (list 1 2 3))
;3

(count-leaves (cons 
               (list 1 2 (cons 3 4)) 
               (list 
                (list 5 6 7 (cons 8 9)) 
                (list 10 11 (cons 12 13)))))
;13