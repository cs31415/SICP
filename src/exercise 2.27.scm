(define (reverse items)
  (define (reverse-iter items prev)
    (if (null? items)
        prev
        (reverse-iter (cdr items) (cons (car items) prev))))
  (reverse-iter items null))

(define (deep-reverse items)
  (define (deep-reverse-iter items prev)
    (if (null? items) 
        prev
        (deep-reverse-iter (cdr items) 
                           (cons ((lambda (x) (if (list? x) 
                                                  (deep-reverse x) 
                                                  x)) 
                                  (car items)) 
                                 prev))))
  (deep-reverse-iter items null))

(define x (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))

(define y (list  (list 1 2) (list 3 4 (list 5 (list 6 7)))))

(deep-reverse y)
;((((7 6) 5) 4 3) (2 1))