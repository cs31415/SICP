(define (last-pair items)
  (if (null? items)
      ()
      (if (null? (cdr items))
          (car items)
          (last-pair (cdr items)))))

(last-pair (list 23 72 149 34))

(last-pair (list 1 3 5 7 9))