(define (for-each proc items)
  (if (not (null? items))
      (begin
        (proc (car items))
        (for-each proc (cdr items)))
      #t))
  
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

