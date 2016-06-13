(define (make-accumulator s)
  (lambda (x) 
    (begin
      (set! s (+ s x))
      s
    )))

(define A (make-accumulator 5))
(A 10)
;15
(A 10)
;25