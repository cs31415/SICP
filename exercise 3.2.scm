(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (if (equal? x 'how-many-calls?)
          counter
          (begin
            (set! counter (+ counter 1))
            (f x))))))


(define s (make-monitored sqrt))

(s 100)
;10
(s 'how-many-calls?)
;1

(s 36)
;6
(s 'how-many-calls?)
;2