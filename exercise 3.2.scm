(define (make-monitored f))
  (lambda (x)))

(define s (make-monitored sqrt))

(s 100)
10

(s 'how-many-calls?)
1