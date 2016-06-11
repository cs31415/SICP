(define (divides? d n)
  (start-divides-test d n (current-inexact-milliseconds)))

(define (start-divides-test d n start-time)
  (begin
    (= 0 (remainder n d))))
(define (square x) (* x x))
(define (next x) 
  (if (= x 2) 
      3
      (+ x 2)))
;(define (next x) (+ x 1))

(define (test-divides d n)
      (if (< d n)
          (begin
            (divides? d n)
            (test-divides (next d) n))))

(define (time-divides d n)
   (let ((start-time (current-inexact-milliseconds)))
      (begin
        (test-divides d n)
        (display (string-append " n = " (number->string n)))
        (display " *** total time = ")   
        (display (- (current-inexact-milliseconds) start-time))
        (newline))))

(define (graph-divides n)
  (if (> n 2)
      (begin
        (time-divides 2 n)
        (graph-divides (- n 100)))))

(graph-divides 10000)

