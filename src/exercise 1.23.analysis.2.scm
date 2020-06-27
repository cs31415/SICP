(define (find-divisor n d)
  (cond ((> (square d) n) n) 
        ((divides? d n) d)
        (else (find-divisor n (next d)))))
(define (divides? d n)
  (= 0 (remainder n d)))
(define (square x) (* x x))
;(define (next x) 
;  (if (= x 2) 
;      3
;      (+ x 2)))
(define (next x) (+ x 1))

; A test version of find-divisor to measure the time taken only for a single 
; divisor value
(define (find-divisor-test n d)
  (cond ((> (square d) n) n) 
        ((divides? d n) d)))

(define (find-divisor-timed-test n d)
   (let ((start-time (current-inexact-milliseconds)))
      (begin
        (find-divisor-test n d)
        (display (string-append " d = " (number->string d)))
        (display " *** total time = ")   
        (display (- (current-inexact-milliseconds) start-time))
        (newline))))

(define (graph-find-divisor n d)
  (if (<= d n)
      (begin
        (find-divisor-timed-test n d)
        (graph-find-divisor n (next d)))))

(define (time-graph-find-divisor n d)
   (let ((start-time (current-inexact-milliseconds)))
      (begin
        (graph-find-divisor n d)
        (display (string-append " d = " (number->string d)))
        (display " --- Total time = ")   
        (display (- (current-inexact-milliseconds) start-time))
        (newline))))
  

;(graph-find-divisor 10000 2)
(time-graph-find-divisor 1000 2)

