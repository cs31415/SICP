; Naive implementation:
(define y 1)
(define (f x)
  (begin
       (set! y (* x y))
       y))

(+ (f 0) (f 1))
;0

(+ (f 1) (f 0))
;1

; Alternative implementation:
; f is defined to return a closure that captures it's local
; environment (containing val).
; Observe that f's body is evaluated only once which is why
; we observe the side-effects introduced by assignment.
(define f
  (let ((val 1))
    (display "val initialized to ")
    (display val)
    (newline)
    (lambda (x) 
      (set! val (* val x))
      val)))
    

(+ (f 0) (f 1))
;0
(+ (f 1) (f 0))
;1
