;Exercise 1.34.  Suppose we define the procedure
;
;(define (f g)
;  (g 2))
;
;Then we have
;
;(f square)
;4
;
;(f (lambda (z) (* z (+ z 1))))
;6
;
;What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))


; (f f) evaluates to (f (f 2)). (f 2) evaluates to (2 2).
; This is an error since the interpreter will try (and fail) to evaluate a function named 2.

; The actual error is:
(f f)
procedure application: expected procedure, given: 2; arguments were: 2
