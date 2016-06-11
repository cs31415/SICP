; Exercise 1.11. A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) +
; 3f(n - 3) if n>= 3. Write a procedure that computes f by means of a recursive process. Write a procedure
; that computes f by means of an iterative process.

; recursive (tree recursive)
(define  (f n)
  (if (< n 3) 
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;f(0) = 0
;f(1) = 1
;f(2) = 2
;f(3) = f(2) + 2*f(1) + 3*f(0) 
;f(4) = f(3) + 2*f(2) + 3*f(1) = (f(2) + 2*f(1) + 3*f(0)) + 2 * f(2) + 3 * f(1)
;f(5) = f(4) + 2*f(3) + 3*f(2) = (f(2) + 2*f(1) + 3*f(0)) + 2 * f(2) + 3 * f(1) + 2 * (f(2) + 2*f(1) + 3*f(0)) + 3 * f(2)
;f(6) = f(5) + 2*f(4) + 3*f(3)

; iterative
(define (fiter n)
  (define (fi n minus1 minus2)
    (begin
      (display (string-append "(fi " (number->string n) " " (number->string minus1) " " (number->string minus2) ")"))
      (newline)
      (if (< n 3)
          n
          (let* ((m3 (fi (- n 3) -1 -1))
                 (m2 (if (>= minus2 0) minus2 (fi (- n 2) m3 -1)))
                 (m1 (if (>= minus1 0) minus1 (fi (- n 1) m2 m3))))
            (+ m1 (* 2 m2)  (* 3 m3))))))
  (fi n -1 -1))

;fi(4) => 
;(fi 4 -1 -1)
;(fi 1 -1 -1)
;(fi 2 1 -1)
;(fi 3 2 1)
;(fi 0 -1 -1)

; Results of prior computations are passed along via the minus1, minus2 parameters avoiding the need to preserve the
; stack frames for those calls;  This is iterative.

(f 4)
(fiter 4)


