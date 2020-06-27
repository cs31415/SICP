; using the iterative version of cont-frac from exercise 1.37
(define (cont-frac n d k)
  (define (term n d i result)
    (if (= i 0)
        result
        (term n d (- i 1) (/ (n i) (+ (d i) result)))))
    (term n d k 0))


; nr
;1, x
;2, -x^2
;3, -x^2
;4, -x^2
;
;
; dr
;1,1  i+0
;2,3  i+1  i+(i-1) = 2i-1
;3,5  i+2
;4,7  i+3
;5,9  i+4


(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x -1.0))) 
             (lambda(i) (- (* 2.0 i) 1)) k))


(tan-cf 3.1415 20)
-9.265359005829808e-05