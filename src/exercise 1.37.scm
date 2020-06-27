; Continued fractions

; recursive version
(define (cont-frac n d k)
  (define (term n d i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (term n d (+ i 1))))))
  (term n d 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;0.6180555555555556


; iterative version
; The key is to count backwards from k down to 1
(define (cont-fraci n d k)
  (define (term n d i result)
    (if (= i 0)
        result
        (term n d (- i 1) (/ (n i) (+ (d i) result)))))
    (term n d k 0))
  

(cont-fraci (lambda (i) 1.0)
            (lambda (i) 1.0)
            11)
  

