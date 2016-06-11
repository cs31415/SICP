(define (good-enough? guess guessprev) 
  (< (/ (abs (- guess guessprev)) guess) 0.001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cubert-iter x guess guessprev)
  (if (good-enough? guess guessprev)
      guess
      (improve guess x)
  ))

(define (cubert x)
  (cubert-iter x 1.0 0))


(cubert 8)