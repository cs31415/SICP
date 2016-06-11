(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f
      ))

(define (square x) (* x x))

((repeated square 2) 5)

((repeated square 1) 5)