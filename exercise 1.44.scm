(define dx 0.1)
(define (average a b c) (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (average (f x) (f (- x dx)) (f (+ x dx)))))

; n-fold smoothed function

; Copying from exercise 1.43

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f
      ))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

(sin 2)
((smooth sin) 2)
((n-fold-smooth sin 100) 2)
0.9092974268256817
0.9062689603873233
0.1409420416690028

(sin 3)
((smooth sin) 3)
((n-fold-smooth sin 1) 3)
((n-fold-smooth sin 100) 3)
0.1411200080598672
0.14064999990238003
0.08307821309221351

;(sin 4)
;((smooth sin) 4)
;((n-fold-smooth sin 100) 4)
-0.7568024953079282
-0.7542819218521041
-0.13969630562769877
