(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 1)

(((double (double double)) inc) 5)

; Let's try to calculate theoretically before running it.
; (double (double double)) is the transform being applied
; (double double) will apply inc 2*2=4 times
; (double (double double)) will apply inc 2*4=8 times
; Thus, the expression should yield 5+8=13

; But it yields 21.  
; Let's reanalyze.
; double applies f twice.
; (double double) applies double twice, thereby applies 
; f four times.
; (double (double double)) applies f four times twice.  
; Let g = f applied 4 times
; then, (double g) = (g (g x)), i.e. (g (f applied 4 times))
; = ((f applied 4 times) applied 4 times)
; = f applied 16 times!
; Thus, inc applied 16 times to 5 yields 16+5=21



