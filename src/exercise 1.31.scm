;a.  The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to pi using the formula
;    
;    pi/4 = 2/3.4/3.4/5.6/5.6/7.8/7...

; recursive version
(define (product2 f next a b)
  (if (> a b)
      1
      (* (f a) (product f next (next a) b))))

; iterative version
(define (product f next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))))

  (iter a 1))


(define (identity a) a)
(define (addone a) (+ a 1))

(define (factorial n)
  (product identity addone 1 n))

(factorial 6)


;n=1, 2/3.4/3 = 2.1/dr.(2.1+2)/dr,    dr = 3 = 3+0 = 3 + (1-1)*2
;n=2, 4/5.6/5 = 2.2/dr.(2.2+2)/dr     dr = 5 = 3+2 = 3 + (2-1)*2
;n=3, 6/7.8/7 = 2.3/dr.(2.3+2)/dr     dr = 7 = 3+4 = 3 + (3-1)*2
;n=i, 2.i/dr.(2.i+2)/dr               dr = 3 + (i-1)*2

(define (term i)
  (let ((dr (+ 3 (* (- i 1) 2))))
     (* (/ (* 2 i) dr) (/ (+ (* 2 i) 2) dr))))

(define (picalc n)
  (* 4.0 (product term addone 1 n)))

(picalc 10000)
3.1416711865344635

