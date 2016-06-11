;nth root;  y^n = x;  y = x/y^(n-1)
(define num-average-damps 4)
(define (nth-root x n)
  (fixed-point ((repeated average-damp num-average-damps) 
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

; Copying over required procedures from prior exercises
(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f
      ))

; -- 2 average damps --
; 4th root 
(nth-root 16 4)

; 5th root
(nth-root 32 5)

; 6th root
(nth-root 64 6)

; 7th root
(nth-root 128 7)

; -- 3 average damps --
; 8th root
(nth-root 256 8)

; 9th root
(nth-root 512 9)

; 10th root
(nth-root 1024 10)

(nth-root 32768 15)

; -- 4 average damps --
(nth-root 65536 16)

;20th root
(nth-root 1048576 20)

; It seems like the nth root requires log2 n average damps
; Thus, we should be able to compute upto the 31st root 
; using 4 average damps

(nth-root 2147483648 31)

; -- 5 average damps --
;(nth-root 4294967296 32)


; redefining nth-root based on this knowledge
(define log2 
    (lambda (x) 
      (/ (log x) (log 2))))

(define (nth-root2 x n)
  (define num-average-damps (log2 n))
  (fixed-point ((repeated average-damp num-average-damps)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))


(nth-root2 4294967296 32)

