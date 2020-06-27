;Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as
; (h/3)*[y0 + 4y1 + 2y2 + 4y3 +....+ 2yn-2 + 4yn-1 + yn]
;
;where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (yk f a k h)
      (f (+ a (* k h))))
    
    (define (coeff k n)
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)))
    
    (define (sum-terms k n sum)
      (begin
        (if (<= k (- n 1))
            (sum-terms (+ k 1) n (+ sum (* (coeff k n) (yk f a k h))))
            sum)))
    
    (* (/ h 3) (sum-terms 0 n 0))))

(define (cube x) (* x x x))

(simpson-integral cube 0 1 100.0)
(simpson-integral cube 0 1 1000.0)

;0.24666666666666673
;0.2496666666666667

; Comparing to the integral shown above:
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(integral cube 0 1 0.01)
;.24998750000000042
(integral cube 0 1 0.001)
;.249999875000001

; dx=0.01 implies dividing the interval [0,1] into 100 sub-intervals.  
; dx=0.001 implies 1000 sub-intervals.
; Although there is no direct correlation between dx from the above
; method and n in Simpson's rule, it does seem like Simpson's rule
; is producing results that are *less* accurate, not more accurate!!  
