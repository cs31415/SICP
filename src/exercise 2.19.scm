; The textbook implementation:
(define (count-change amount coin-values)
  (cc amount coin-values))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(+ (cc 100 (list 25 10 5 1)) ; all the ways to make 100 without a 50
   (cc 50 (list 50 25 10 5 1))) ; all the ways to make 100 with a 50
     
     


(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(count-change 100 us-coins)
;292
;(count-change 100 uk-coins)
;104561

; Changing the order of coins
(define us-coins (list 1 5 10 25 50))
(count-change 100 us-coins)
;292

; Changing the order didn't alter the result. 
; The algorithm splits the calculation into two parallel
; processing streams. One that calculates the number of 
; combinations without the first coin, the second that 
; calculates the number of combinations that contain the 
; first coin.
; Since we're essentially adding up combinations, order
; doesn't really matter.



; My implementation of the coin counting algorithm already
; took a list of coins.
; It doesn't work for fractional coins though because the 
; quotient function expects integral arguments. 
; So, we redefine quotient to work with fractional divisors

(define (quotient dividend divisor)
  (truncate (/ dividend divisor)))

;(quotient 101 20)
;(quotient 100 0.5)

(define (comb coins total)
  (let ((ncombs 0))
    (define (comb-iter coins total solution idx q)
      (let* ((c (list-ref coins idx)) 
             (qc (if (= -1 q) (quotient total c) q)) 
             (r (- total (* qc c))))
        (begin
          (if (zero? r) 
              (begin 
                (set! ncombs (+ 1 ncombs))
                ; print solution if target reached
                ;(write (append solution (list qc))) 
                ;(newline)
                )
              ; if target not reached, then move on to next coin, 
              ; duly adjusting the total
              ; making sure index into list is valid
              (if (< idx (- (length coins) 1))  
                  ; call comb for the next coin
                  (comb-iter coins 
                             (- total (* qc c)) 
                             (append solution (list qc)) 
                             (+ 1 idx) 
                             -1)))
          ; count down the coin quantity to 0 for alternate 
          ; possibilities
          (if (> qc 0)  
              ; decrement quantity for current coin and recalculate 
              ; possibilities
              (comb-iter coins total solution idx (- qc 1))))))
    (comb-iter coins total () 0 -1)
    (newline)
    (display (string-append (number->string ncombs) 
                            " combinations."))))

(comb us-coins 100)
;292 combinations.
(comb uk-coins 100)
;104561 combinations.