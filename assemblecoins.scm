(define (comb coins total)
  (let ((ncombs 0))
    (define (comb-iter coins total solution idx q)
      (let* ((c (list-ref coins idx)) (qc (if (= -1 q) (quotient total c) q)) (r (- total (* qc c))))
        (begin
          (if (zero? r) 
              (begin 
                (set! ncombs (+ 1 ncombs))
                (write (append solution (list qc))) ; print solution if target reached
                (newline))
              ; if target not reached, then move on to next coin, duly adjusting the total
              (if (< idx (- (length coins) 1)) ; making sure index into list is valid 
                  ; call comb for the next coin
                  (comb-iter coins (- total (* qc c)) (append solution (list qc)) (+ 1 idx) -1)))
          ; count down the coin quantity to 0 for alternate possibilities
          (if (> qc 0)  
              ; decrement quantity for current coin and recalculate possibilities
              (comb-iter coins total solution idx (- qc 1))))))
    (comb-iter coins total () 0 -1)
    (newline)
    (display (string-append (number->string ncombs) " combinations."))))

(comb '(50 25 10 5 1) 100)


;(define (comb coins total)
;  (comb-iter coins total () 0 -1))



; 1. start with largest coin c = (car coins)
; 2. qc = total/c
; 3. if remainder=0, print solution
; 4. else repeat 1-3 with (cdr coins)
; 4. decrement qc
; 5. if (and (>= qc 0) (not (null? (cdr coins)))) repeat 1-4 for (cdr coins)


;(2 0 0 0 0)
;
;(1 2 0 0 0)
;(1 1 2 1 0)
;(1 1 2 0 5)
;(1 1 1 3 0)
;(1 1 1 2 5)
;(1 1 1 1 10)
;(1 1 1 0 15)
;(1 1 0 5 0)
;(1 1 0 4 5)
;(1 1 0 3 10)
;(1 1 0 2 15)
;(1 1 0 1 20)
;(1 1 0 0 25)
;(1 0 5 0 0)
;(1 0 4 2 0)
;(1 0 4 1 5)
;(1 0 4 0 10)
;(1 0 3 4 0)
;(1 0 3 3 5)
;(1 0 3 2 10)
;(1 0 3 1 15)
;(1 0 3 0 20)
;(1 0 2 6 0)
;(1 0 2 5 5)
;(1 0 2 4 10)
;(1 0 2 3 15)
;(1 0 2 2 20)
;(1 0 2 1 25)
;(1 0 2 0 30)
;(1 0 1 8 0)
;(1 0 1 7 5)
;(1 0 1 6 10)
;(1 0 1 5 15)
;(1 0 1 4 20)
;(1 0 1 3 25)
;(1 0 1 2 30)
;(1 0 1 1 35)
;(1 0 1 0 40)
;(1 0 0 0 50)
;
;(0 4 0 0 0)
;(0 3 2 1 0)


;(define coins '(2 4 10 20))
;(+ 1 (car coins))
;(cdr coins)
;(cons (+ 1 (car coins)) (cdr coins))
;(apply + (map * denominations coins))
;(list-ref coins 1)
;(list-tail coins 1)

;(define (printcomb coins idx) 
;(begin
;     (display "comb ")
;     (write coins)
;     (display (string-append " " (number->string idx)))
;     (newline)))                                
;
;(define (comb coins idx)
;  (begin
;     (printcomb coins idx)
;     (if (<= idx (length coins))
;         (let ((qi (list-ref coins idx))) ; get the ith quantity (qi)
;            (if (>= qi 0) 
;                (comb coins idx) ; recurse 
;                (comb coins (inc idx))))
;         (write "done")
;         )))
;
;(comb coins 0)
      

;(define (assemblecoins targetTotal denominations coins runningTotal )
;  (begin
;    (display (string-append "assemblecoins " (number->string targetTotal) " "))
;    (write denominations)
;    (display (string-append " "))
;    (write coins)
;    (display (string-append " " (number->string runningTotal)))
;    (newline)
;    (if (>= runningTotal targetTotal)
;      coins
;      (if (>= (length coins) 1)
;       (let ((left (+ 1 (car coins))))
;        (cons left (assemblecoins targetTotal (cdr denominations) (cdr coins) (+ runningTotal (* left (car denominations))))))))))
;
;(assemblecoins 100 denominations coins 0)

