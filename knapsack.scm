; Hashtable for memoization
(define h (make-hash-table 'equal))
(hash-table-put! h 'key1 'val1)

(hash-table-get h 'key1)

(if 
 (hash-table-get h 'key1 (lambda () #f))
 #t
 #f)


(define (weight item) (car item))
(define (value item) (cdr item))
(define (make-item weight value) (cons weight value))
(define (first-item items) (car items))
(define (rest-items items) (cdr items))
(define (isEmpty? lst) (or (null? lst) (eq? 0 length lst)))
 
(define iterations 0)

(define (knapsack items maxWeight)
  (begin
    (display "knapsack ")
    (display items)
    (display ",")
    (display maxWeight)
    (newline)
    (set! iterations (+ iterations 1))
    (if (or (null? items)
            (null? (first-item items)))
        0
        (if (> (weight (first-item items)) maxWeight)
            (knapsack (rest-items items) maxWeight)
            (max (+ (value (first-item items))
                    (knapsack (rest-items items)
                              (- maxWeight (weight (first-item items)))))
                 (knapsack (rest-items items) maxWeight))))))
  
(define items (list (make-item 20 65)
                    (make-item 8 35)
                    (make-item 60 245)
                    (make-item 55 195)
                    (make-item 40 65)
                    (make-item 70 150)
                    (make-item 85 275)
                    (make-item 25 155)
                    (make-item 30 120)
                    (make-item 65 320)
                    (make-item 75 75)
                    (make-item 95 200)
                    (make-item 50 100)
                    (make-item 40 220)
                    (make-item 10 99)))                  
 
(define maxWeight 130)

(knapsack items maxWeight)
(display "iterations ")
(display iterations)
(newline)