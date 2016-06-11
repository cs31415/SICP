(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (next d)))))
(define (divides? d n)
  (= 0 (remainder n d)))
(define (square x) (* x x))
(define (next x) 
  (cond ((= x 2) 3)
        (else (+ x 2))))
 
(define (prime? n)
  (= n (smallest-divisor n)))


(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; a. unique-pairs
(define (unique-pairs n)
  (let ((sequence (enumerate-interval 1 n)))
    (filter (lambda (z) (> (car z) (cadr z)))
            (flatmap (lambda (x) (map (lambda (y) (list x y)) 
                                      (remove x sequence))) 
                     sequence))))
  
(unique-pairs 3)
;((2 1) (3 1) (3 2))

; b. prime-sum-pairs
(define (prime-sum-pairs n)
  (filter (lambda (y) (prime? (caddr y))) 
          (map 
           (lambda (x) (append x (list (+ (car x) (cadr x))))) 
           (unique-pairs 6))))


(prime-sum-pairs 6)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))