; required definitions
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

; Borrowing unique-pairs definition from exercise 2.40
(define (unique-pairs sequence)
  (filter (lambda (z) (< (car z) (cadr z)))
          (flatmap (lambda (x) (map (lambda (y) (list x y)) 
                                    (remove x sequence))) 
                   sequence)))

; ordered-triples
; This is kind of ugly but it works
(define (ordered-triples n s)
  (let ((sequence (enumerate-interval 1 n)))
    (filter (lambda (z) 
              (let ((a (car z)) (b (cadr z)) (c (caddr z)))
                (and (< a b) 
                     (< b c) 
                     (= s (+ a b c)))))
            (flatmap (lambda (x) 
                       (map (lambda (y) (append (list x) y))
                            (unique-pairs (remove x sequence)))) 
                     sequence))))
  
(ordered-triples 10 8)
;((1 2 5) (1 3 4))

(ordered-triples 10 9)
;((1 2 6) (1 3 5) (2 3 4))

(ordered-triples 10 10)
;((1 2 7) (1 3 6) (1 4 5) (2 3 5))

(ordered-triples 10 11)
;((1 2 8) (1 3 7) (1 4 6) (2 3 6) (2 4 5))
  


