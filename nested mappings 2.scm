; required definitions
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))  
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (combinations s n)
  (cond ((or (null? s) (zero? n)) null)
        ((= n 1) (map (lambda (x) (list x)) s))
        (else
           (append
            (combinations (cdr s) n)
            (map (lambda (x) (append (list (car s)) x))
                 (combinations (cdr s) (- n 1)))))))

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


(filter (lambda (x) (prime? (caddr x))) 
        (map (lambda(x) (append x (list (+ (car x) (cadr x))))) 
             (combinations (enumerate-interval 1 6) 2)))


(accumulate 
 append 
 null
 (map (lambda(i) (map (lambda (j) (list i j)) 
                      (enumerate-interval 1 (- i 1)))) 
      (enumerate-interval 1 6)))


(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(flatmap (lambda(i) (map (lambda (j) (list i j)) 
                      (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 6))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(filter (lambda (x) (prime? (caddr x)))
        (map (lambda(y) (append y (list (+ (car y) (cadr y)))))
             (flatmap (lambda(i) (map (lambda (j) (list i j)) 
                                      (enumerate-interval 1 (- i 1))))
                      (enumerate-interval 1 6))))
        
(flatmap (lambda(i) (map (lambda (j) (list i j)) 
                      (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 3))

(define (permutations s)
  (if (null? s) 
      (list null)
      (flatmap (lambda (x) (map 
                            (lambda (y) (cons x y)) 
                            (permutations (remove x s)))) 
               s)))


(permutations '(1 2 3))


(combinations '(1 2 3) 2)
;((2 3) (1 2) (1 3))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(remove 2 '(1 2 3))

