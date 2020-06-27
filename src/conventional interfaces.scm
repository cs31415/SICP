(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (square x) (* x x))

(define x (list (list 1 2) (list 3 4)))

(sum-odd-squares x)
;10


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(even-fibs 10)

(map fib (list 0 1 2 3 4 5 6 7 8 9 10))


(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter (lambda (x) (odd? x)) (list 1 2 3 4 5))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(accumulate cons (list 8 9 10) (list 1 2 3 4 5))

(define (square x) (* x x))
(define (sumsquare x y) (+ (square x) (square y)))
(accumulate sumsquare 0 (list 1 2))

(sumsquare 2 0)

;(sumsquare 1 (accumulate sumsquare 0 (list 2)))
;(sumsquare 1 (sumsquare 2 (accumulate sumsquare 0 null)))
;(sumsquare 1 (sumsquare 2 0))
;(sumsquare 1 4)


(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (list? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(define tree (list (list 1 2) (list 3 4 (list 5 6)) (list 7 8)))
(enumerate-tree tree)


(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares tree)

(+ (square 1) (square 3) (square 5) (square 7))



(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib (enumerate-interval 0 n)))))

(even-fibs 12)


(define (list-fib-squares n)
  (accumulate cons 
              null
              (map (lambda (x) (square (fib x))) (enumerate-interval 0 n))
              ))

(list-fib-squares 10)

(map fib (enumerate-interval 0 10))



(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square (filter odd? sequence))))


(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;225



