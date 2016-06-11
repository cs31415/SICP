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


(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product '(1 2 3) '(2 2 2))
;12

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define m '((1 2 3) (4 5 6) (7 8 9)))
(define v '(2 2 2))
(matrix-*-vector m v)
;(12 30 48)

(define (transpose mat)
  (accumulate-n cons null mat))
(transpose m)
;((1 4 7) (2 5 8) (3 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows) (matrix-*-vector cols rows)) m)))
(define i '((1 0 0) (0 1 0) (0 0 1)))

(matrix-*-matrix m i)
;((1 2 3) (4 5 6) (7 8 9))

(matrix-*-matrix m '((1 1) (1 1) (1 1)))
;((6 6) (15 15) (24 24))