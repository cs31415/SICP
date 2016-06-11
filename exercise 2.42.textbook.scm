(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (begin
    (write (string-append "queens " (number->string board-size)))
    (newline)
    (queen-cols board-size)))

; rest-of-queens is a way to place k - 1 queens in the first k - 1 
; columns
; new-row is a proposed row in which to place the queen for the kth 
; column.
; adjoin-position, which adjoins a new row-column position to a set 
; of positions
; empty-board, which represents an empty set of positions
; safe?, which determines for a set of positions, whether the queen
; in the kth column is safe with respect to the others. (Note that we
; need only check whether the new queen is safe -- the other queens 
; are already guaranteed safe with respect to each other.)

; Let us use the following representation for a position on the board
; ((c1 . r1) (c2 . r2) ... (ck . rk))
; So, (ci . ri) are the squares on which queens are placed.

(define (square c r)
  (cons c r))

(define (row square)
  (cdr square))

(define (col square)
  (car square))


; required defines
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define empty-board ())

; returns a list of queen positions
(define (adjoin-position row k rest-of-queens) 
  (append rest-of-queens (list (square k row))))

; safe?
(define (last z)
  (define (try x last)
    (cond ((null? x) last)
          ((null? (cdr x)) (car x))
          (else (try (cdr x) (car x)))))
  (try z null))

(define (intersect square1 square2)
  (let* ((c1 (col square1)) 
         (c2 (col square2)) 
         (r1 (row square1)) 
         (r2 (row square2)) 
         (cdiff (- c1 c2)))
    (or (= r1 r2) (= r1 (- r2 cdiff)) (= r1 (+ r2 cdiff)))))

(define (safe? k positions) 
  (if (or (= k 1) 
          (null? positions) 
          (null? (cdr positions))) #t
      (and (not (intersect (last positions) (car positions))) 
           (safe? k (cdr positions)))))

;(length (queens 0))
;(length (queens 1))
;(length (queens 2))
;(length (queens 3))
;(length (queens 4))
;(length (queens 5))
;(length (queens 6))
;(length (queens 7))
;(length (queens 8))
;(length (queens 9))
;(length (queens 10))
;(length (queens 11))


(queens 8)

;(length (queens 8))
;92

;(queens 8)
;(((1 . 1) (2 . 5) (3 . 8) (4 . 6) (5 . 3) (6 . 7) (7 . 2) (8 . 4))
; ((1 . 1) (2 . 6) (3 . 8) (4 . 3) (5 . 7) (6 . 4) (7 . 2) (8 . 5))
; ((1 . 1) (2 . 7) (3 . 4) (4 . 6) (5 . 8) (6 . 2) (7 . 5) (8 . 3))
; ((1 . 1) (2 . 7) (3 . 5) (4 . 8) (5 . 2) (6 . 4) (7 . 6) (8 . 3))
; ((1 . 2) (2 . 4) (3 . 6) (4 . 8) (5 . 3) (6 . 1) (7 . 7) (8 . 5))
; ((1 . 2) (2 . 5) (3 . 7) (4 . 1) (5 . 3) (6 . 8) (7 . 6) (8 . 4))
; ((1 . 2) (2 . 5) (3 . 7) (4 . 4) (5 . 1) (6 . 8) (7 . 6) (8 . 3))
; ((1 . 2) (2 . 6) (3 . 1) (4 . 7) (5 . 4) (6 . 8) (7 . 3) (8 . 5))
; ((1 . 2) (2 . 6) (3 . 8) (4 . 3) (5 . 1) (6 . 4) (7 . 7) (8 . 5))
; ((1 . 2) (2 . 7) (3 . 3) (4 . 6) (5 . 8) (6 . 5) (7 . 1) (8 . 4))
; ((1 . 2) (2 . 7) (3 . 5) (4 . 8) (5 . 1) (6 . 4) (7 . 6) (8 . 3))
; ((1 . 2) (2 . 8) (3 . 6) (4 . 1) (5 . 3) (6 . 5) (7 . 7) (8 . 4))
; ((1 . 3) (2 . 1) (3 . 7) (4 . 5) (5 . 8) (6 . 2) (7 . 4) (8 . 6))
; ((1 . 3) (2 . 5) (3 . 2) (4 . 8) (5 . 1) (6 . 7) (7 . 4) (8 . 6))
; ((1 . 3) (2 . 5) (3 . 2) (4 . 8) (5 . 6) (6 . 4) (7 . 7) (8 . 1))
; ((1 . 3) (2 . 5) (3 . 7) (4 . 1) (5 . 4) (6 . 2) (7 . 8) (8 . 6))
; ((1 . 3) (2 . 5) (3 . 8) (4 . 4) (5 . 1) (6 . 7) (7 . 2) (8 . 6))
; ((1 . 3) (2 . 6) (3 . 2) (4 . 5) (5 . 8) (6 . 1) (7 . 7) (8 . 4))
; ((1 . 3) (2 . 6) (3 . 2) (4 . 7) (5 . 1) (6 . 4) (7 . 8) (8 . 5))
; ((1 . 3) (2 . 6) (3 . 2) (4 . 7) (5 . 5) (6 . 1) (7 . 8) (8 . 4))
; ((1 . 3) (2 . 6) (3 . 4) (4 . 1) (5 . 8) (6 . 5) (7 . 7) (8 . 2))
; ((1 . 3) (2 . 6) (3 . 4) (4 . 2) (5 . 8) (6 . 5) (7 . 7) (8 . 1))
; ((1 . 3) (2 . 6) (3 . 8) (4 . 1) (5 . 4) (6 . 7) (7 . 5) (8 . 2))
; ((1 . 3) (2 . 6) (3 . 8) (4 . 1) (5 . 5) (6 . 7) (7 . 2) (8 . 4))
; ((1 . 3) (2 . 6) (3 . 8) (4 . 2) (5 . 4) (6 . 1) (7 . 7) (8 . 5))
; ((1 . 3) (2 . 7) (3 . 2) (4 . 8) (5 . 5) (6 . 1) (7 . 4) (8 . 6))
; ((1 . 3) (2 . 7) (3 . 2) (4 . 8) (5 . 6) (6 . 4) (7 . 1) (8 . 5))
; ((1 . 3) (2 . 8) (3 . 4) (4 . 7) (5 . 1) (6 . 6) (7 . 2) (8 . 5))
; ((1 . 4) (2 . 1) (3 . 5) (4 . 8) (5 . 2) (6 . 7) (7 . 3) (8 . 6))
; ((1 . 4) (2 . 1) (3 . 5) (4 . 8) (5 . 6) (6 . 3) (7 . 7) (8 . 2))
; ((1 . 4) (2 . 2) (3 . 5) (4 . 8) (5 . 6) (6 . 1) (7 . 3) (8 . 7))
; ((1 . 4) (2 . 2) (3 . 7) (4 . 3) (5 . 6) (6 . 8) (7 . 1) (8 . 5))
; ((1 . 4) (2 . 2) (3 . 7) (4 . 3) (5 . 6) (6 . 8) (7 . 5) (8 . 1))
; ((1 . 4) (2 . 2) (3 . 7) (4 . 5) (5 . 1) (6 . 8) (7 . 6) (8 . 3))
; ((1 . 4) (2 . 2) (3 . 8) (4 . 5) (5 . 7) (6 . 1) (7 . 3) (8 . 6))
; ((1 . 4) (2 . 2) (3 . 8) (4 . 6) (5 . 1) (6 . 3) (7 . 5) (8 . 7))
; ((1 . 4) (2 . 6) (3 . 1) (4 . 5) (5 . 2) (6 . 8) (7 . 3) (8 . 7))
; ((1 . 4) (2 . 6) (3 . 8) (4 . 2) (5 . 7) (6 . 1) (7 . 3) (8 . 5))
; ((1 . 4) (2 . 6) (3 . 8) (4 . 3) (5 . 1) (6 . 7) (7 . 5) (8 . 2))
; ((1 . 4) (2 . 7) (3 . 1) (4 . 8) (5 . 5) (6 . 2) (7 . 6) (8 . 3))
; ((1 . 4) (2 . 7) (3 . 3) (4 . 8) (5 . 2) (6 . 5) (7 . 1) (8 . 6))
; ((1 . 4) (2 . 7) (3 . 5) (4 . 2) (5 . 6) (6 . 1) (7 . 3) (8 . 8))
; ((1 . 4) (2 . 7) (3 . 5) (4 . 3) (5 . 1) (6 . 6) (7 . 8) (8 . 2))
; ((1 . 4) (2 . 8) (3 . 1) (4 . 3) (5 . 6) (6 . 2) (7 . 7) (8 . 5))
; ((1 . 4) (2 . 8) (3 . 1) (4 . 5) (5 . 7) (6 . 2) (7 . 6) (8 . 3))
; ((1 . 4) (2 . 8) (3 . 5) (4 . 3) (5 . 1) (6 . 7) (7 . 2) (8 . 6))
; ((1 . 5) (2 . 1) (3 . 4) (4 . 6) (5 . 8) (6 . 2) (7 . 7) (8 . 3))
; ((1 . 5) (2 . 1) (3 . 8) (4 . 4) (5 . 2) (6 . 7) (7 . 3) (8 . 6))
; ((1 . 5) (2 . 1) (3 . 8) (4 . 6) (5 . 3) (6 . 7) (7 . 2) (8 . 4))
; ((1 . 5) (2 . 2) (3 . 4) (4 . 6) (5 . 8) (6 . 3) (7 . 1) (8 . 7))
; ((1 . 5) (2 . 2) (3 . 4) (4 . 7) (5 . 3) (6 . 8) (7 . 6) (8 . 1))
; ((1 . 5) (2 . 2) (3 . 6) (4 . 1) (5 . 7) (6 . 4) (7 . 8) (8 . 3))
; ((1 . 5) (2 . 2) (3 . 8) (4 . 1) (5 . 4) (6 . 7) (7 . 3) (8 . 6))
; ((1 . 5) (2 . 3) (3 . 1) (4 . 6) (5 . 8) (6 . 2) (7 . 4) (8 . 7))
; ((1 . 5) (2 . 3) (3 . 1) (4 . 7) (5 . 2) (6 . 8) (7 . 6) (8 . 4))
; ((1 . 5) (2 . 3) (3 . 8) (4 . 4) (5 . 7) (6 . 1) (7 . 6) (8 . 2))
; ((1 . 5) (2 . 7) (3 . 1) (4 . 3) (5 . 8) (6 . 6) (7 . 4) (8 . 2))
; ((1 . 5) (2 . 7) (3 . 1) (4 . 4) (5 . 2) (6 . 8) (7 . 6) (8 . 3))
; ((1 . 5) (2 . 7) (3 . 2) (4 . 4) (5 . 8) (6 . 1) (7 . 3) (8 . 6))
; ((1 . 5) (2 . 7) (3 . 2) (4 . 6) (5 . 3) (6 . 1) (7 . 4) (8 . 8))
; ((1 . 5) (2 . 7) (3 . 2) (4 . 6) (5 . 3) (6 . 1) (7 . 8) (8 . 4))
; ((1 . 5) (2 . 7) (3 . 4) (4 . 1) (5 . 3) (6 . 8) (7 . 6) (8 . 2))
; ((1 . 5) (2 . 8) (3 . 4) (4 . 1) (5 . 3) (6 . 6) (7 . 2) (8 . 7))
; ((1 . 5) (2 . 8) (3 . 4) (4 . 1) (5 . 7) (6 . 2) (7 . 6) (8 . 3))
; ((1 . 6) (2 . 1) (3 . 5) (4 . 2) (5 . 8) (6 . 3) (7 . 7) (8 . 4))
; ((1 . 6) (2 . 2) (3 . 7) (4 . 1) (5 . 3) (6 . 5) (7 . 8) (8 . 4))
; ((1 . 6) (2 . 2) (3 . 7) (4 . 1) (5 . 4) (6 . 8) (7 . 5) (8 . 3))
; ((1 . 6) (2 . 3) (3 . 1) (4 . 7) (5 . 5) (6 . 8) (7 . 2) (8 . 4))
; ((1 . 6) (2 . 3) (3 . 1) (4 . 8) (5 . 4) (6 . 2) (7 . 7) (8 . 5))
; ((1 . 6) (2 . 3) (3 . 1) (4 . 8) (5 . 5) (6 . 2) (7 . 4) (8 . 7))
; ((1 . 6) (2 . 3) (3 . 5) (4 . 7) (5 . 1) (6 . 4) (7 . 2) (8 . 8))
; ((1 . 6) (2 . 3) (3 . 5) (4 . 8) (5 . 1) (6 . 4) (7 . 2) (8 . 7))
; ((1 . 6) (2 . 3) (3 . 7) (4 . 2) (5 . 4) (6 . 8) (7 . 1) (8 . 5))
; ((1 . 6) (2 . 3) (3 . 7) (4 . 2) (5 . 8) (6 . 5) (7 . 1) (8 . 4))
; ((1 . 6) (2 . 3) (3 . 7) (4 . 4) (5 . 1) (6 . 8) (7 . 2) (8 . 5))
; ((1 . 6) (2 . 4) (3 . 1) (4 . 5) (5 . 8) (6 . 2) (7 . 7) (8 . 3))
; ((1 . 6) (2 . 4) (3 . 2) (4 . 8) (5 . 5) (6 . 7) (7 . 1) (8 . 3))
; ((1 . 6) (2 . 4) (3 . 7) (4 . 1) (5 . 3) (6 . 5) (7 . 2) (8 . 8))
; ((1 . 6) (2 . 4) (3 . 7) (4 . 1) (5 . 8) (6 . 2) (7 . 5) (8 . 3))
; ((1 . 6) (2 . 8) (3 . 2) (4 . 4) (5 . 1) (6 . 7) (7 . 5) (8 . 3))
; ((1 . 7) (2 . 1) (3 . 3) (4 . 8) (5 . 6) (6 . 4) (7 . 2) (8 . 5))
; ((1 . 7) (2 . 2) (3 . 4) (4 . 1) (5 . 8) (6 . 5) (7 . 3) (8 . 6))
; ((1 . 7) (2 . 2) (3 . 6) (4 . 3) (5 . 1) (6 . 4) (7 . 8) (8 . 5))
; ((1 . 7) (2 . 3) (3 . 1) (4 . 6) (5 . 8) (6 . 5) (7 . 2) (8 . 4))
; ((1 . 7) (2 . 3) (3 . 8) (4 . 2) (5 . 5) (6 . 1) (7 . 6) (8 . 4))
; ((1 . 7) (2 . 4) (3 . 2) (4 . 5) (5 . 8) (6 . 1) (7 . 3) (8 . 6))
; ((1 . 7) (2 . 4) (3 . 2) (4 . 8) (5 . 6) (6 . 1) (7 . 3) (8 . 5))
; ((1 . 7) (2 . 5) (3 . 3) (4 . 1) (5 . 6) (6 . 8) (7 . 2) (8 . 4))
; ((1 . 8) (2 . 2) (3 . 4) (4 . 1) (5 . 7) (6 . 5) (7 . 3) (8 . 6))
; ((1 . 8) (2 . 2) (3 . 5) (4 . 3) (5 . 1) (6 . 7) (7 . 4) (8 . 6))
; ((1 . 8) (2 . 3) (3 . 1) (4 . 6) (5 . 2) (6 . 5) (7 . 7) (8 . 4))
; ((1 . 8) (2 . 4) (3 . 1) (4 . 3) (5 . 6) (6 . 2) (7 . 7) (8 . 5)))


;"(safe? 2 (list (square 1 1) (square 3 4)))"
;(safe? 2 (list (square 1 1) (square 3 4)))
;
;"(safe? 2 (list (square 1 1) (square 3 3)))"
;(safe? 2 (list (square 1 1) (square 3 3)))
;
;"(safe? 2 (list (square 1 1) (square 3 4)))"
;(safe? 2 (list (square 1 1) (square 3 4)))
;
;"(safe? 2 (list (square 1 1) (square 2 1) (square 3 3)))"
;(safe? 3 (list (square 1 1) (square 2 4) (square 3 3)))
;
;"(safe? 3 (list (1 . 1) (2 . 3) (3 . 2)))"
;(safe? 3 (list (square 1 1) (square 2 3) (square 3 2)))

;"(intersect (square 1 1) (square 2 1))"
;(intersect (square 1 1) (square 2 1))

;"(intersect (square 1 1) (square 2 2))"
;(intersect (square 1 1) (square 2 2))

;"(intersect (square 1 1) (square 2 3))"
;(intersect (square 1 1) (square 2 3))

;"(intersect (square 1 1) (square 2 4))"
;(intersect (square 1 1) (square 2 4))

;"(intersect (square 1 1) (square 3 4))"
;(intersect (square 1 1) (square 3 4))


