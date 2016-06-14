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
  (queen-cols board-size))

; rest-of-queens is a way to place k - 1 queens in the first k - 1 columns
; new-row is a proposed row in which to place the queen for the kth column.
; adjoin-position, which adjoins a new row-column position to a set of positions
; empty-board, which represents an empty set of positions
; safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other.)

; Let us use the following representation for a position on the board
; ((c1 . r1) (c2 . r2) ... (ck . rk))
; So, (ci . ri) are the squares on which queens are placed.

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
  (append rest-of-queens (list (cons k row))))

(define (safe? k positions) 
  ())
    

(define board-size 8)
(define rest-of-queens ())


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

"(queen-cols 0)"
;(queen-cols 0)

"(queen-cols 1)"
;(queen-cols 1)

"(queen-cols 2)"
;(queen-cols 2)

"(queen-cols 3)"
;(queen-cols 3)


;(queens 8)


"flatmap"
(filter
(lambda (positions) (safe? 2 positions))
(flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row 2 rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols 1)))
