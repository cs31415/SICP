; A> My solution to the problem (textbook solution follows):
;
; Possible approaches to Queens problem:
; 1. Draw an nxn matrix.  Each column and row must have one 1 and 
;    n-1 0's.
;    However, this is not a sufficient condition.  A matrix with 1's 
;    in it's diagonal would satisfy it, but not be a viable solution.

; 2. Maintain a list of the squares that are under capture.
;    Check against this list when placing the next piece.
;    Alternatively maintain a list of non-captured squares and
;    pick from this list.
; 
; for col c -- itercol
;   get list of free squares 
;     for each free square fs -- iterposs
;        place queen qc in fs -- add to tSolution list
;            recurse for col c+1..n 
;              if free square found for nth col, add to list of 
;                solutions
; return list of solutions            

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


; returns list of captured rows for each column
(define (square c r)
  (cons c r))

(define (row square)
  (cdr square))

(define (col square)
  (car square))

; projection of a square sq onto a column c on a nxn board
(define (project-col sq c n)
  (cond 
    ((null? sq) ())
    ((= (col sq) c) (enumerate-interval 1 n))
    (else     
      (let* ((rs (row sq)) 
             (cs (col sq))
             (cdiff (- c cs))
             (pr1 (+ rs cdiff))
             (pr2 (- rs cdiff)))
        (filter (lambda (x) (and (> x 0) (<= x n)))
                (list (min pr1 pr2) rs (max pr1 pr2)))))))


"(project-col (square 1 3) 8 1)"
(project-col (square 1 3) 1 8)
;(1 2 3 4 5 6 7 8)

(project-col (square 3 2) 8 8)
;(2 7)

; squares captured by a queen sitting on square sq on an nxn board 
(define (captured-squares sq n)
  (accumulate append () 
              (map (lambda (x) (list (project-col sq x n))) 
                   (enumerate-interval 1 n))))

"(captured-squares (square 1 3) 8)"
(captured-squares (square 1 3) 8)
;((1 2 3 4 5 6 7 8) (2 3 4) (1 3 5) (3 6) (3 7) (3 8) (3) (3))

(captured-squares (square 3 2) 8)
;((2 4) (1 2 3) (1 2 3 4 5 6 7 8) (1 2 3) (2 4) (2 5) (2 6) (2 7))

(captured-squares (square 1 1) 8)
;((1 2 3 4 5 6 7 8) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8))

; removelist removes a list of values from another list 
(define (removelist src-list from-list)
  (if (null? src-list)
      from-list
      (removelist (cdr src-list) (remove (car src-list) from-list))))

;(removelist (list 1 6) (enumerate-interval 1 8))

(define (inverse-project-col sq c n)
  (removelist (project-col sq c n) (enumerate-interval 1 n)))

"inverse-project-col"
(inverse-project-col () 1 8)

(define (free-squares-one-queen queen-square n)
  (accumulate append () 
              (map 
               (lambda (x) 
                 (list (inverse-project-col queen-square x n))) 
               (enumerate-interval 1 n))))

"(free-squares-one-queen (square 1 3) 8)"  
(free-squares-one-queen (square 1 3) 8)
"(free-squares-one-queen () 8)"
(free-squares-one-queen () 8)


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (intersection-sets list-of-sets)
  (define (try lsets iset first-call)
    (if (null? lsets) 
        iset
        (try (cdr lsets)
             (if first-call 
                 (car lsets)
                 (intersection-set iset (car lsets)))
             #f)))
  (try list-of-sets () #t))


"(intersection-set '(1 3 4 5 6 7) '(2 4 5 6 7 8) '(2 4 5 6))"
(intersection-sets (list '(1 3 4 5 6 7) '(2 4 5 6 7 8) '(2 4 5 6)))
(intersection-sets (list () (list 1 2 3 4 5)))
(intersection-sets (list (list 1 2 3 4 5) ()))
(intersection-sets (list (list 1 2 3 4 5) () (list 1 2 3 6 7 8)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))  
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons null mat))

(transpose (list '(1 2 3) '(4 5 6) '(7 8 9)))

(define (free-squares-multiple-queens queen-squares n)
  (map intersection-sets 
       (transpose 
        (map 
         (lambda (x) (free-squares-one-queen x n)) 
         queen-squares)))) 

(define (get-index z i)
  (define (try x last j)
    (cond ((or (null? x) (> j i)) ())
          ((= j i) (car x))
          (else (try (cdr x) (car x) (+ j 1)))))
  (try z null 0))

;(get-index (list 1 2 3 4) 0)

; ci starts from 1
(define (free-squares-next-col queen-squares n)
  (if (null? queen-squares) 
      (map (lambda (y) (square 1 y)) (enumerate-interval 1 n))
      (let ((col (+ 1 (length queen-squares))))
        (map (lambda (x) (square col x)) 
             (get-index 
              (free-squares-multiple-queens queen-squares n) 
              (- col 1)))))) 
  

"(free-squares-next-col (list (square 1 3) (square 2 7)) 8)"
(free-squares-next-col (list (square 1 3) (square 2 7)) 8) 

"(free-squares-next-col () 8)"
(free-squares-next-col () 8) 

(define queen-squares (list (square 1 3) (square 2 7)))
"(captured-squares (square 1 3) 8)"
(captured-squares (square 1 3) 8)

"(free-squares-one-queen (square 1 3) 8)"
(free-squares-one-queen (square 1 3) 8)

"(free-squares-one-queen (square 2 7) 8)"
(free-squares-one-queen (square 2 7) 8)

"(map (lambda (x) (free-squares-one-queen x 8)) queen-squares)"
(map (lambda (x) (free-squares-one-queen x 8)) queen-squares)

"(tranpose (map (lambda (x) (free-squares-one-queen x 8)) 
                queen-squares))"
(transpose (map (lambda (x) (free-squares-one-queen x 8)) 
                queen-squares))

"(free-squares-multiple-queens (list (square 1 3) (square 2 7)) 8)"
(free-squares-multiple-queens (list (square 1 3) (square 2 7)) 8)

(free-squares-multiple-queens 
 (list (square 1 3) 
       (square 2 7) 
       (square 3 2) 
       (square 4 8) 
       (square 5 5) 
       (square 6 1) 
       (square 7 4)) 
 8)

(free-squares-multiple-queens 
 (list (square 1 3) 
       (square 2 7) 
       (square 3 2) 
       (square 4 8) 
       (square 5 5) 
       (square 6 1)) 
 8)

(define (last z)
  (define (try x last)
    (cond ((null? x) last)
          ((null? (cdr x)) (car x))
          (else (try (cdr x) (car x)))))
  (try z null))

(define (flatten n srclist)
  (define (try inlist outlist)
    (cond ((or (null? inlist) 
               (not (list? inlist))) 
           outlist)
          ((and (list? inlist) 
                (not (list? (car inlist)))) 
           (append outlist (list inlist)))
          (else 
           (try (cdr inlist) (try (car inlist) outlist)))))
  (try srclist ()))

(define (queens n queens-square-list)
  (if (= (length queens-square-list) n)
       queens-square-list
       (let ((free-squares 
             (free-squares-next-col queens-square-list n)))
         (flatten n 
                  (map (lambda (x) 
                         (queens 
                          n 
                          (append queens-square-list 
                                  (if (null? free-squares) 
                                      ()
                                      (if (list? x) x (list x))))))
                       free-squares)))))

(define (queens n queens-square-list)
  (if (= (length queens-square-list) n)
       queens-square-list
       (let ((free-squares 
             (free-squares-next-col queens-square-list n)))
         (flatten n 
                  (map (lambda (x) 
                         (queens 
                          n 
                          (append queens-square-list 
                                  (if (null? free-squares) 
                                      ()
                                      (if (list? x) x (list x))))))
                       free-squares)))))


"(length (queens 8 ()))"
(length (queens 8 ()))
;92 

"(queens 8 ())"
(queens 8 ())
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

