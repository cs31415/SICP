(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions)) 
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens)) 
;                 (enumerate-interval 1 board-size)))
;        (queen-cols (- k 1))))))
          ; returns list of all possible boards
          (flatmap ; flatten tree of boards into a list of boards   
           (lambda (new-row)
             (map (lambda (rest-of-queens) ; for each board 
                    (adjoin-position new-row k rest-of-queens)) 
                  (queen-cols (- k 1))))  ;  list of all boards
           (enumerate-interval 1 board-size))))) ;           
  (begin
    (define start-time (current-inexact-milliseconds))
    (define result (queen-cols board-size))
    (display (string-append 
              (number->string board-size) 
              " = " 
              (number->string 
               (- (current-inexact-milliseconds) 
                  start-time))))
    (newline))
    board-size)


; In 2.42, queen-cols is called 1 time per iteration
; T = q8 + q7 + q6 + q5 + ... + q1

; In Louis Reasoner's version, queen-cols is called board-size 
; times per iteration
; T' = 
;  q8' + 
;  8*(q7' + 8*(q6' + 8*(q5' + 8*(q4' + 8*(q3' + 8*(q2' + 8*q1'))))))  
; T' = 
;  q8' + 8*q7' + 8^2q6' + 8^3q5' + 8^4q4' + 8^5q3' + 8^6q2' + 8^7q1'  
; Thus, Louis' version makes 8 times as many recursive calls in each 
; iteration.
; The time taken by Louis' version is a power series where the
; coefficients are the per-iteration timings
; There really is no simple formula that relates T' to T.
; The power series formula is borne out by measurements of timings 
; for 8.42 and 8.43:

; In the table below, f2 is the timing for Louis' program, f1 is the 
; timing for the exercise 2.42 program.

n  B = f2        C = f1         D = 8*f2      Dn+Dn-1	
1  0.110839844   0.095947266    0.88671875	
2  0.343017578   0.277099609    2.744140625   0.88671875
3  3.366943359   1.635986328    26.93554688   3.630859375
4  33.27807617   7.724853516    266.2246094   29.6796875
5  283.7128906   26.24414063    2269.703125   293.1601563
6  2359.414063   64.83398438    18875.3125    2535.927734
7  19068.71606   104.3120117    152549.7285   21145.01563
8  143775.114    133.8869629    1150200.912   171425.041



; 2.42:
; T8 = 8*Ta*n7 + q7
; T7 = 8*Ta*n6 + q6

; 2.43:
; T8' = 8*Ta*n7 + 8*q7
; T7' = 8*Ta*n6 + 8*q6



; 2.42:  solves in time T
; i.e. (queen-cols N) = T
; filter > flatmap (queen-cols k-1) > map (1..N)
; queen-cols N = N * queen-cols N-1 + N * queen-cols N-2 + .. + N * queen-cols 1
; q8 > q7 > q6 > q5 ... > q0
; ; N q8 > N q7 > N q6 > N q5 ... > N q0

;; 
; 2.43:  board-size * T
; queen-cols is O(N^3)
; It is called N times 
; Thus queens becomes O(N^4)
; adjoin-position: O(N)
; enumerate-interval: O(N)
; map: O(N)
; queen-cols: 
; filter: O(N)
; flatmap: O(N^2)
;        (filter ; O(N)
;         (lambda (positions) (safe? k positions))
;          (flatmap
;           (lambda (new-row)
;             (map (lambda (rest-of-queens)
;                    (adjoin-position new-row k rest-of-queens))
;                  (queen-cols (- k 1))))
;           (enumerate-interval 1 board-size)))))            



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



;(map (lambda (x) (queens x)) (enumerate-interval 1 8))

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


(define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))  ; safe? returns valid boards (where the queens aren't in conflict)
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row k rest-of-queens))
;                 (enumerate-interval 1 board-size)))
;        (queen-cols (- k 1))))))
          ; returns list of all possible boards
          (flatmap ; flatten tree of boards into a list of boards   
           (lambda (new-row)
             (map (lambda (rest-of-queens) ; for each board rest-of-queens in B
                    (adjoin-position new-row k rest-of-queens)) ; adjoin new-row to rest-of-queens
                  (queen-cols (- k 1))))  ; x8; list of all boards with k-1 queens in 1st k-1 columns (B)
           (enumerate-interval 1 8)))))

;(length (queen-cols 7))
;312
;(length (queen-cols 6))
;550
;(length (queen-cols 5))
;568
;(length (queen-cols 4))
;344
;(length (queen-cols 3))
;140
;(length (queen-cols 2))
;42
(length (queen-cols 1))
