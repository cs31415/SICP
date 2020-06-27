; returns list of captured rows for each column
(define (square c r)
  (cons c r))

(define (row square)
  (cdr square))

(define (col square)
  (car square))

; projection of a square sq onto a column c on a n x n board
(define (project-col sq c n)
  (if (= (col sq) c)
      (enumerate-interval 1 n)
      (let* ((rs (row sq)) 
             (cs (col sq))
             (cdiff (- c cs))
             (pr1 (+ rs cdiff))
             (pr2 (- rs cdiff)))
        (filter (lambda (x) (and (> x 0) (<= x n)))
                (list (min pr1 pr2) rs (max pr1 pr2))))))

; squares captured by a queen sitting on square sq on an n x n board 
(define (captured-squares sq n)
  (accumulate append () 
              (map (lambda (x) (list (project-col sq x n))) (enumerate-interval 1 n))))

; removelist removes a list of values from another list 
(define (removelist src-list from-list)
  (if (null? src-list)
      from-list
      (removelist (cdr src-list) (remove (car src-list) from-list))))

(define (inverse-project-col sq c n)
  (removelist (project-col sq c n) (enumerate-interval 1 n)))

(define (free-squares-one-queen queen-square n)
  (accumulate append () 
              (map (lambda (x) (list (inverse-project-col queen-square x n))) (enumerate-interval 1 n))))

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

(define (accumulate-n op init seqs)
  (if (null? (car seqs))  
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (free-squares-multiple-queens queen-squares n)
  (map intersection-sets (transpose (map (lambda (x) (free-squares-one-queen x n)) queen-squares)))) 
