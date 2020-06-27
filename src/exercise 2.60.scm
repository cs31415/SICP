; set with dups allowed

; Theta(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Theta(1)
(define (adjoin-set x set)
  (cons x set))

; Theta(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) (remove (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

; union s1 s2 = s2 + all of s1 not in s2
; Theta(n^2)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2)) 
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set1) 
                    (union-set (cdr set1) (remove (car set1) set2))))))


(define set1 (list 4 1 2 2 3 2 3))
(define set2 (list 2 2 3 3 4 4))
(intersection-set set1 set2)
;(4 2 2 3 3)

(union-set set1 set2)
;(4 1 2 2 3 2 3 4)

; adjoin is constant time versus Theta(n) for the non-duplicate set
; The dup version is suited to applications that append and don't care
; if the element already exists. 
; e.g. an inventory management system, a personnel management app etc.
