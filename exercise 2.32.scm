(define (subsets s)
  (define (comb s)
    (if (null? s) null
        (map (lambda (x) (remove x s))
             s)))
  (if (null? s) null
      (append (append (list s) (comb s)) (subsets (cdr s)))))
  
(define (comb s)
  (cond ((null? s) null)
        ((not (list? s)) (list s))
        (else (map (lambda (x) (remove x s))
           s))))
;(comb (list 1 2 3 4))


(define (left s n)
  (if (or (= n 0) (null? s)) null
      (cons (car s) (left (cdr s) (- n 1)))))

(left (list 1 2 3 4 5) 3)
;(1 2 3)

(define (right s n)
  (let ((l (length s)))
    (define (try i x)
      (cond ((or (and (= i l) (= l n)) (null? x)) null)
            ((>= i (- l n)) x)
            (else (try (+ i 1) (cdr x)))))
    (try 0 s)))


(right (list 1 2 3 4) 3)
;(2 3 4)

(define (rotleft s)
  (append (cdr s) (list (car s))))

(rotleft (list 1 2 3 4))
;(2 3 4 1)
(rotleft (list 1 2 3 4))
;(3 4 1 2)
(rotleft (rotleft (rotleft (list 1 2 3 4))))
;(4 1 2 3)

(define (rotright s)
  (append (right s 1) (left s (- (length s) 1))))

(rotright (list 1 2 3 4))
;(4 1 2 3)
(rotright (rotright (list 1 2 3 4)))
;(3 4 1 2)

(define (butlast s)
  (if (and (pair? s) (null? (cdr s))) null
      (cons (car s) (butlast (cdr s)))))

  
(butlast (list 1 2 3 4 5))
;(1 2 3 4)

(define (combinations s n)
  (cond ((or (null? s) (zero? n)) null)
        ((= n 1) (map (lambda (x) (list x)) s))
        (else 
           (append 
            (combinations (cdr s) n)
            (map (lambda (x) (append (list (car s)) x)) 
                 (combinations (cdr s) (- n 1)))))))


(define x (list 1 2 3 4))

(combinations x 0)
;;()
;
(combinations x 1)
;;((1) (2) (3) (4))
;
(combinations x 2)
;;((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
;
(combinations x 3)
;;((1 2 3) (1 2 4) (1 3 4) (2 3 4))
;
(combinations x 4)
;;(1 2 3 4)
;
;(subsets x)
;;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


; Now for the coup de grace
(define (subsets s)
  (define (try n)
    (if (< n 0) (list null)
        (append (combinations s n) (try (- n 1)))))
  (try (length s)))

;((2) (3) (4) (2 3) (3 4) (2 4) (2 3 4) ())

(define x (list 1 2 3 4 5 6))
;(subsets x)

; Now for the textbook solution
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append x (list (car s)))) rest)))))
        
(subsets x)

; The algorithm works based on the fact that the subsets of s 
; can be divided into 2 groups:
; 1. a group that doesn't contain the first element of s
; 2. a group that contains the first element of x.  
; These groups are completely symmetrical in that appending the 
; first element of s to each element of the first group produces 
; the second group.
