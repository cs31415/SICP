(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (try-decode bits branch tree)
      (cond ((null? branch) '())
            ((leaf? branch) 
             (append (symbols branch) (try-decode bits tree tree)))
            ((null? bits) '())
            ((= 0 (car bits)) 
             (try-decode (cdr bits) (left-branch branch) tree))
            ((= 1 (car bits)) 
             (try-decode (cdr bits) (right-branch branch) tree))
            (else (display "Bad input string "))))
    (try-decode bits tree tree))

(define code-tree 
  (make-code-tree 
   (make-leaf 'A 8)
   (make-code-tree 
    (make-code-tree 
     (make-leaf 'B 3)
     (make-code-tree 
      (make-leaf 'C 1)
      (make-leaf 'D 1)))
     (make-code-tree 
      (make-code-tree 
       (make-leaf 'E 1)
       (make-leaf 'F 1))
      (make-code-tree 
       (make-leaf 'G 1)
       (make-leaf 'H 1))))))

;code-tree
;((leaf A 8)
; (((leaf B 3) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 5)
;  (((leaf E 1) (leaf F 1) (E F) 2) 
;   ((leaf G 1) (leaf H 1) (G H) 2) 
;   (E F G H)
;   4)
;  (B C D E F G H)
;  9)
; (A B C D E F G H)
; 17)

(decode (list 1 0 0 0 1 0 1 0) code-tree)
;(B A C)

(decode '(1 0 0 0 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 0
            1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1)
        code-tree)
;(B A C A D A E A F A B B A A A G A H)



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(make-leaf-set pairs)

