; leaf constructor and selectors
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; code tree constructor and selectors
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


; add an element to the set of ordered symbol-weight pairs
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Transform symbol-weight pairs into ordered set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define pairs (list (list 'A 8) (list 'B 3) (list 'C 1) (list 'D 1) 
                    (list 'E 1) (list 'F 1) (list 'G 1) (list 'H 1)))
(make-leaf-set pairs)
;((leaf H 1) (leaf G 1) (leaf F 1) (leaf E 1) 
; (leaf D 1) (leaf C 1) (leaf B 3) (leaf A 8))

; Generate Huffman Encoding Tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge ordered-leaves)
  (cond ((null? ordered-leaves) '())
        ((= 1 (length ordered-leaves)) ordered-leaves)
        (else
         (let ((first (car ordered-leaves))
               (second (cadr ordered-leaves)))
           (successive-merge (adjoin-set 
                              (make-code-tree first second) 
                              (cddr ordered-leaves)))))))


(generate-huffman-tree pairs)
;(((leaf A 8)
;  ((((leaf H 1) (leaf G 1) (H G) 2) 
;    ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4)
;   (((leaf D 1) (leaf C 1) (D C) 2) 
;    (leaf B 3) (D C B) 5)
;   (H G F E D C B)
;   9)
;  (A H G F E D C B)
;  17))

(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) 
                    (list 'E 16)))
(define htree1 (generate-huffman-tree pairs))


(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) 
                    (list 'E 16) (list 'F 32) (list 'G 64) (list 'H 128) (list 'I 256) (list 'J 512)))
(generate-huffman-tree pairs)
(define htree2 (generate-huffman-tree pairs))

(make-leaf-set pairs)
