(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Theta(n) 
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

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

; start at root. 
; Is node a leaf?  If yes, then return null.
; Does (symbols tree) contain symbol? If not, then
; return null.  
; Does (symbols (left-branch tree)) contain symbol? If yes, then
; append 0 to result and go down left branch .
; If not, then append 1 to result and go down right branch until
; leaf reached.

; Theta()
(define (encode-symbol symbol tree)
  (define (try-encode-symbol symbol tree code)
    (let ((node (if (= 1 (length tree)) (car tree) tree)))
      (cond ((and (leaf? node) 
                  (equal? symbol (car (symbols node))))
             code) 
            ; We only need to check this for the root.
            ((and (null? code) (not (element-of-set? symbol (symbols node)))) ; a
             '(ERROR: Symbol not found)) 
            (else
             (let* ((ltree (left-branch node))
                    (rtree (right-branch node))
                    (goleft (element-of-set? symbol (symbols ltree))) ; b
                    (nextbranch (if goleft (list ltree) (list rtree))))
               (try-encode-symbol 
                  symbol 
                  nextbranch 
                  (append code (if goleft (list 0) (list 1))))))))) ; c
  (try-encode-symbol symbol tree '()))


; 1 iteration = a+b+c
; log2(n) iterations since we are traversing a tree of height n
; a = n+n-1+...+1 = n(n+1)/2 [1,n]
; b = a/2  [1,n/2]
; c = 0 + 1 + 2 + ... + n-2 = (n-2)(n-1)/2  [0,n-2]
; In general, encode-symbol is of order theta(n^2)

; How is symbol frequency related to number of steps?
; What are the symbol codes for most/least frequent symbols for n=5, n=10?
;"htree1"
;(0 0 0 0) 
;(1) 
;"htree2"
;(0 0 0 0 0 0 0 0 0)
;(1)
; least frequent symbol requires n-1 iterations
; most frequent symbol requires 1 iteration



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

(define pairs (list (list 'A 8) 
                    (list 'B 3) 
                    (list 'C 1) 
                    (list 'D 1) 
                    (list 'E 1) 
                    (list 'F 1) 
                    (list 'G 1) 
                    (list 'H 1)))
(define htree (generate-huffman-tree pairs))
;htree
;((leaf A 8)
;  ((((leaf H 1) (leaf G 1) (H G) 2) 
;    ((leaf F 1) (leaf E 1) (F E) 2) 
;    (H G F E) 
;    4)
;   (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5)
;   (H G F E D C B)
;   9)
;  (A H G F E D C B)
;  17)


(encode-symbol 'A htree)
;(0)

(encode-symbol 'B htree)
;(1 1 1)

(encode-symbol 'C htree)
;(1 1 0 1)

(encode-symbol 'D htree)
;(1 1 0 0)

(encode-symbol 'E htree)
;(1 0 1 1)

(encode-symbol 'F htree)
;(1 0 1 0)

(encode-symbol 'H htree)
;(1 0 0 0)

(encode-symbol 'G htree)
;(1 0 0 1)

(encode-symbol 'M htree)
;(ERROR: Symbol not found)

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(encode (list 'A 'D 'A 'B 'B 'C 'A) sample-tree)
;(0 1 1 0 0 1 0 1 0 1 1 1 0)




"htree1"
(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) 
                    (list 'E 16)))
(define htree1 (generate-huffman-tree pairs))
(encode-symbol 'A htree1)
(encode-symbol 'E htree1)


"htree2"
(define pairs (list (list 'A 1) (list 'B 2) (list 'C 4) (list 'D 8) 
                    (list 'E 16) (list 'F 32) (list 'G 64) (list 'H 128) (list 'I 256) (list 'J 512)))
(define htree2 (generate-huffman-tree pairs))
(encode-symbol 'A htree2)
(encode-symbol 'J htree2)
