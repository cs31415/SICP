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

(define (successive-merge ordered-leaves)
  (cond ((null? ordered-leaves) '())
        ((= 1 (length ordered-leaves)) ordered-leaves)
        (else
         (let ((first (car ordered-leaves))
               (second (cadr ordered-leaves)))
           (successive-merge (adjoin-set 
                              (make-code-tree first second) 
                              (cddr ordered-leaves)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define pairs (list (list 'a 2) 
                    (list 'boom 1) 
                    (list 'Get 2)
                    (list 'job 2)
                    (list 'na 16)
                    (list 'Sha 3)
                    (list 'yip 9)
                    (list 'Wah 1)))


(define htree (generate-huffman-tree pairs))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (try-encode-symbol symbol tree code)
    (let ((node (if (= 1 (length tree)) (car tree) tree)))
      (cond ((and (leaf? node) 
                  (equal? symbol (car (symbols node))))
             code) 
            ((not (element-of-set? symbol (symbols node))) 
             '(ERROR: Symbol not found)) 
            (else
             (let* ((ltree (left-branch node))
                    (rtree (right-branch node))
                    (goleft (element-of-set? symbol (symbols ltree)))
                    (nextbranch (if goleft (list ltree) (list rtree))))
               (try-encode-symbol 
                  symbol 
                  nextbranch 
                  (append code (if goleft (list 0) (list 1)))))))))
  (try-encode-symbol symbol tree '()))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


htree
;(((leaf na 16)
;  ((leaf yip 9)
;   (((leaf a 2) 
;     ((leaf Wah 1) (leaf boom 1) (Wah boom) 2) 
;     (a Wah boom) 
;     4)
;    ((leaf Sha 3) 
;     ((leaf job 2) (leaf Get 2) (job Get) 4) 
;     (Sha job Get) 
;     7)
;    (a Wah boom Sha job Get)
;    11)
;   (yip a Wah boom Sha job Get)
;   20)
;  (na yip a Wah boom Sha job Get)
;  36))

(encode (list 'Get 'a 'job) htree)
(encode (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na) htree)
(encode (list 'Get 'a 'job) htree)
(encode (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na) htree)
(encode (list 'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip) 
        htree)
(encode (list 'Sha 'boom) htree)

;(1 1 1 1 1 1 1 0 0 1 1 1 1 0)
;(1 1 1 0 0 0 0 0 0 0 0 0)
;(1 1 1 1 1 1 1 0 0 1 1 1 1 0)
;(1 1 1 0 0 0 0 0 0 0 0 0)
;(1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
;(1 1 1 0 1 1 0 1 1)

(length 
 (encode (list 'Get 'a 'job 
               'Sha 'na 'na 'na 'na 'na 'na 'na 'na
               'Get 'a 'job
               'Sha 'na 'na 'na 'na 'na 'na 'na 'na
               'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip
               'Sha 'boom) htree))
; 84 bits are required for encoding

; Using fixed length encoding, there are 8 symbols which can be 
; represented using a minimum of 3 bits each. 
; The message has 36 words(symbols) which translates to 
; 36*3 = 108 bits, about 20% more than our Huffman encoding.