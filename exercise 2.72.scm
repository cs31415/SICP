(define (encode-symbol symbol tree)
  (define (try-encode-symbol symbol tree code)
    (let ((node (if (= 1 (length tree)) (car tree) tree)))
      (cond ((and (leaf? node) 
                  (equal? symbol (car (symbols node))))
             code) 
            ; We only need to do this error check once for the root.
            ((and (null? code) 
                  (not (element-of-set? symbol (symbols node)))) ; a
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
; a = n steps (since this only happens once for the root node)
; b = n-1 steps
; c = 0 + 1 + 2 + ... + n-2 = (n-2)(n-1)/2  [0,n-2]
; In general, encode-symbol is of order theta(n^2)

; How is symbol frequency related to number of steps?
; What are the symbol codes for most/least frequent symbols
; for n=5, n=10?
;"htree1"
;(0 0 0 0) 
;(1) 
;"htree2"
;(0 0 0 0 0 0 0 0 0)
;(1)
; least frequent symbol A requires n iterations
; a + b = n+n-1+...+1 + 1 (for leaf?) 
;       = 1 + n(n+1)/2 
; c = (n-2)(n-1)/2
; number of steps = a+b+c [ theta(n^2) ]
; most frequent symbol E requires 1 iteration
; a + b = n+1+1(for leaf?)
; c = 0
; number of steps = n+2 [ theta(n) ]




