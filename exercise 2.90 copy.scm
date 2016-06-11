; Note: Language needs to be Textual(MzScheme, includes R5RS)
(define (break)
  (display "break"))

;...............................................................
; Defines for generic operations
;...............................................................
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (has-type-tag? datum)
  (if (and (pair? datum)
           (not (null? (car datum)))
           (not (number? (car datum))))
      #t
      #f))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op (if (null? (cdr type-tags))
                            (car type-tags)
                            type-tags))))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))



;...............................................................
; Defines for scheme number package
;...............................................................
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'zero 'scheme-number (lambda (x) (= x 0)))
  (put 'negate 'scheme-number (lambda (x) (tag (* -1 x))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

; constructor for creating ordinary numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;...............................................................
; Defines for polynomial package
;...............................................................
; We need to have a sparse and dense type layer for term lists

; exported functions from term list package
;  adjoin-term
;  add-terms
;  coeff
;  first-term
;  rest-terms
;  empty-termlist?

; generic term list defines
(define (coeff term) (apply-generic 'coeff term))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))

;(define (make-dense-termlist . terms) (cons 'dense terms))
;(define (make-sparse-termlist . terms) (cons 'sparse terms))

(define (make-dense-termlist . terms)
  ((get 'make-termlist 'dense) terms))
(define (make-sparse-termlist . terms)
  ((get 'make-termlist 'sparse) terms))

(define (make-dense-term coeff)
  ((get 'make-dense-term 'dense) coeff))
(define (make-sparse-term order coeff)
  ((get 'make-dense-term 'dense) order coeff))

(define (the-empty-termlist) '())
(define (empty-termlist? term-list) (null? term-list))

;...............................................................
; Dense term lists
;...............................................................
;No make-term since it just returns itself
(define (install-dense-termlist-package)
  (define (order term-list) (- (length term-list) 1))
  (define (coeff term) term)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  
  (define (adjoin-term term term-list)
    (cons term term-list))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2))
                 (order1 (order L1))
                 (order2 (order L2)))
             (cond ((> order1 order2)
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< order1 order2)
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else ; same order. add them.
                    (adjoin-term
                     (add (coeff t1) (coeff t2))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (tag x) (attach-tag 'dense x))
  
  (put 'order 'dense (lambda (l) (tag (order l))))
  (put 'coeff 'dense (lambda (l) (tag (coeff l))))
  (put 'first-term 'dense (lambda (l) (tag (first-term l))))
  (put 'rest-terms 'dense (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'dense (lambda (t l) (tag (adjoin-term t l))))
  (put 'add-terms 'dense (lambda (l1 l2) (tag (add-terms l1 l2))))
  (put 'make-termlist 'dense (lambda (l) (tag (map (lambda (x) (tag x)) l))))
  (put 'make-term 'dense (lambda (c) (tag c)))
  
  'done
  )
  
(install-dense-termlist-package)

;...............................................................
; Sparse term lists
;...............................................................
(define (make-term order coeff) (list order coeff))

(define (install-sparse-termlist-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else ; same order. add them.
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (tag x) (attach-tag 'sparse x))
  
  (put 'order 'sparse (lambda (l) (tag (order l))))
  (put 'coeff 'sparse (lambda (l) (tag (coeff l))))
  (put 'first-term 'sparse (lambda (l) (tag (first-term l))))
  (put 'rest-terms 'sparse (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'sparse (lambda (t l) (tag (adjoin-term t l))))
  (put 'add-terms 'sparse (lambda (l1 l2) (tag (add-terms l1 l2))))
  (put 'make-termlist 'sparse (lambda (l) (tag (map (lambda (x) (tag x)) l))))
  (put 'make-term 'sparse (lambda (o c) (tag (make-term o c))))

  
  'done
  )

(install-sparse-termlist-package)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; polynomial installer
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (list variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cadr p))
  
  (define (add-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (negate-poly p) 
    (if (and (not (null? p)) 
             (not (empty-termlist? (term-list p))))
        (let* ((first-term (first-term (term-list p)))
               (rest-terms (rest-terms (term-list p)))
               (first-term-neg (negate (coeff first-term))))
            (make-poly 
             (variable p)
             (if (not (empty-termlist? rest-terms)) 
                 (adjoin-term 
                  first-term-neg
                  (term-list (negate-poly 
                              (make-poly (variable p) 
                                         rest-terms))))
                 (list first-term-neg))))
            p))
  (define (=zero? p) (empty-termlist? (term-list p)))
  (define (tag x) (attach-tag 'polynomial x))
  
  (put 'zero '(polynomial) =zero?)
  (put 'negate 'polynomial (lambda (p) (tag (negate-poly p))))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (=zero? x)
  (if (has-type-tag? x)
      (apply-generic 'zero x)
      (= x 0)))
(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (negate x)
  (apply-generic 'negate x))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

  
;...............................................................
; Testing
;...............................................................
(define tl (make-dense-termlist 
                         (make-scheme-number 3)
                         (make-scheme-number 0)
                         (make-scheme-number 0)
                         ))

(define p1 
  (make-polynomial 'y 
                   (make-dense-termlist 
                         (make-scheme-number 3)
                         (make-scheme-number 0)
                         (make-scheme-number 0)
                         )))

p1
;(polynomial y (dense 
;               (scheme-number . 3) 
;               (scheme-number . 0) 
;               (scheme-number . 0)))

(define p2 
  (make-polynomial 'y 
                   (make-dense-termlist 
                       (make-scheme-number 6)
                       (make-scheme-number 0)
                       (make-scheme-number 0)
                       (make-scheme-number 4)
                       (make-scheme-number 0)
                       (make-scheme-number 0)
                       )))
p2 
;(polynomial
;  y
;  (dense
;   (scheme-number . 6)
;   (scheme-number . 0)
;   (scheme-number . 0)
;   (scheme-number . 4)
;   (scheme-number . 0)
;   (scheme-number . 0)))


(empty-termlist? tl)

(break)
(negate p2)

;(add p1 p2)

;(sub p1 p2)


;(define ps1 
;  (make-polynomial 'y 
;                   (make-sparse-termlist 
;                         (make-term 2 (make-scheme-number 3))
;                         (make-term 1 (make-scheme-number 0))
;                         (make-term 0 (make-scheme-number 0))
;                         )))
;ps1

