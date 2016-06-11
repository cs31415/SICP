; Note: Language needs to be Textual(MzScheme, includes R5RS)

;(load "genericoperations.scm")
;(load "genericopswithtypecoercion.scm")

(define (break)
  (display "break"))
(break)

;.................................................................
; Hashtable for generic apply
;.................................................................
(define h (make-hash-table 'equal))
 
(define (put op type proc)
  (hash-table-put! h (list op type) proc))
 
(define (get op type)
  (hash-table-get h (list op type) (lambda () #f)))
 
;.................................................................
; Type tag definitions
;.................................................................
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
(define (contents-recursive datum)
  (if (has-type-tag? datum)
      (let ((tt (type-tag datum)))
        (cond ((equal? tt 'rectangular) datum)
              ((equal? tt 'polar) datum)
              ((equal? tt 'rational) (/ (numer (contents datum))
                                        (denom (contents datum))))
              (else (contents-recursive (contents datum)))))
      datum))
 
; We might have to add a type layer on top of 
; scheme-number just like we did for complex types
(define (get-type x)
  (let ((type (type-tag x)))
    (if (equal? type 'scheme-number) 
        (if (integer? (contents x)) 'integer 'real)
        type)))
 
 
;.................................................................
; Define a Type
;.................................................................
(define (make-typerec typename raisefunc projectfunc) 
  (list typename raisefunc projectfunc))
(define (typename typerec)
  (car typerec))
(define (raiseFunc typerec)
  (cadr typerec))
(define (projectFunc typerec)
  (caddr typerec))
 
;.................................................................
; Drop
;.................................................................
; drop an object down the tower as far as possible
(define (drop x)
  (let ((y (project x)))
    (if (or (equal? (typename x) (typename y))
            (not (equ? x (raise y))))
        x 
        (drop y))))
 
;.................................................................
; Setup type tower
; dense > sparse
;.................................................................
; iterate type-tower and setup type functions
(define (setup-tower type-tower)
  (map (lambda (typerec)
         (let ((fr (raiseFunc typerec))
               (fp (projectFunc typerec)))
           (begin
            (if (not (null? fr))
               (put 'raise (typename typerec) fr))
            (if (not (null? fp))
                (put 'project (typename typerec) fp)))))
       type-tower))

 
; which type is higher in the tower?
(define (ishigher? typename1 typename2)
  (define (try remainingtypes)
    (if (and (not (null? remainingtypes))
             (not (null? (car remainingtypes))))
        (let ((nexttypename (typename (car remainingtypes))))
          (if (equal? nexttypename typename1)
            #f
            (if (equal? nexttypename typename2)
                #t
                (try (cdr remainingtypes)))))        
        #f)
    )
  (try type-tower))
 
;.................................................................
; Coercion
; Note: Language needs to be Textual(MzScheme, includes R5RS)
;.................................................................
; hash table for coercion functions
(define hc (make-hash-table 'equal))
(define (putc op type proc)
  (hash-table-put! h (list op type) proc))
 
(define (getc op type)
  (hash-table-get h (list op type) (lambda () #f)))
 
; Here we need to check if targettypename is higher than x, 
; only then attempt coercion else return the input value
(define (get-coercion x targettypename)
  (if (equal? (get-type x) targettypename)
      x
      (if (ishigher? targettypename (get-type x))
          (let ((rx (raise x)))
            (get-coercion rx targettypename))
          x)))
   
;.................................................................
; Generic Apply function using drop
;.................................................................
(define (apply-generic op . args)
  (define (try op args remainingargs)
    (let ((type-tags (map typename args)))
      (let ((proc (get op (if (null? (cdr type-tags))
                            (car type-tags)
                            type-tags))))
        (if proc
            (let ((rslt (apply proc (map contents args))))
              ; This causes a circular reference because drop 
              ; calls project which calls apply-generic which 
              ; calls drop again. Hence the check for op = project
              (if (and (not (equal? op 'project)) 
                       (not (equal? op 'raise))
                       (has-type-tag? rslt))
                  (drop rslt)
                  rslt))
            (if (and (not (null? remainingargs)) 
                     (not (null? (car remainingargs))))
                (let ((cargs 
                       (map
                        (lambda (x) 
                          (let* ((nextarg (car remainingargs))
                                 (typet (typename nextarg)))
                            (get-coercion x typet)))
                        args)))
                  (try op cargs (cdr remainingargs)))
                (error "No method for these types"
                           (list op type-tags)))))))
  (try op args args))

(define (raise x)
  (begin
  (apply-generic 'raise x)))


(define (project x)
  (apply-generic 'project x))

;.................................................................
; dense > sparse
; Raise
;.................................................................

(define (dense->sparse d)
  (define (pos l)
    (if (null? l)
        null
        (cons (make-term (- (length l) 1) (contents (car l)))
              (pos (cdr l)))))
  (let* ((v (variable-poly d))
         (tl (termlist-poly d)))
    (make-polynomial 
     v 
     (make-sparse-termlist (car (pos (contents tl)))))))

;.................................................................
; Project
; Lower a type down the tower
;.................................................................
 
(define (project-sparse s)
  (define (expand l pos)
    (cond ((or (< pos 0)) null)
          ((and (>= pos 0) (null? l))
           (cons (make-scheme-number 0)
                 (expand l (- pos 1))))
          ((> pos (order-term (car l)))
              (cons (make-scheme-number 0)
                    (expand l (- pos 1))))
          (else
           (cons (cadr (contents (car l)))
                 (expand (cdr l) (- pos 1))))))
  (let* ((v (variable-poly s))
         (tl (termlist-poly s))
         (maxorder (order-term (car (contents tl)))))
    (make-polynomial 
     v 
     (make-dense-termlist-from-list (expand (contents tl) maxorder)))))
  
(define type-tower  
  (list (make-typerec 
         'dense dense->sparse (lambda (x) x))
        (make-typerec 
         'sparse (lambda (x) x) project-sparse)))


(setup-tower type-tower)
type-tower

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

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
;(define (zero? n)
;  (apply-generic 'zero n))
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
(define (coeff-term term) (apply-generic 'coeff term))
(define (order-term term) (apply-generic 'order term))
(define (first-term-termlist term-list) 
  (apply-generic 'first-term term-list))
(define (rest-terms-termlist term-list) 
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) 
  (apply-generic 'empty-termlist? term-list))
(define (adjoin-polynomial-term term term-list) 
  (apply-generic 'adjoin-term term term-list))
(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))


(define (make-dense-termlist . terms)
  ((get 'make-termlist 'dense) terms))
(define (make-dense-termlist-from-list terms)
  ((get 'make-termlist 'dense) terms))
(define (make-sparse-termlist . terms)
  ((get 'make-termlist 'sparse) terms))
(define (make-termlist-from-term term)
  ((get 'make-termlist (type-tag term)) (list (contents term))))
(define (make-dense-term coeff)
  ((get 'make-term 'dense) coeff))
(define (make-sparse-term order coeff)
  ((get 'make-term 'sparse) order coeff))

(define the-empty-termlist '())
;(define (empty-termlist? term-list) (null? term-list))

;...............................................................
; Dense term lists
;...............................................................
;No make-term since it just returns itself
(define (install-dense-termlist-package)
  (define (order term-list) (- (length term-list) 1))
  (define (coeff term) term)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (cons term term-list))
  
  (define (add-terms L1 L2)
    (cond ((empty? L1) L2)
          ((empty? L2) L1)
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
  (put 'first-term 'dense (lambda (l) (first-term l)))
  (put 'rest-terms 'dense (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term '(dense dense) 
       (lambda (t l) (tag (adjoin-term (tag t) l))))
  (put 'add-terms '(dense dense) 
       (lambda (l1 l2) (tag (add-terms l1 l2))))
  (put 'add '(dense dense) (lambda (t1 t2) (tag (add t1 t2))))
  (put 'make-termlist 'dense 
       (lambda (l) (tag (map (lambda (x) (tag x)) l))))
  (put 'make-term 'dense (lambda (c) (tag c)))
  (put 'negate 'dense (lambda (x) (tag (negate x))))
  (put 'empty-termlist? 'dense empty?)
  (put 'zero 'dense (lambda (x) (=zero? x)))
  'done
  )
  
(install-dense-termlist-package)

;...............................................................
; Sparse term lists
;...............................................................
;why this duplicate defn here and within the sparse package?
(define (make-term order coeff) (list order coeff))

(define (install-sparse-termlist-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty? term-list) (null? term-list))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons (tag term) term-list)))
  
  (define (add-terms L1 L2)
    (cond ((empty? L1) L2)
          ((empty? L2) L1)
          (else
           (let ((t1 (contents (first-term L1))) 
                 (t2 (contents (first-term L2))))
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
  
  (put 'order 'sparse (lambda (l) (order l)))
  (put 'coeff 'sparse (lambda (l) (coeff l)))
  (put 'first-term 'sparse (lambda (l) (first-term l)))
  (put 'rest-terms 'sparse (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term '(sparse sparse) 
       (lambda (t l) (tag (adjoin-term t l))))
  (put 'add-terms '(sparse sparse) 
       (lambda (l1 l2) (tag (add-terms l1 l2))))
  (put 'add '(sparse sparse) 
       (lambda (t1 t2) (tag (add t1 t2))))
  (put 'make-termlist 'sparse 
       (lambda (l) 
         (tag 
          (map (lambda (x) (tag x)) 
               (filter (lambda (y) (not (=zero? (coeff y)))) 
                       l)))))
  (put 'make-term 'sparse (lambda (o c) (tag (make-term o c))))
  (put 'negate 'sparse 
       (lambda (x) (tag (make-term (order x) (negate (coeff x))))))
  (put 'empty-termlist? 'sparse empty?)
  (put 'zero 'sparse (lambda (x) (=zero? (coeff x))))
  'done
  )

(install-sparse-termlist-package)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;...............................................................
; Polynomial package
;...............................................................
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
        (let* ((tl (term-list p))
               (var (variable p))
               (first-term (first-term-termlist tl))
               (rest-terms (rest-terms-termlist tl))
               (first-term-neg (negate first-term)))
            (make-poly 
             var
             (if (not (empty-termlist? rest-terms)) 
                 (adjoin-polynomial-term 
                  first-term-neg
                  (term-list (negate-poly 
                              (make-poly var 
                                         rest-terms))))
                 (make-termlist-from-term first-term-neg))))
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
  (put 'variable 'polynomial variable)
  (put 'term-list 'polynomial term-list)
  'done)
(install-polynomial-package)

(define (variable-poly p)
  ((get 'variable 'polynomial) (contents p)))
(define (termlist-poly p)
  ((get 'term-list 'polynomial) (contents p)))
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

(define a (dense->sparse p1))
a
;(make-sparse-termlist (list (make-term 2 (make-scheme-number 3)) (make-term 1 (make-scheme-number 0))))
;(dense->sparse p1)
(project-sparse a)


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
(break)
(negate p2)
;(polynomial
;  y
;  (dense
;   (dense scheme-number . -6)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . -4)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)))

(add p1 p2)
;(polynomial
;  y
;  (dense
;   (dense scheme-number . 6)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 7)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)))

(add p2 (negate p2))
;(polynomial
;  y
;  (dense
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)))

(sub p2 p2)
;(polynomial
;  y
;  (dense
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)
;   (dense scheme-number . 0)))

(define ps1 
  (make-polynomial 'y 
                   (make-sparse-termlist 
                         (make-term 2 (make-scheme-number 3))
                         (make-term 1 (make-scheme-number 0))
                         (make-term 0 (make-scheme-number 0))
                         )))
ps1
;(polynomial y (sparse (sparse 2 (scheme-number . 3))))

(negate ps1)
;(polynomial y (sparse (sparse 2 (scheme-number . -3))))

(add ps1 (negate ps1))
;(polynomial y (sparse))

(define ps2
  (make-polynomial 'y 
                   (make-sparse-termlist 
                         (make-term 3 (make-scheme-number 4))
                         (make-term 2 (make-scheme-number 2))
                         (make-term 1 (make-scheme-number 1))
                         )))
ps2
;(polynomial
;  y
;  (sparse (sparse 3 (scheme-number . 4)) 
;          (sparse 2 (scheme-number . 2)) 
;          (sparse 1 (scheme-number . 1))))


(add ps1 ps2)
;(polynomial
;  y
;  (sparse (sparse 3 (scheme-number . 4)) 
;          (sparse 2 (scheme-number . 5)) 
;          (sparse 1 (scheme-number . 1))))

(sub ps1 ps1)
;(polynomial y (sparse))

(sub ps1 ps2)
;(polynomial
;  y
;  (sparse (sparse 3 (scheme-number . -4)) 
;          (sparse 2 (scheme-number . 1)) 
;          (sparse 1 (scheme-number . -1))))


; We don't have a coercion system yet to convert between
; sparse and dense;  
; This layer could convert between sparse and dense -
; if sparse and dense terms are combined, then sparse
; 