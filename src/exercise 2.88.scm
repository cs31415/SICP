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
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

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

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; polynomial installer
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
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
               (first-term-neg 
                (make-term (order first-term)
                          (negate (coeff first-term)))))
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
  (put 'negate 'polynomial negate-poly)
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

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
                   (list 
                    (make-term 2 (make-scheme-number 3)))))
p1
;(polynomial y (2 (scheme-number . 3)))

(define p2 
  (make-polynomial 'y 
                   (list 
                       (make-term 5 (make-scheme-number 6))
                       (make-term 2 (make-scheme-number 4))
                       )))
p2 
;(polynomial y (5 (scheme-number . 6)) (2 (scheme-number . 4)))

(negate p2)
;(y (5 (scheme-number . -6)) (2 (scheme-number . -4)))

(add p1 p2)
;(polynomial y (5 (scheme-number . 6)) (2 (scheme-number . 7)))

(sub p1 p2)
;(polynomial y (5 (scheme-number . -6)) (2 (scheme-number . -1)))

(define p3 
  (make-polynomial 'y 
                   (list 
                    (make-term 6 (make-scheme-number 3))
                    (make-term 2 (make-scheme-number 3))
                    )))
p3
;(polynomial y (6 (scheme-number . 3)) (2 (scheme-number . 3)))

(define p4 
  (make-polynomial 'y 
                   (list 
                       (make-term 5 (make-scheme-number 6)) 
                       (make-term 2 (make-scheme-number 4))
                       )))
p4 
;(polynomial y (5 (scheme-number . 6)) (2 (scheme-number . 4)))

(add p3 p4)
;(polynomial y 
;            (6 (scheme-number . 3)) 
;            (5 (scheme-number . 6)) 
;            (2 (scheme-number . 7)))

(sub p3 p4)
;(polynomial y 
;            (6 (scheme-number . 3)) 
;            (5 (scheme-number . -6)) 
;            (2 (scheme-number . -1)))
