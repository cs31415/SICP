; Note: Language needs to be Textual(MzScheme, includes R5RS)

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
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))


;...............................................................
; Defines for polynomial package
;...............................................................

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (=zero? p) (empty-termlist? (term-list p)))
  (define (tag x) (attach-tag 'polynomial x))
  
  (put 'zero '(polynomial) =zero?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (=zero? x)
  (apply-generic 'zero x))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;...............................................................
; Testing
;...............................................................
(define p (make-polynomial 'x null))
p
(=zero? p)
;#t

(define p2 (make-polynomial 'y (list (make-term 2 3))))
p2
(=zero? p2)
;#f
