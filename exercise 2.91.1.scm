; Note: Language needs to be Textual(MzScheme, includes R5RS)

; Symbolic Algebra
(load "genericoperations.scm")
(define (break)
  (display "break"))
(break)


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (div a b)
  (/ a b))
(define (mul a b)
  (* a b))
(define (add a b)
  (+ a b))
(define (=zero? x)
  (= x 0))
(define (negate x)
  (* -1 x))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;<procedures same-variable? and variable? from section 2.3.2>
  ;; representation of terms and term lists
  ;<procedures adjoin-term ...coeff from text below>

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (neg-poly p)
    (make-poly (variable p) (negate-term (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate 'polynomial (lambda (p) (tag (neg-poly p))))
  'done)
(install-polynomial-package)

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

(define (negate-term L)
  (if (and (not (null? L)) 
           (not (empty-termlist? L)))
      (let* ((first-term (first-term L))
             (rest-terms (rest-terms L))
             (first-term-neg 
              (make-term (order first-term)
                         (negate (coeff first-term)))))
        (if (not (empty-termlist? rest-terms)) 
            (adjoin-term 
             first-term-neg
             (negate-term rest-terms))
            (list first-term-neg)))
      L))
  
(define (sub-terms L1 L2)
  (add-terms L1 (negate-term L2)))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let* ((q (make-term new-o new-c))
                     (rest-of-result
                     ;<compute rest of result recursively>
                      (div-terms (sub-terms L1 (mul-terms L2 (list q))) L2)))
                ;<form complete result>
                (list (adjoin-term q (car rest-of-result))
                      (cadr rest-of-result))
                ))))))


; **
; Notice that, since we operate on terms using the generic procedures
; add and mul, our polynomial package is automatically able to handle 
; any type of coefficient that is known about by the generic arithmetic
; package. If we include a coercion mechanism such as one of those
; discussed in section 2.5.2, then we also are automatically able to 
; handle operations on polynomials of different coefficient types,
; such as
;
; [3x^2 + (2 + 3i)x + 7].[x^4 + (2/3)x^2 + (5 + 3i)]



(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (div-polynomial var terms)
  (apply-generic 'div var terms))
(define (negate-polynomial p)
  (apply-generic 'negate p))

;...............................................................
; Testing
;...............................................................

(define p1 (make-polynomial 'x (list (make-term 5 1) (make-term 0 -1))))
p1

(define p2 (make-polynomial 'x (list (make-term 2 1) (make-term 0 -1))))
p2

(div-terms (list (make-term 5 1) (make-term 0 -1))
           (list (make-term 2 1) (make-term 0 -1)))
;(((3 1) (1 1)) ((1 1) (0 -1)))

(div-polynomial p1 p2)
;(polynomial x ((3 1) (1 1)) ((1 1) (0 -1)))
