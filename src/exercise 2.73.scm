; a. number? and variable? can't be assimilated into the data-directed
; dispatch because we dispatch on operator and no operator is involved
; in this case.


; b. 

(define (install-deriv-sum-package)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))  
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (put 'deriv '+ (lambda (exp var) 
                   (make-sum 
                    (deriv (addend exp) var)
                    (deriv (augend exp) var))))
  'done)


(define (install-deriv-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (put 'deriv '* (lambda (exp var) 
                   (make-sum
                    (make-product (multiplier exp)
                                  (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                  (multiplicand exp)))))
  'done)


; c.

(define (install-deriv-exp-package)
  (define (base x)
    (cadr x))
  (define (exponent x)
    (caddr x))
  (define (make-exponent base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else 
           (list '** base exp))))
  (put 'deriv '** 
       (lambda (exp var)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product 
            (make-product n (make-exponent u (- n 1))) 
            (deriv u var)))))
  'done)


; d. The keys in the put procedures also need to be 
; swapped.
