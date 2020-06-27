; An exponent is a list like (** x 2)
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(exponentiation? '(** x 2))
(exponentiation? '(* x 2))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponent base exp)
  (list '** base exp))
; Adding rules:
; x^0 = 1
; x^1 = 1
(define (make-exponent base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else 
         (list '** base exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
          (make-product (make-product n (make-exponent u (- n 1))) 
                        (deriv u var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


; redefine make-sum: 
; - if both summands are numbers, add them and return their sum
; - if one of the summands is 0, then return the other summand
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; redefine make-product
; - 0 times anything is 0
; - 1 times anything is the thing itself
; - if both multiplicands are numbers, multiply them and return
;   the product

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 5) 'x)
;(* 5 (** x 4))

(deriv '(+ (** x 5) (* 3 (** x 4))) 'x)
;(+ (* 5 (** x 4)) (* 3 (* 4 (** x 3))))

(deriv '(** x 0) 'x)
;0
(deriv '(** x 1) 'x)
;1
(deriv '(** x 2) 'x)
;(* 2 x)
