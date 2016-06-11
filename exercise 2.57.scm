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
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (list? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;(define (augend s) (caddr s))
(define (augend s) 
  (let ((term (cddr s)))
    (cond ((null? (cdr term)) (car term))
          ((eq? (car term) '+) term)
          (else (append (list '+) term)))))

(define (product? x)
  (and (list? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;(define (multiplicand p) (caddr p))
(define (multiplicand p)
  (let ((term (cddr p)))
    (cond ((null? (cdr term)) (car term))
          ((eq? (car term) '*) term)
          (else (append (list '*) term)))))

; redefine make-sum: 
(define (make-sum . args)
  (define (reduce args numeric)
    (let* ((noMoreArgs (null? args)) ; reached end of list
           (first (if noMoreArgs null (car args)))
           (rest (if noMoreArgs null (cdr args))))
      (cond ; if numeric component is zero, return empty list 
        ; else return (list numeric) so that the append below 
        ; has a list to work with
        (noMoreArgs (if (=number? numeric 0) null (list numeric)))
        ((number? first) (reduce rest (+ numeric first)))
        (else (append (list first) (reduce rest numeric))))))
  
  (let ((sum (reduce args 0)))
    (if (and (null? (cdr sum)) (number? (car sum))) 
        (car sum)
        (append (list '+) sum))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; redefine make-product
; - 0 times anything is 0
; - 1 times anything is the thing itself
; - if both multiplicands are numbers, multiply them and 
;   return the product
  
(define (contains args n)
  (cond ((null? args) #f)
        ((=number? (first args) n) #t)
        (else (contains (cdr args) n))))

(define (make-product . args)
   (define (reduce args numeric)
     (let* ((noMoreArgs (null? args)) ; reached end of list
            (first (if noMoreArgs null (car args)))
            (rest (if noMoreArgs null (cdr args))))
       (cond ; if numeric component is 1, return empty list 
         ; else return (list numeric) so that the append below 
         ; has a list to work with
         ((contains args 0) 
          (list 0))
         (noMoreArgs 
          (if (=number? numeric 1) null (list numeric)))
         ((number? first) 
          (reduce rest (* numeric first)))
         (else 
          (append (list first) (reduce rest numeric)))))
   )
   
   (let* ((product (reduce args 1))
          (first (car product))
          (last (last product))
          (rest (cdr product))
          (butlast (butlast product)))
     (cond ((and (number? first) (null? rest)) 
            first)
           ((number? last) 
            (append (list '*) (list last) butlast)) 
           (else 
            (append (list '*) product)))))


(define (last z)
  (define (try x last)
    (cond ((null? x) last)
          ((null? (cdr x)) (car x))
          (else (try (cdr x) (car x)))))
  (try z null))

(define (butlast z)
  (cond ((null? z) null)
        ((null? (cdr z)) null)
        (else (remove (last z) z))))

;(deriv '(+ x 3) 'x)
;1

;(deriv '(* x y) 'x)
;(+ (* y))

;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* (* x y)) (* (+ (* y)) (+ x 3)))

; As we can see, we need to eliminate the */+ symbols
; when there is only a single term.

(define (make-sum . args)
  (define (reduce args numeric)
    (let* ((noMoreArgs (null? args)) ; reached end of list
           (first (if noMoreArgs null (car args)))
           (rest (if noMoreArgs null (cdr args))))
      (cond ; if numeric component is zero, return empty 
        ; list else return (list numeric) so that the append 
        ; below has a list to work with
        (noMoreArgs 
         (if (=number? numeric 0) null (list numeric)))
        ((number? first) 
         (reduce rest (+ numeric first)))
        (else 
         (append (list first) (reduce rest numeric))))))
  (let ((sum (reduce args 0)))
    (if (> (length sum) 1) 
        (append (list '+) sum) 
        (if (null? sum) 
            0
            (car sum)))))


(define (make-product . args)
   (define (reduce args numeric)
     (let* ((noMoreArgs (null? args)) ; reached end of list
            (first (if noMoreArgs null (car args)))
            (rest (if noMoreArgs null (cdr args))))
       (cond ; if numeric component is 1, return empty list 
         ; else return (list numeric) so that the append below 
         ; has a list to work with
         ((contains args 0) (list 0))
         (noMoreArgs 
          (if (=number? numeric 1) null (list numeric)))
         ((number? first) 
          (reduce rest (* numeric first)))
         (else 
          (append (list first) (reduce rest numeric)))))
   )
   
   (let* ((product (reduce args 1))
          (first (car product))
          (last (last product))
          (rest (cdr product))
          (butlast (butlast product)))
     (cond ((and (number? first) (null? rest))
            first)
           ((number? last) 
            (append (list '*) (list last) butlast)) 
           ((= (length product) 1) 
            (car product))
           (else 
            (append (list '*) product)))))


(deriv '(+ x 3) 'x)
;1

(deriv '(* x y) 'x)
;y

(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

(deriv '(* x y (+ x 3)) 'x)
;(multiplicand '(* x y (+ x 3)))


(deriv '(+ x y (* x 3)) 'x)
;4

(deriv '(+ x y (* 3 x) (* 3 (+ x y 2))) 'x)
