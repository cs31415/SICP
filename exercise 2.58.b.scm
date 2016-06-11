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

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) 
  (let ((val (cddr s)))
    (if (> (length val) 1) val (car val))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (let ((val (cddr p)))
    (if (> (length val) 1) val (car val))))

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
    (infix sum '+)))

(define (make-list item)
  (cond ((null? item) null)
        ((list? item) item)
        (else (list item))))

(define (infix items operator)
  (let* ((sortedItems 
          (if (and (eq? operator '*) 
                   (number? (last items))) 
              (append (list (last items)) (butlast items))
              items)))
    (if (> (length sortedItems) 1) 
        (append 
         (flatmap 
          (lambda (x) 
            (append (make-list x) (list operator))
            ;(list x operator)
            ) (butlast sortedItems)) 
         (list (last sortedItems))) 
        (if (null? sortedItems) 0 (car sortedItems)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
 
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

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

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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
   
   (let* ((product (reduce args 1)))
     (infix product '*)))

; disregard anything inside parentheses (the fact that
; parenthetical terms are elements of the list obviates this 
; requirement).
; the predicate check then simply boils down to checking for 
; existence of + & * in the list. 
; if + exists (remember, + inside parentheses is not considered
; since the whole term will be an element of the list), then
; it is a sum.  if not, and if * exists, then it is a product.

(define (contains args symbol)
  (cond ((null? args) #f)
        ((eq? (first args) symbol) #t)
        (else (contains (cdr args) symbol))))

(define (sum? exp)
  (contains exp '+))
(define (product? exp)
  (and (contains exp '*) (not (contains exp '+))))

(define (split items symbol)
  (define (try items symbol lhs)
    (cond ((null? items) lhs)
          ((eq? (car items) symbol) (list lhs (cdr items)))
          (else (try (cdr items) 
                     symbol 
                     (append lhs (list (car items)))))))
  (try items symbol ()))
  
(define (delist x)
  (cond ((null? x) null)
        ((and (list? x) (null? (cdr x)) (car x)))
        (else x)))

(define (addend exp)
  (delist (car (split exp '+))))

(define (augend exp)
  (delist (cadr (split exp '+))))

(define (multiplier exp)
  (delist (car (split exp '*))))

(define (multiplicand exp)
  (delist (cadr (split exp '*))))

(deriv '(x + y + 3 * x + (3 * (x + y + 2))) 'x)
;7

(deriv '(x + 3) 'x)
;1
;
(deriv '(x * y) 'x)
;y

(deriv '((x * y) * (x + 3)) 'x)
;(x * y) + (y * (x + 3))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;4
(deriv '(x + 3 * (x + y + 2)) 'x)
;4
(deriv '(x + y + 3 * x) 'x)
;4

(deriv '(3 * (x + y + 2)) 'x)
;3
