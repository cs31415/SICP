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
       (cond ; if numeric component is 1, return empty list else return
         ; (list numeric) so that the append below has a list to work with
         ((contains args 0) (list 0))
         (noMoreArgs (if (=number? numeric 1) null (list numeric)))
         ((number? first) (reduce rest (* numeric first)))
         (else (append (list first) (reduce rest numeric)))))
   )
   
   (let* ((product (reduce args 1))
          (first (car product))
          (last (last product))
          (rest (cdr product))
          (butlast (butlast product)))
     (cond ((and (number? first) (null? rest)) first)
           ((number? last) (append (list '*) (list last) butlast)) 
           (else (append (list '*) product)))))


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


(make-product 'x 1 'y 0)
;(* 2 x y)
(make-product 'x 1 'y 23)
(make-product 'x 1 'y 23)
