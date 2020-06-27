(define (reduce args numeric)
  (let* ((noMoreArgs (null? args)) ; reached end of list
         (first (if noMoreArgs null (car args)))
         (rest (if noMoreArgs null (cdr args))))
   (cond ; if numeric component is zero, return empty list else return
         ; (list numeric) so that the append below has a list to work with
         (noMoreArgs (if (=number? numeric 0) null (list numeric)))
         ((number? first) (reduce rest (+ numeric first)))
         (else (append (list first) (reduce rest numeric))))))

(define (make-sum . args)
  (let ((sum (reduce args 0)))
    (if (and (null? (cdr sum)) (number? (car sum))) 
        (car sum)
        (append (list '+) sum))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;(reduce '(x 1 y 2 0 z 1 3 5) 0)

(make-sum 'x 1 'y 2 0 'z 1 3 5)
(make-sum 'x 'y)
(make-sum 0 'x 'y)
(make-sum 0 1 2)
