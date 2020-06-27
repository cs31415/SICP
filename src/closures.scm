; closure property of cons is the ability to apply cons on it's output
; in terms of set theory, a closure is a function whose range is a 
; subset of or coincident with it's domain


; sequence - produced by nested cons pairs
(cons 1 (cons 2 (cons 3 (cons 4 null))))

; Scheme equivalent of the above:
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

(car one-through-four)
(cdr one-through-four)

(cons 10 one-through-four)
(cons one-through-four 5)


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 0)
(list-ref squares 2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length squares)

; append cdr's down list1 while car'ing up the answer list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define odds (list 1 3 5 7 9))

(append odds squares)

