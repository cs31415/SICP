; Y Combinator - aka how to define a recursive function when the language doesn't
; natively support recursion
; Courtesy Brian Harvey:
; (https://www.youtube.com/watch?v=tYVbCmqFVhY&index=12&list=PLE741AD926AA152C4)
((lambda (n)
   ((lambda (f) (f f n))
    (lambda (fact n)
      (if (= n 0)
          1
          (* n (fact fact (- n 1)))))))
 5)

; Starting with my naive Y combinator where we define
; a lambda that calculates factorial taking in a function
; to call recursively, and pass in the same lambda to 
; itself:
((lambda (n)
  ((lambda (fact x)
    (if (= x 0)
        1
        (* x (fact fact (- x 1)))))
   (lambda (fact x)
    (if (= x 0)
        1
        (* x (fact fact (- x 1))))) n)) 5)

; Applying DRY
((lambda (n)
  (let ((f (lambda (fact x)
    (if (= x 0)
        1
        (* x (fact fact (- x 1)))))))
    (f f n))) 5)

; let is syntactic sugar for lambda. Replacing with lambda
((lambda (n)
   ((lambda (f) (f f n))
    (lambda (fact x)
      (if (= x 0)
          1
          (* x (fact fact (- x 1))))))) 5)
    

;; gives us the generic form of the Y-combinator
;(lambda (f) (f f n))
;
;
;(define Y
;  (lambda (r)
;    ((lambda (f) (f f))
;     (lambda (y)
;       (r (y y))))))

((lambda (n)
   ((lambda (f) (f f n))
    (lambda (fact x)
      (if (= x 0)
          1
          (* x (fact fact (- x 1))))))) 5)


