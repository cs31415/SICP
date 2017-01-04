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