;Exercise 1.20.  The process that a procedure generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative gcd procedure given above. Suppose we were to interpret this procedure using normal-order evaluation, as discussed in section 1.1.5. (The normal-order-evaluation rule for if is described in exercise 1.5.) Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed. How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


; Normal order evaluation - operator first evaluated down to primitives. Operands evaluated when needed.
; Apply this to gcd and remainder's evaluation.

(gcd 206 40) ; a = 206, b = 40
(gcd 40 (remainder 206 40)) ; a = 40, b = (remainder 206 40)
(if (= (remainder 206 40) 0)
(if (= 6 0) ; 1
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))); a = (remainder 206 40), b = (remainder 40 (remainder 206 40))
(if (= (remainder 40 (remainder 206 40)) 0))
(if (= (remainder 40 6) 0) ; 2
(if (= 4 0) ; 3
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; a = (remainder 40 (remainder 206 40)), b = (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
(if (= (remainder 6 (remainder 40 (remainder 206 40))) 0) ; 4
(if (= (remainder 6 (remainder 40 6)) 0) ; 5
(if (= (remainder 6 4) 0) ; 6
(if (= 2 0) ; 7
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; a = (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; b = (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
(if (= (remainder (remainder 40 6) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ; 8
(if (= (remainder (remainder 40 6) (remainder 6 (remainder 40 (remainder 206 40)))) 0) ; 9
(if (= (remainder 4 (remainder 6 (remainder 40 (remainder 206 40)))) 0) ; 10
(if (= (remainder 4 (remainder 6 (remainder 40 6))) 0) ; 11
(if (= (remainder 4 (remainder 6 4)) 0) ; 12
(if (= (remainder 4 2) 0) ; 13
(if (= 0 0) ; 14
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
(remainder 6 (remainder 40 (remainder 206 40))) ; 15
(remainder 6 (remainder 40 6)) ; 16
(remainder 6 4) ; 17
2 ; 18
    
; 18 remainder operations are actually performed in normal order
; Checking correctness;  shouldn't see any gcd calls that have numbers other than 206 and 40 (the original values of a and b)

; Applicative order evaluation; operands first evaluated, then operator.

(gcd 206 40) ; a = 206, b = 40
(gcd 40 (remainder 206 40)) ; a = 40, b = (remainder 206 40)
(gcd 40 6) ; 1
(if (= 6 0)
(gcd 6 (remainder 40 6)) ; a = 6, b = (remainder 40 6)
(gcd 6 4) ; 2
(if (= 4 0)
(gcd 4 (remainder 6 4)) ; a = 4, b = (remainder 6 4)
(gcd 4 2) ; 3
(if (= 2 0)
(gcd 2 (remainder 4 2)) ; a = 2, b = (remainder 4 2)
(gcd 2 0) ; 4
(if (= 0 0)
2
; 4 remainder operations are actually performed in applicative order

; Normal order is monstrously inefficient at least in this case.
