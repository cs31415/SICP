;Exercise 1.26.  Louis Reasoner is having great difficulty doing exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they examine Louis's code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:
;
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))
;
;``I don't see what difference that could make,'' says Louis. ``I do.'' says Eva. ``By writing the procedure like that, you have transformed the (log n) process into a (n) process.'' Explain.
;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod2 base (/ exp 2) m)
                       (expmod2 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

;Expmod takes log(n) steps because n is halved with each step.
;
;Expmod2 takes n steps because n is halved with each step but that is negated by the additional call to expmod2 due to applicative order evaluation. The exact same argument to * is evaluated twice.