; in number package
(define (equ? x y) (and (= x y)))
(put 'equ '(scheme-number scheme-number) equ?)


; in rational package
(define (equ? x y) (= (* (numer x) (denom y)) 
                      (* (numer y) (denom x))))
(put 'equ '(rational rational) equ?)

  
; in complex package
(define (equ? x y) (and (= (real-part x) (real-part y)) 
                        (= (imag-part x) (imag-part y))))
(put 'equ '(complex complex) equ?)

  
(define (equ? x y)
  (apply-generic 'equ x y))

