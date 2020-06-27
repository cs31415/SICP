; in number package
(define (install-scheme-number-package)
  (define (=zero? x) (= x 0))
  (put 'zero 'scheme-number =zero?)
  )

; in rational package
(define (install-rational-package)
  (define (=zero? x) 
    (and (= (numer x) 0) (not (= (denom x) 0))))
  (put 'zero 'rational =zero?)
  )

; in complex package
(define (install-complex-package)
  (define (=zero? x) (and (= (real-part x) 0) 
                          (= (imag-part x) 0)))
  (put 'zero 'complex =zero?)
  )
  
; generic function
(define (=zero? x)
  (apply-generic 'zero x))


; Test
(=zero? (make-scheme-number 0))
;#t
(=zero? (make-scheme-number 1))
;#f
(=zero? (make-rational 0 1))
;#t
(=zero? (make-rational 1 1))
;#f
(=zero? (make-complex-from-real-imag 3 4))
;#f
(=zero? (make-complex-from-real-imag 0 0))
;#t
(=zero? (make-complex-from-mag-ang 0 0))
;#t
(=zero? (make-complex-from-mag-ang 1 1))
;#f

