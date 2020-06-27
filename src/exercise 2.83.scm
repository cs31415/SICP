;integer > rational > real

; integer
(define (integer->rational i)
  (make-rational i 1))

; rational
(define (rational->real r)
  (make-scheme-number (/ (* 1.0 (numer r)) (denom r))))

; real
(define (real->complex r)
  (make-complex-from-real-imag r 0))

(put 'raise 'integer integer->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real real->complex)

(define (raise x)
  (apply-generic 'raise x))

(define (get-type x)
  (let ((type (type-tag x)))
    (if (equal? type 'scheme-number) 
        (if (integer? (contents x)) 'integer 'real)
        type)))

(define (get-coercion x tt)
  (if (equal? (get-type x) tt)
      x
      (let ((rx (raise x)))
        (get-coercion rx tt))))