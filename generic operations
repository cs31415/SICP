; Note: Language needs to be Textual(MzScheme, includes R5RS)
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (square x) (* x x))

; install package for handling ordinary numbers
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (define (=zero? x) (= x 0))
  (put 'zero '(scheme-number) =zero?)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

; constructor for creating ordinary numbers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


; install package for handling rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (=zero? x) (and (= (numer x) 0) (not (= (denom x) 0))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'zero '(rational) =zero?)
 
  'done)

"install-rational-package"
(install-rational-package)

; constructor/selectors for rational number
(define (make-rational n d)
  ((get 'make 'rational) n d))


; install rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; install polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; install package for handling complex numbers
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (=zero? x) 
    (and (= (real-part x) 0) 
         (= (imag-part x) 0)))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'zero '(complex) =zero?)
  'done)

(install-complex-package)

; constructors for complex numbers
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define z (make-complex-from-real-imag 3 4))
(magnitude z)

; execution path
;(apply-generic 'magnitude ((get 'make-from-real-imag 'complex) 3 4))
;(apply-generic 'magnitude ((lambda (x y) (tag (make-from-real-imag x y))) 3 4))
;(apply-generic 'magnitude ((lambda (x y) (cons 'complex (make-from-real-imag x y))) 3 4))
;(apply-generic 'magnitude (cons 'complex (make-from-real-imag 3 4)))
;(apply-generic 'magnitude (cons 'complex ((get 'make-from-real-imag 'rectangular) 3 4)))
;(apply-generic 'magnitude (cons 'complex ((lambda (x y) (tag (make-from-real-imag x y))) 3 4)))
;(apply-generic 'magnitude (cons 'complex ((lambda (x y) (cons 'rectangular (make-from-real-imag x y))) 3 4)))
;(apply-generic 'magnitude (cons 'complex (cons 'rectangular (make-from-real-imag 3 4))))
;(apply-generic 'magnitude (cons 'complex (cons 'rectangular (cons 3 4))))
;(apply-generic 'magnitude (cons 'complex (cons 'rectangular (cons 3 4))))
;(apply-generic 'magnitude (cons 'complex (cons 'rectangular (cons 3 4))))
;(apply-generic 'magnitude (list 'complex 'rectangular (cons 3 4)))
;(magnitude 'rectangular (cons 3 4))
;(sqrt (+ (square (real-part (cons 3 4)))
;             (square (imag-part (cons 3 4)))))
;(sqrt (+ (square 3) (square 4)))
;(sqrt (+ 9 16))
;5

; in number package
;(define (=zero? x) (= x 0))
;(put 'zero '(scheme-number) =zero?)


; in complex package
;(define (=zero? x) (and (= (real-part x) 0) 
;                        (= (imag-part x) 0)))
;(put 'zero '(complex) =zero?)

; in rational package
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
;(define (=zero? x) (= (numer x) 0)) 
;(put 'zero '(rational) =zero?)
  
(define (=zero? x)
  (apply-generic 'zero x))

"=zero?"
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


