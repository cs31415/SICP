; Note: Language needs to be Textual(MzScheme, includes R5RS)
(define (break) (display "break"))
(break)
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type) (lambda () #f)))

(define hc (make-hash-table 'equal))

(define (put-coercion t1 t2 proc)
  (hash-table-put! hc (list t1 t2) proc))

(define (get-coercion t1 t2)
  (hash-table-get hc (list t1 t2) (lambda () #f)))


(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (rational-number->scheme-number r) 
  (make-scheme-number (/ (numer r) (denom r))))
(put-coercion 'rational 'scheme-number
              rational-number->scheme-number)


; coerce all to 1st type, then to 2nd, then to 3rd, etc.
(define (apply-generic op . args)
  (define (try op args remainingargs)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (and (not (null? remainingargs)) 
                     (not (null? (car remainingargs))))
                (let ((cargs 
                       (map 
                        (lambda (x) 
                          (let* ((nextarg (car remainingargs))
                                 (typet (type-tag x))
                                 (types (type-tag nextarg)))
                            (if (equal? typet types)
                                x
                                (let 
                                    ((cproc (get-coercion typet types)))
                                  (if cproc
                                      (cproc x)
                                      x)))))
                        args)))
                  (try op cargs (cdr remainingargs)))
                (error "No method for these types"
                           (list op type-tags)))))))
  (try op args args))


(define (exp x y) (apply-generic 'exp x y))

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

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
  (put 'add '(rational scheme-number)
       (lambda (x y) (attach-tag 
                      'scheme-number 
                      (+ (/ (numer x) (denom x)) y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'mul '(rational scheme-number)
       (lambda (x y) (attach-tag 
                      'scheme-number 
                      (/(* (numer x) y)
                        (denom x)))))
    (put 'mul '(scheme-number rational)
       (lambda (x y) (attach-tag 
                      'scheme-number 
                      (if (= 0 (denom y))
                          (error "#Infinity")
                          (/ (* (numer y) x)
                             (denom y))))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'zero '(rational) =zero?)
 
  'done)

(install-rational-package)

; constructor/selectors for rational number
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

(define r1 (make-rational 1 3))
(numer r1)

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

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))


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

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


;(define (add x y) (apply-generic 'add x y))
(define (apply-generic-op op args) 
  (apply apply-generic (append (list op) args)))
(define (add . args) (apply-generic-op 'add args))
(define z1 (make-complex-from-real-imag 3 4))
(define s1 (make-scheme-number 4))
;(add z1 s1)
;(complex rectangular 7 . 4)

;(add s1 z1)
;(complex rectangular 7 . 4)

(define z2 (make-complex-from-real-imag 1 2))

;(add s1 z1 z2)
;(complex rectangular 8 . 6)

;(exp z1 z2)
; No method for these types (exp (complex complex))

(define r1 (make-rational 1 0))
(define s2 (make-scheme-number 3))
(define (mul . args) (apply-generic-op 'mul args))
;(break)
(mul s2 r1)
; -- calls mul (scheme-number rational)
;#Infinity 

;(mul r1 s2)
; if the arguments are in a different order, then 
; s2 is coerced to rational-number, the check for 
; denominator zero never happens and we get :
;/: division by zero
