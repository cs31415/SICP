;.................................................................
;For debugging
;.................................................................
(define (break) 
  (begin 
    (display "break")
    (newline)))
  
(break)

;.................................................................
; Hashtable for generic apply
;.................................................................
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type) (lambda () #f)))

;.................................................................
; Type tag definitions
;.................................................................
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

; We might have to add a type layer on top of 
; scheme-number just like we did for complex types
(define (get-type x)
  (let ((type (type-tag x)))
    (if (equal? type 'scheme-number) 
        (if (integer? (contents x)) 'integer 'real)
        type)))


;.................................................................
; Setup type tower
; integer > rational > real > complex
;.................................................................
; Raise definitions
; integer
(define (integer->rational i)
  (let ((x (if (equal? (type-tag i)
                       'scheme-number)
               (contents i)
               i)))
  (make-rational x 1)))

; rational
(define (rational->real rationalnumber)
  (let ((x (if (equal? (type-tag rationalnumber)
                       'rational)
               (contents rationalnumber)
               rationalnumber)))
    (make-real (/ (* 1.0 (numer x)) 
                  (denom x)))))
  
; real
(define (real->complex r)
  (let ((x (if (equal? (type-tag r)
                       'scheme-number)
               (contents r)
               r)))
  (make-complex-from-real-imag x 0)))

(define (make-typerec typename raisefunc) 
  (cons typename raisefunc))
(define (typename typerec)
  (car typerec))
(define (raiseFunc typerec)
  (cdr typerec))
(define type-tower  
  (list (make-typerec 
         'integer integer->rational)
        (make-typerec 
         'rational rational->real)
        (make-typerec 
         'real real->complex)
        (make-typerec 
         'complex (lambda (x) (attach-tag 'complex x)))))


; iterate type-tower and setup coercion functions
(define (setup-tower)
  (map (lambda (typerec)
         (let ((func (raiseFunc typerec)))
           (if (not (null? func))
               (put 'raise (list (typename typerec)) func))))
       type-tower))
(setup-tower)

; which type is higher in the tower?
(define (ishigher? typename1 typename2)
  (define (try remainingtypes)
    (if (and (not (null? remainingtypes))
             (not (null? (car remainingtypes))))
        (let ((nexttypename (typename (car remainingtypes))))
          (if (equal? nexttypename typename1)
            #f
            (if (equal? nexttypename typename2)
                #t
                (try (cdr remainingtypes)))))        
        #f)
    )
  (try type-tower))

"ishigher"
(ishigher? 'complex 'rational)
;#t
(ishigher? 'integer 'complex)
;#f
(ishigher? 'real 'complex)
;#f
(ishigher? 'rational 'integer)
;#t
(ishigher? 'real 'rational)
;#t

;.................................................................
; Coercion definitions
; Note: Language needs to be Textual(MzScheme, includes R5RS)
;.................................................................
; hash table for coercion functions
(define hc (make-hash-table 'equal))
(define (putc op type proc)
  (hash-table-put! h (list op type) proc))

(define (getc op type)
  (hash-table-get h (list op type) (lambda () #f)))

; Here we need to check if targettypename is higher than x, 
; only then attempt coercion else return the input value
(define (get-coercion x targettypename)
  (if (equal? (get-type x) targettypename)
      x
      (if (ishigher? targettypename (get-type x))
          (let ((rx (raise x)))
            (get-coercion rx targettypename))
          x)))
  

;.................................................................
; Generic Apply function
; coerce all to 1st type, then to 2nd, then to 3rd, etc.
;.................................................................
(define (apply-generic op . args)
  (define (try op args remainingargs)
    (let ((type-tags (map typename args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (and (not (null? remainingargs)) 
                     (not (null? (car remainingargs))))
                (let ((cargs 
                       (map 
                        (lambda (x) 
                          (let* ((nextarg (car remainingargs))
                                 (typet (typename nextarg)))
                            (get-coercion x typet)))
                        args)))
                  (try op cargs (cdr remainingargs)))
                (error "No method for these types"
                           (list op type-tags)))))))
;  (break)
  (try op args args))
  
  
(define (raise x)
  (begin
;    (display "x = ")
;    (display x)
;    (newline)
;    (display "type(x) = ")
;    (display (get-type x))
;    (newline)
  (apply-generic 'raise x)))

(define (square x) (* x x))

;.................................................................
; Scheme number package
;.................................................................
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

;.................................................................
; Extensions to scheme number package to support integer and real
;.................................................................
(define (install-scheme-number-package-ext subtype)
  (put 'zero (list subtype) (lambda (x) (=zero? x)))
  (put 'add (list subtype subtype) 
       (lambda (x y) (add x y)))
  (put 'sub (list subtype subtype)
       (lambda (x y) (sub x y)))
  (put 'mul (list subtype subtype)
       (lambda (x y) (mul x y)))
  (put 'div (list subtype subtype)
       (lambda (x y) (div x y)))
  'done)

;.................................................................
; Integer package
;.................................................................
(define (make-integer i)
  (attach-tag 'integer (make-scheme-number i)))
(install-scheme-number-package-ext 'integer)

;.................................................................
; Real package
;.................................................................
(define (make-real r)
  (attach-tag 'real (make-scheme-number r)))
(install-scheme-number-package-ext 'real)

;.................................................................
; Rational number package
;.................................................................
;(define (numer x) (apply-generic 'numer x))
;(define (denom x) (apply-generic 'denom x))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (install-rational-package)
  ;; internal procedures
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
  (put 'mul '(rational scheme-number)
       (lambda (x y) (attach-tag 
                      'scheme-number 
                      (/(* (numer x) y)
                        (denom x)))))
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

;.................................................................
; Rectangular package
;.................................................................
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

;.................................................................
; Complex package
;.................................................................
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

; Convert the op to (list op) since the map args in apply-generic
; generates an op list
(define (apply-generic-op op args) 
  (apply apply-generic (append (list op) args)))
(define (add . args) 
  (begin
    (break)
    (apply-generic-op 'add args)))
(define (=zero? x)
  (apply-generic 'zero x))

;.................................................................
; Testing
;.................................................................
(define x (make-integer 10))
x
;(integer scheme-number . 10)
(raise x)
;(rational 10 . 1)
(raise (raise x))
;(real scheme-number . 10.0)
(raise (raise (raise x)))
;(complex rectangular 10.0 . 0)
(raise (raise (raise (raise x))))
;(complex rectangular 10.0 . 0)

(define r1 (make-rational 1 3))
(define s1 (make-real 3.1))
s1
;(real scheme-number . 3.1)
(add r1 s1)
;(scheme-number . 3.4333333333333336)
(add s1 r1)
;(scheme-number . 3.4333333333333336)
(=zero? (make-real 0))
;#t
(=zero? s1)
;#f
(define i1 (make-integer 10))
i1
;(integer scheme-number . 10)
(define c1 (make-complex-from-real-imag 2 3))
(add i1 c1)
;(complex rectangular 12.0 . 3)

