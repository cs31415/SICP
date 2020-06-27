; Note: Language needs to be Textual(MzScheme, includes R5RS)

; Hash table definitions
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type)))

; Definitions for complex numbers in rectangular form 
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
  (let ((type-tags (map type-tag args))) ; for multiple type tags
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; rectangular/polar layer dispatch
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


; Definitions for handling complex numbers
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
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
  
  'done)

(install-complex-package)

; constructors for complex numbers
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Complex layer dispatch
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define z (make-complex-from-real-imag 3 4))
(magnitude z)

; execution path (using substitution model)
(apply-generic 'magnitude 
               ((get 'make-from-real-imag 'complex) 3 4))
(apply-generic 'magnitude 
               ((lambda (x y) 
                  (tag (make-from-real-imag x y))) 3 4))
(apply-generic 'magnitude 
               ((lambda (x y) 
                  (cons 'complex 
                        (make-from-real-imag x y))) 3 4))
(apply-generic 'magnitude 
               (cons 'complex 
                     (make-from-real-imag 3 4)))
(apply-generic 'magnitude 
               (cons 'complex 
                     ((get 'make-from-real-imag 
                           'rectangular) 3 4)))
(apply-generic 'magnitude 
               (cons 
                'complex 
                ((lambda (x y) 
                   (tag (make-from-real-imag x y))) 3 4)))
(apply-generic 'magnitude 
               (cons 
                'complex 
                ((lambda (x y) 
                   (cons 'rectangular 
                         (make-from-real-imag x y))) 3 4)))
(apply-generic 'magnitude 
               (cons 
                'complex 
                (cons 'rectangular (make-from-real-imag 3 4))))
(apply-generic 'magnitude 
               (cons 
                'complex 
                (cons 'rectangular (cons 3 4))))
(apply-generic 'magnitude 
               (cons 
                'complex 
                (cons 'rectangular (cons 3 4))))
(apply-generic 'magnitude 
               (cons 'complex (cons 'rectangular (cons 3 4))))
(apply-generic 'magnitude 
               (cons 'complex (cons 'rectangular (cons 3 4))))
(magnitude (cons 'rectangular (cons 3 4)))
(apply-generic 'magnitude (cons 'rectangular (cons 3 4)))
; the real-part and imag-part are from rectangular package
(sqrt (+ (square (real-part (cons 3 4)))
             (square (imag-part (cons 3 4)))))
(sqrt (+ (square (car (cons 3 4)))
             (square (cdr (cons 3 4)))))
(sqrt (+ (square 3) (square 4)))
(sqrt (+ 9 16))
5

; apply-generic is called twice. Once with keys ('magnitude 'complex)
; dispatching to the magnitude of complex:
; (define (magnitude z) (apply-generic 'magnitude z))
; and again with keys ('magnitude 'rectangular) dispatching to the
; magnitude of rectangular:
; (define (magnitude z)
;    (sqrt (+ (square (real-part z))
;             (square (imag-part z)))))