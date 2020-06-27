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
(define (has-type-tag? datum)
  (if (and (pair? datum)
           (not (null? (car datum)))
           (not (number? (car datum))))
      #t
      #f))
(define (contents-recursive datum)
  (if (has-type-tag? datum)
      (let ((tt (type-tag datum)))
        (cond ((equal? tt 'rectangular) datum)
              ((equal? tt 'polar) datum)
              ((equal? tt 'rational) (/ (numer (contents datum))
                                        (denom (contents datum))))
              (else (contents-recursive (contents datum)))))
      datum))
 
; We might have to add a type layer on top of 
; scheme-number just like we did for complex types
(define (get-type x)
  (let ((type (type-tag x)))
    (if (equal? type 'scheme-number) 
        (if (integer? (contents x)) 'integer 'real)
        type)))
 
 
;.................................................................
; Define a Type
;.................................................................
(define (make-typerec typename raisefunc projectfunc) 
  (list typename raisefunc projectfunc))
(define (typename typerec)
  (car typerec))
(define (raiseFunc typerec)
  (cadr typerec))
(define (projectFunc typerec)
  (caddr typerec))
 
;.................................................................
; Drop
;.................................................................
; drop an object down the tower as far as possible
(define (drop x)
  (let ((y (project x)))
    (if (or (equal? (typename x) (typename y))
            (not (equal? x (raise y))))
        x 
        (drop y))))
 
;.................................................................
; Setup type tower
; dense > sparse
;.................................................................
; iterate type-tower and setup type functions
(define (setup-tower type-tower)
  (map (lambda (typerec)
         (let ((fr (raiseFunc typerec))
               (fp (projectFunc typerec)))
           (begin
            (if (not (null? fr))
               (put 'raise (typename typerec) fr))
            (if (not (null? fp))
                (put 'project (typename typerec) fp)))))
       type-tower))

 
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
 
;.................................................................
; Coercion
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
; Generic Apply function using drop
;.................................................................
(define (apply-generic op . args)
  (define (try op args remainingargs)
    (let ((type-tags (map typename args)))
      (let ((proc (get op (if (null? (cdr type-tags))
                            (car type-tags)
                            type-tags))))
        (if proc
            (let ((rslt (apply proc (map contents args))))
              ; This causes a circular reference because drop 
              ; calls project which calls apply-generic which 
              ; calls drop again. Hence the check for op = project
              (if (and (not (equal? op 'project)) 
                       (not (equal? op 'raise))
                       (has-type-tag? rslt))
                  (drop rslt)
                  rslt))
            (if (and (not (null? remainingargs)) 
                     (not (null? (car remainingargs))))
                (let* ((nextarg (car remainingargs))
                       (typet (typename nextarg))
                       (cargs 
                        (map
                         (lambda (x) 
                           (get-coercion x typet))
                         args)))
                  (try op cargs (cdr remainingargs)))
                (error "No method for these types"
                           (list op type-tags)))))))
  (try op args args))

(define (raise x)
  (apply-generic 'raise x))


(define (project x)
  (apply-generic 'project x))
