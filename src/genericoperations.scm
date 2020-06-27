;...............................................................
; Defines for generic operations
;...............................................................
(define h (make-hash-table 'equal))

(define (put op type proc)
  (hash-table-put! h (list op type) proc))

(define (get op type)
  (hash-table-get h (list op type) (lambda () #f)))

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op (if (null? (cdr type-tags))
                            (car type-tags)
                            type-tags))))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
