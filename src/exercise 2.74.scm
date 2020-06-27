;a. Implement get-record

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (get-record name file) 
  (apply-generic 'employee-record (attach-tag file name)))
(define (get-name employee-record) 
  (apply-generic 'name employee-record))
(define (get-salary employee-record) 
  (apply-generic 'salary employee-record))
(define (get-address employee-record) 
  (apply-generic 'address employee-record))


; divisionA's file and package
;((Chandra Sivaraman) (1 Funny St, Funny Valley CA 99999) 100000)
;((Some Developer) (123 Any St, SomeTown, ST 90000) 80000)
;((Bill G) (100 Microsoft Way, Redmond, WA 10000) 1000000)

(define (install-divisionA-package)
  (define file "divisionA.txt")
  (define (get-employee-record name)
  (call-with-input-file file
    (lambda (p)
      (let f ((x (read p)))
        (if (eof-object? x)
            '()
            (if (equal? name (car x))
                (attach-tag file x)
                (f (read p)))
            )))))
  (define (get-name employee-record)
    (car employee-record))
  (define (get-address employee-record)
    (cadr employee-record))
  (define (get-salary employee-record)
    (cddr employee-record))
  (put 'name file get-name)
  (put 'address file get-address)
  (put 'salary' file get-salary)
  (put 'employee-record file get-employee-record)
  'done)


;b. Implement get-salary
; see above

;c. Implement find-employee-record

(define (find-employee-record name files)
  (if (or (null? files) (null? (car files)))
      '(Employee record not found!)
      (let ((record (get-record name (car file))))
        (if (not (null? record))
            record
            (find-employee-record name (cdr files))
        ))))


;d. They must create an install procedure like the one above
; that returns division specific selectors, and the new company's
; files must be added to the master list.


  
         