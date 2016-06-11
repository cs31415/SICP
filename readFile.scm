(define (get-employee-record fileName name)
  (call-with-input-file fileName
    (lambda (p)
      (let f ((x (read p)))
        (if (eof-object? x)
            '()
            (if (equal? name (car x))
                x
                (f (read p)))
            )))))

(get-employee-record "DivisionA.txt" '(Some Developer))
