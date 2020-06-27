(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))
;7

(define b (list (list 7)))
(car (car b))
;7

(define c (list 1 
                (list 2 
                      (list 3 
                            (list 4 
                                  (list 5 
                                        (list 6 7)))))))

(cdr (list 1 2))
; is equivalent to
(cdr (cons 1 (cons 2 null)))
; therefore, cdr returns (cons 2 null) or the list (2)
; i.e. cdr of a list returns a list of elements. 
; If the first element of that list is itself a list,
; we need to car the cdr to access that sublist.

(car (cdr 
      (car (cdr 
            (car (cdr 
                  (car (cdr 
                        (car (cdr 
                              (car (cdr c))))))))))))
;7


