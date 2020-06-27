(define (same-parity . z)
  (define first-parity (parity (if (not (null? z)) (car z) null)))
  (define (same-parity-iter y)
    (if (null? y)
        null
        (if (= (parity (car y)) first-parity)
          (cons (car y) (same-parity-iter (cdr y)))
          (same-parity-iter (cdr y)))))
  (same-parity-iter z))


(define (parity x)
  (if (and (not (null? x)) (odd? x)) 1 0))

(same-parity 1 2 3 4 5 6 7)
;(1 3 5 7)

(same-parity 2 3 4 5 6 7)
;(2 4 6)

(same-parity)
;()


