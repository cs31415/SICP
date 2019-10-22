(define (f) 
  (let ((x "(define (f)\n  (let ((x ~s))\n    (display (format x x))))\n(f)"))
    (display (format x x))))
(f)