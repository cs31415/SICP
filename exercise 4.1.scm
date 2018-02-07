; Evaluate operands from left to right
(define (cons-lr first rest)
  (let ((p1 (cons first rest))
        (p2 (cons rest first)))
    (if (eq? first (car p1)) p1 p2)))

(cons-lr 1 2)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons-lr (eval (first-operand exps) env)
               (list-of-values (rest-operands exps) env))))

; Evaluate operands from right to left
(define (cons-rl first rest)
  (let ((p1 (cons first rest))
        (p2 (cons rest first)))
    (if (eq? first (car p1)) p2 p1)))

(cons-rl 1 2)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons-rl (eval (first-operand exps) env)
               (list-of-values (rest-operands exps) env))))
