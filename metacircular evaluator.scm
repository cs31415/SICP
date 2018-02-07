(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

; apply
; primitive procedures are those that have to be implemented by the implementation language
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Uknown procedure type -- APPLY" procedure))))

; if

; sequences

; assignment

; define


; Self-evaluating expressions
(define (self-evaluating? exp)
  (cond (((number? exp) true)
         ((string? exp) true)
         (else false))))

; Variables
(define (variable? exp) (symbol? exp))


; Quoted expressions 'hello
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
  
(define (text-of-quotation exp) (cadr exp))

; Assignment (set! x 4)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


; if



; lambda



; begin



; cond



; application