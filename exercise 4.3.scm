(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) 
         (make-procedure (lambda-parameters exp) 
                                       (lambda-body exp) 
                                       env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) 
         (apply (eval (operator exp) env) 
                                   (list-of-values 
                                    (operands exp) 
                                    env)))
        (else 
         (error "Unknown expression type -- EVAL" exp))))

(define true #t)
(define false #f)

; Self-evaluating expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; Variables
(define (variable? exp) (symbol? exp))

; Quoted expressions 'hello
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
  
; Assignment (set! x 4)
(define (assignment? exp)
  (tagged-list? exp 'set!))

; defines
(define (definition? exp)
  (tagged-list? exp 'define))

; if
(define (if? exp)
  (tagged-list? exp 'if))

; lambda
(define (lambda? exp)
  (tagged-list? exp 'lambda))

; begin
(define (begin? exp)
  (tagged-list? exp 'begin))

; cond
(define (cond? exp)
  (tagged-list? exp 'cond))

; application
(define (application? exp) (pair? exp))

(define (get-exp-type exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
        ((variable? exp) 'variable)
        ((quoted? exp) 'quoted)
        ((assignment? exp) 'assignment)
        ((definition? exp) 'definition)
        ((if? exp) 'if)
        ((lambda? exp) 'lambda)
        ((begin? exp) 'begin)
        ((cond? exp) 'cond)
        ((application? exp) 'application)
        (else 
         (error "Unknown expression type -- EVAL" exp))))

;(get-exp-type '(begin (+ x 2) (+ y 3)))
