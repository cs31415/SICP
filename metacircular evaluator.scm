(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) 
                                       (lambda-body exp) 
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env) 
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


;-----------------------------------------------------------
; Evaluation logic
;-----------------------------------------------------------
; quoted
(define (text-of-quotation exp) (cadr exp))

; assignment
(define (eval-assignment exp env)
  (set-variable-value? (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; define (variable definitions)
(define (eval-definition exp env)
  (define-variable? (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; can be of the forms:
; (define <var> <value>)
; (define (<var> <param-1> ... <param-n>)
;   <body>)
; Procedure define is syntactic sugar for:
; (define <var>
;   (lambda (param-1 ... param-n)
;     <body>))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; for variable definition
      (caadr exp))) ; for procedure definition

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (cddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

; if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; true? is required because the if-predicate is evaluated in the 
; language being implemented and yields a value in that language.
; The interpreter predicate true? translates this value into a value
; that can be tested by the if in the implementation language.

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

; if the alternative is not provided then false is returned if the
; predicate evaluates to false
(define (if-alternative exp)
  (if (not (null? cdddr))
      (cadddr exp)
      'false))

; lambda
(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; begin (sequences)
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

; cond
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; constructor used by cond->if to transform a sequence into a 
; single expression
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

; procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

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

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; Derived expressions (some special forms can be expressed in terms
; of other special forms. e.g. a cond expression can be stated in 
; terms of an if expression)
;(cond ((> x 0) x)
;      ((= x 0) (display 'zero) 0)
;      (else (- x)))

;can be restated as:
;(if (> x 0)
;    x
;    (if (= x 0)
;        (begin 
;          (display 'zero)
;          0)
;        (- x)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (expand-clauses clauses)
  (if (null? clauses) 
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" 
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;-----------------------------------------------------------
; Dispatching Logic
;-----------------------------------------------------------

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