; a.
; define and set! are special forms; If they are not
; processed before procedure application, the application?
; dispatch check will treat them like procedures and 
; attempt to apply the 'procedures' define or set! to the
; arguments, which will result in syntax errors.

; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define exp '(define x 1))
(application? exp)
;#t,
(operator exp)
;define
(operands exp)
;(x 1)


; b. 
; If the syntax of the evaluator is changed so that 
; procedure applications start with call, the application? 
; check can be modified as follows:

(define (application? exp) 
  (tagged-list? exp 'call))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(application? exp)
;#f