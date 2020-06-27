(define MAX-ATTEMPTS 7)
(define (make-account balance acctPasswd)
  (let ((attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (lambda (x) "Calling the cops"))
    (define (dispatch passwdInput m)
      (cond ((not (eq? passwdInput acctPasswd)) 
             (begin
               (set! attempts (+ 1 attempts))
               (print attempts)
               (if (> attempts MAX-ATTEMPTS)
                   (call-the-cops)
                   (lambda (x) "Incorrect password"))))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
;1"Incorrect password"
;2"Incorrect password"
;3"Incorrect password"
;4"Incorrect password"
;5"Incorrect password"
;6"Incorrect password"
;7"Incorrect password"
;8"Calling the cops"

