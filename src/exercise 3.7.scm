(define (make-account balance acctPasswd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passwdInput m)
    (cond ((not (eq? passwdInput acctPasswd)) 
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint acct acctPasswd newAcctPasswd)
  (define (dispatch passwdInput m)
    (cond ((not (eq? passwdInput newAcctPasswd))
           (lambda (x) "Incorrect password"))
          (else (acct acctPasswd m))))
  dispatch)
  

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60

(define jacc (make-joint acc 'secret-password 'joint-password))
((jacc 'joint-password 'deposit) 10)
;70

((jacc 'some-other-password 'deposit) 50)
;"Incorrect password"

((jacc 'joint-password 'withdraw) 50)
;20

((acc 'joint-password 'withdraw) 40)
;"Incorrect password"

((acc 'secret-password 'withdraw) 10)
;10

