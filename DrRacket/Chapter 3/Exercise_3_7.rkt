#lang scheme

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  (define (protected-dispatch password m)
    (if (eq? password secret-password)
        (dispatch m)
        (lambda args "Incorrect password")))
  protected-dispatch)

(define (make-joint account secret-password joint-password)
  (lambda (password m)
    (if (eq? password joint-password)
        (apply account (list secret-password m))
        (lambda args "Incorrect joint account password"))))

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'deposit) 50)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 50)

; bad joint account password
((paul-acc 'rosebu 'withdraw) 50)

; bad main account password
(define jack-acc (make-joint peter-acc 'open-sesam 'rosebud))
((jack-acc 'rosebud 'withdraw) 50)