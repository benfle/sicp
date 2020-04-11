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
  (let ((failed-attempts 0))
    (define (call-the-cops . args)
      "You're in trouble!")
    (define (protected-dispatch password m)
      (if (eq? password secret-password)
          (begin
            (set! failed-attempts 0)
            (dispatch m))
          (if (= failed-attempts 6)
              call-the-cops
              (begin
                (set! failed-attempts (+ failed-attempts 1))
                (lambda args "Incorrect password")))))
    protected-dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
