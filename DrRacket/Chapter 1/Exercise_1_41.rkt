#lang scheme

(define (double p)
  (lambda (x) (p (p x))))

(define (inc n) (+ n 1))

; (double double) is a procedure that takes another procedure as argument and returns a procedure that applies the original procedure 4 times
; (double (double double)) is a procedure that takes another procedure as argument and returns a procedure that applies the original procedure 4 x 4 = 16 times
; So the expression below should evaluate to 5 + 16 = 21

(((double (double double)) inc) 5)
