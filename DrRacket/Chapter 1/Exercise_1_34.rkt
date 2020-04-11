#lang scheme

(define (f g)
  (g 2))

(define (square x) (+ x x))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)

; (f f) = (f 2) = (2 2) but 2 is not a procedure so the call fails.