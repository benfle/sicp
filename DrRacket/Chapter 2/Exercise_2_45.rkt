#lang scheme

(define (split op1 op2)
  (define (op painter n)
    (if (= n 0)
        painter
        (let ((smaller (op painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  op)

(define corner-split (split beside below))
(define up-split (split below beside))
