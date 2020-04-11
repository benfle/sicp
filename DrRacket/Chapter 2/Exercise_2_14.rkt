#lang scheme

(require "Interval.rkt")
(require "Interval_with_percent.rkt")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-interval 100.0 101.0))
(define r2 (make-interval 100.0 101.0))

(par1 r1 r2)
(par2 r1 r2)

; To understand why we don't get the same result we can just
; compare (* R1 R1) / R1 and R1.

r1
(div-interval (mul-interval r1 r1) r1)

; The error margins in the second expression are accumulated
; and lead to a larger error margin at the end.
;
; We can verify that 1 / (1/R1 + 1/R2) is included in R1R2 / (R1 + R2).
;
; It doesn't mean that the second expression is wrong. It just means
; that it is less precise than the first one.
