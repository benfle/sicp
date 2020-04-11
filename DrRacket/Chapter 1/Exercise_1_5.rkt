#lang scheme

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 p)

; Applicative-order
;
; (test 0 (p))
; (test 0 (p))
; ...
;
; The program doesn't terminate. The expression (p) evaluates to itself.
;

; Normal-order
;
; (test 0 (p))
; 0
;
; The program returns 0 because the expression (p) is never evaluated.
;