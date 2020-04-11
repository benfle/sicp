#lang scheme

; a + |b| = a + b if b > 0
;           a - b if b < 0
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
