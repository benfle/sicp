#lang scheme

(define (adjoin-set x set1)
  (if (null? set1)
      (list x)
      (cond ((> x (car set1)) (cons (car set1) (adjoin-set x (cdr set1))))
            ((= x (car set1)) set1)
            (else (cons x set1)))))

; Testing

(adjoin-set 1 '())
(adjoin-set 1 '(2 3))
(adjoin-set 2 '(1 3))
(adjoin-set 3 '(1 2))
(adjoin-set 2 '(1 2 3))

              