#lang scheme

(define (same-parity? . l)
  (or (null? l) (all? odd? l) (all? even? l)))

(define (all? test l)
  (or (null? l) (and (test (car l)) (all? test (cdr l)))))

(same-parity?)
(same-parity? 1 3)
(same-parity? 1 2)
(same-parity? 2 4 6 8)