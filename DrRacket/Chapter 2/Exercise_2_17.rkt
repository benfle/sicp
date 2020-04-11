#lang scheme

(define (last-pair list)
  (if (null? (cdr list))
      (cons (car list) null)
      (last-pair (cdr list))))

(last-pair (list 23 72 149 34))
