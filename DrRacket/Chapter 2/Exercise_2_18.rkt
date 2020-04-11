#lang scheme

(define (reverse list)
  (define (iter list acc)
    (if (null? list)
        acc
        (iter (cdr list) (cons (car list) acc))))
  (iter list null))

(reverse (list 1 2 3))
