#lang scheme

(define x (list (list 1 2) (list 3 4) 5))

(define (deep-reverse list)
  (define (iter list acc)
    (if (null? list)
        acc
        (iter (cdr list) (cons (deep-reverse (car list)) acc))))
  (if (pair? list)
      (iter list null)
      list))

(deep-reverse x)
