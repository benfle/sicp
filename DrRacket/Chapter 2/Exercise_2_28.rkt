#lang scheme

(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (cond ((null? tree) null)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(fringe x)

(fringe (list x x))