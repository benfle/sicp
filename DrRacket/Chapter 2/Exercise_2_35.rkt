#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Using fring from Exercise 2.28 to enumerate the leaves of a tree
(define (fringe tree)
  (cond ((null? tree) null)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (fringe t))))

(define x (list 1 (list 2 3) (list 4 5)))
(count-leaves x)
