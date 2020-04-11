#lang scheme

(define (equal? a b)
  (if (pair? a)
      (if (pair? b)
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b)))
          #f)
      (eq? a b)))

; Testing

(eq? (equal? 1 1) #t)
(eq? (equal? 1 2) #f)
(eq? (equal? 1 (list 1)) #f)
(eq? (equal? (list 1) 1) #f)
(eq? (equal? (list 1) (list 1)) #t)
(eq? (equal? (list 1) (list 1 2)) #f)
(eq? (equal? (list 1 2) (list 1 2)) #t)
(eq? (equal? (list 1 (list 2)) (list 1 (list 2))) #t)

