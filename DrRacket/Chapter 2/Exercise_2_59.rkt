#lang scheme

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) (adjoin-set (car set2) (union-set set1 (cdr set2))))
        ((null? set2) (adjoin-set (car set1) (union-set (cdr set1) set2)))
        (else (adjoin-set (car set1) (adjoin-set (car set2) (union-set (cdr set1) (cdr set2)))))))

(union-set (list 1 2 3) (list 1 10 11))
