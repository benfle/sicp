#lang scheme

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
; Same implementation, no performance improvement

(define (adjoin-set x set)
  (cons x set))
; We don't have to check if the element is already present so
; we improved the performance from O(n) to O(1)

(define (union-set set1 set2)
  (append set1 set2))
; Same as above. We don't have to check that each element of a set is in the other so
; we improve the performance from O(n^2) to O(n) (the elements of each set must be copied)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
; Same implementation, no performance improvement

; Due to the performance improvements for adjoin-set and union-set,
; this representation can be better for applications that
; do a lot of insertions in the set. 