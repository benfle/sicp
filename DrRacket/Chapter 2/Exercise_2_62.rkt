#lang scheme

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) (cons (car set2) (union-set set1 (cdr set2))))
        ((null? set2) (cons (car set1) (union-set (cdr set1) set2)))
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      (else (cons x2 (union-set set1 (cdr set2)))))))))

; Testing

(union-set '(1 3 6) '(1 2 3))
(union-set '() '(1 2 3))
(union-set '(1 2 3) '())
(union-set '(1 2) '(3 4 5))

