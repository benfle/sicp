#lang scheme

(define (same-sign? a b)
  (or (and (>= a 0) (>= b 0)) (and (< a 0) (< b 0))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (same-sign? n d) + -)))
    (cons (/ (sign (abs n)) g) (/ (abs d) g))))

(make-rat 1 2)
(make-rat -1 2)
(make-rat 1 -2)
(make-rat 0 1)
(make-rat 0 -1)
(make-rat 1 0)
(make-rat -1 0)
