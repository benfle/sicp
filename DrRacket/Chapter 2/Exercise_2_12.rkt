#lang scheme

(require "Interval.rkt")

(define (make-center-percent c p)
  (let ((tolerance (* c (/ p 100))))
    (make-interval (- c tolerance) (+ c tolerance))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (/ (* 100 (width x)) (center x)))

(define x (make-interval 4.5 5.5))
(equals? (make-center-percent 5 10) x)
(= (percent x) 10)

