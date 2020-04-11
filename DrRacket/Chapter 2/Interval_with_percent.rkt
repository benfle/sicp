#lang scheme

(require "Interval.rkt")

(provide make-center-percent center percent)

(define (make-center-percent c p)
  (let ((tolerance (* c (/ p 100))))
    (make-interval (- c tolerance) (+ c tolerance))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (/ (* 100 (width x)) (center x)))
