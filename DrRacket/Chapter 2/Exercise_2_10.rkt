#lang scheme

(require "Interval.rkt")

; If we divide by an interval of the form [0, a] ou [a, 0], we will get a division by zero error
; We implement a check to raises a similar error if the denominator spans 0.

(define (spans-zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

(define (div-interval-zero-check x y)
  (if (spans-zero? y)
      (error "Division by zero" y)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

(define a (make-interval 1.0 2.0))

;(div-interval-zero-check a (make-interval -2.0 -1.0))
;(div-interval-zero-check a (make-interval -1.0  0.0))
;(div-interval-zero-check a (make-interval  0.0  0.0))
;(div-interval-zero-check a (make-interval  0.0  1.0))
;(div-interval-zero-check a (make-interval  1.0  2.0))

