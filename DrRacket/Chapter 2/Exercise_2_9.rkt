#lang scheme

(require "Interval.rkt")

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

; Width of sum
; ------------
;
; width([x0, x1] + [y0, y1]) = 1/2 * ((x1 + y1) - (x0 + y0)) = 1/2 * (x1 - x0) + 1/2 * (y1 - y0) = width(x) + width(y)
;
; Width of difference
; -------------------
;
; width([x0, x1] - [y0, y1]) = 1/2 * ((x1 - y0) - (x0 - y1)) = 1/2 * (x1 - x0) + 1/2 * (y1 - y0) = width(x) + width(y)
;

(define a (make-interval 1.0 2.0)) ; width = 1/2
(define b (make-interval 3.0 4.0)) ; width = 1/2
(width (mul-interval a b))         ; width = 5/2
(width (mul-interval a a))         ; width = 3/2
(width (div-interval a b))         ; width = 0.2083333...
(width (div-interval a a))         ; width = 0.75
