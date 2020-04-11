#lang scheme

(define (cube-root x)
  (define (cube-root-iter guess previous-guess x)
    (if (good-enough? guess previous-guess)
        guess
        (cube-root-iter (improve guess x) guess x)))
  (define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.001))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (square x)
    (* x x))
  (cube-root-iter 1.0 0 x))

(cube-root 27)
