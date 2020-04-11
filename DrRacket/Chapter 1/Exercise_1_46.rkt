#lang scheme

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))
  try)

(define (sqrt x)
  ((iterative-improve (lambda (guess next) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   1.0))

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))

(sqrt 9)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (v1 v2) (< (abs (- v1 v2)) 0.00001))
                      (lambda (x) (f x)))
   first-guess))
