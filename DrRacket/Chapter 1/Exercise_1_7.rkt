#lang scheme

; Compute the square root of a positive number by Newton's method of successive approximations

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

; (sqrt 0.0001)
; Returns 0.03230844833048122 but should have been 0.01 (An error of 223%)


; (sqrt 1e50)
; The program doesn't terminate because we cannot represent the number with the appropriate precision (0.001)

(define (better-sqrt x)
  (define (better-sqrt-iter guess previous-guess x)
    (if (better-good-enough? guess previous-guess)
        guess
        (better-sqrt-iter (improve guess x)
                          guess
                          x)))
  (define (better-good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.001))
  (better-sqrt-iter 1.0 0 x))
