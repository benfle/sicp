#lang scheme

; We know that phi^2 = 1 + phi
; If we divide by phi on both side: phi = 1/phi + 1 = f(phi)
;
; So phi is a fixed-point of f: x -> 1 + 1/x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define phi (/ (+ 1 (sqrt 5)) 2))
phi

