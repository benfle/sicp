#lang scheme

; Ackerman function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;(A 1 10)
;(A 2 4)
;(A 3 3)

; f(n) = 2 times n
(define (f n) (A 0 n))

; g(n) = 2 to the power n if n > 0, 0 if n = 0
(define (g n) (A 1 n))

; h(n) = 2 to the power (2 to the power (n - 1)) if n > 0, 0 if n = 0
(define (h n) (A 2 n))

; k(n) = 5 times n to the square
(define (k n) (* 5 n n))

(define (samples f n)
  (cond ((< n 0) 0)
        (else (display (f n))
              (newline)
              (samples f (- n 1)))))
  
(samples h 5)
