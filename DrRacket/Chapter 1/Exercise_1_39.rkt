#lang scheme

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

; Lambert's formula to approximate tan(x)
(define (tan-cf x k)
  (cont-frac (lambda (n) (if (= n 1) x (- (square x))))
             (lambda (n) (- (* 2.0 n) 1))
             k))

(define (square x) (* x x))

(tan-cf (/ pi 4) 5)
