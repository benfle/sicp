#lang scheme

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(define (approx-e k)
  (+ 2 (cont-frac (lambda (x) 1.0)
           (lambda (x) (if (= (modulo x 3) 2)
                           (/ (* 2 (+ x 1)) 3)
                           1))
           10)))

(approx-e 10)