#lang scheme

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

; count how many times b contains the factor a
(define (count-factor b a)
  (define (iter n c)
    (if (= (remainder c a) 0)
        (iter (+ n 1) (/ c a))
        n))
  (iter 0 b))

(define (car c)
  (count-factor c 2))

(define (cdr c)
  (count-factor c 3))

(car (cons 9 12))
(cdr (cons 9 12))
