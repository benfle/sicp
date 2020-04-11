#lang scheme

; Procedures from 1.43
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))

; Average of 3 numbers
(define (average a b c)
  (/ (+ a b c) 3))

; Small delta
(define dx 0.1)

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-smooth f n)
  (repeated (smooth f) n))

