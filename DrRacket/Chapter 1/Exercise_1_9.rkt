#lang scheme

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

; This is the recursive version of +

(define (+-rec a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; This is the iterative version of +

(define (+-iter a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
