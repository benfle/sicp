#lang scheme

; Implement an iterative version of the "fast" multiplication

(define (fast-* a b)
  (fast-*-iter a b 0))

; Invariant: ab + c
(define (fast-*-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (fast-*-iter (double a) (halve b) c))
        (else (fast-*-iter a (- b 1) (+ c a)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))
