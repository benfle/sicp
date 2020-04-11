o#lang scheme

; Recusrive version of accumulate
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; Iterative version of accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Definition of sum using accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

; Definition of product using accumulate
(define (product term a next b)
  (accumulate * 1 term a next b))
