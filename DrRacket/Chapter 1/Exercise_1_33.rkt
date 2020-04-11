#lang scheme

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a) (term a) null-value)))))
  (iter a null-value))

(define (sum-square-primes a b)
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (filtered-accumulate + 0 square a inc b prime?))

(define (prime? n)
  #t)

(define (product-relative-primes n)
  (define (inc x) (+ x 1))
  (define (id x) x)
  (define (relative-prime? x) (= (gcd x n) 1))
  (filtered-accumulate * 1 id 1 inc n relative-prime?))
