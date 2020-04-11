#lang scheme

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (id n) n)
  (define (inc n) (+ n 1))
  (product id 1 inc n))

(define (pi-product n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (pi-next n) (+ n 1))
  (* 4.0 (product pi-term 1 pi-next n)))

; Recursive version of product
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))
