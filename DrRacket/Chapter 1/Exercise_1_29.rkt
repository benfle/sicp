#lang scheme

; Simpson's Rule for integral approximation

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (g k)
    (* (cond ((or (= k 0) (= k n)) 1.0)
             ((even? k) 2.0)
             (else 4.0))
       (y k)))
  (define (inc n) (+ n 1))
  (* (/ h 3) (sum g 0 inc n)))

; The integral approximation proposed in the book

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))
