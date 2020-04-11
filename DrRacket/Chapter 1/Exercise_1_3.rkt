
(define (square x) (* x x))

(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))

(define (sum-of-square-of-two-larger a b c)
  (+ (square (max a b) (max (min a b) c))))
