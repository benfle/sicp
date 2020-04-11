#lang scheme

; T(p, q)^2 = ( a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, b <- (bp + aq)p + (bq + aq + ap)q )
; T(p, q)^2 = ( a <- b (2qp + q^2) + a (2qp + q^2) + a (p^2 + q^2), b <- b (p^2 + q^2) + a (2qp + q^2)
; T(p, q)^2 = T(p', q') with p' = (p^2 + q^2) and q' = (2qp + q^2)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a b (+ (square p) (square q)) (+ (* 2 q p) (square q)) (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x)
  (* x x))


