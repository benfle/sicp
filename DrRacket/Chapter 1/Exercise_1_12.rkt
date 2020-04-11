#lang scheme

; Recursive computation of Pascal's triangle

(define (pascal i j)
  (cond ((or (> j i) (or (< i 0) (< j 0))) 0)
        ((or (= j 0) (= j i)) 1)
        (else (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))

