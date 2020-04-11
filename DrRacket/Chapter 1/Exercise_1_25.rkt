#lang scheme

; The problem with Alyssa's solution is that the exponential is actually calculated
; which makes the program run out of memory for 2^1e9 for example.
;
; The advantage of the solution proposed in the book (as explained in footnote 46) is that
; by breaking down the computation we don't have to deal with integers much larger than m.


; base^exp % m as described in 1.2.6

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

; base^exp % m as proposed by Alyssa P. Hacker

(define (expmod-alyssa base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt a b)
  (cond ((= b 0) 1)
        ((even? b) (square (fast-expt a (/ b 2))))
        (else (* a (fast-expt a (- b 1))))))

(define (square x)
  (* x x))


