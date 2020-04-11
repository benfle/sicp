#lang scheme

; I actually did a little more than what the exercise was asking for. I wanted to find camichael numbers myself.
; Unfortunately the find-carmichael-before procedure has a time complexity of n^2 x log(n) so I stopped at 1e4.
;
; Camichael numbers before 1e4: 561, 1105, 1729, 2465, 2821, 6601, 8911
;

(define (find-carmichael-before n)
  (define (find-carmichael-before-iter n k)
    (cond ((test-carmichael k) (newline) (display k)))
    (unless (= k n) (find-carmichael-before-iter n (+ k 1))))
  (find-carmichael-before-iter n 1))
          
(define (test-carmichael n)
  (define (test-carmichael-iter n a res)
    (cond ((not res) res)
          ((= a n) (not (prime? n)))
          (else (test-carmichael-iter n (+ a 1) (= (expmod a n n) a)))))
  (if (> n 2)
      (test-carmichael-iter n 2 #t)
      #f))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))
