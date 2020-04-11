#lang scheme

; For (random-integer) because (random) is limited to 2^31
(require (lib "27.ss" "srfi"))

; Miller-Rabin test
;

(define (prime? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (check (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (check x m)
  (if (and (not (= x 1))
           (not (= x (- m 1)))
           (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))

(define (square x)
  (* x x))
