#lang scheme

;
; Results
; -------
;
;  Effect on average time when iterating over odd numbers instead of all:
;
;  1e6     0.24 ms    0.15 ms (x 1.6)
;  1e7     0.76 ms    0.47 ms (x 1.6)
;  1e8     2.42 ms    1.45 ms (x 1.6)
;  1e9     7.70 ms    4.62 ms (x 1.6)
;  1e10  110.00 ms   51.60 ms (x 2.13)
;  1e11  380.00 ms  200.00 ms (x 1.9)
;  1e12 1160.00 ms  615.00 ms (x 1.88)
;
; The results seem to confirm that the algorithm is running almost twice as fast.
; The difference might be due to the fact that we're testing everytime if the number is 2.
; 

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
  (define start-time (runtime))
  (unless (not (prime? n))
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes-after a)
  (search-for-primes a (+ a 100)))

(define (search-for-primes a b)
  (define (search-for-primes-iter n)
    (cond ((<= n b)
           (timed-prime-test n)
           (search-for-primes-iter (+ n 2)))))
  (search-for-primes-iter (if (even? a) (+ a 1) a)))

(define (even? n)
  (= (remainder n 2) 0))

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

(search-for-primes-after 1000000)          ; 1e6
(search-for-primes-after 10000000)         ; 1e7
(search-for-primes-after 100000000)        ; 1e8
(search-for-primes-after 1000000000)       ; 1e9
(search-for-primes-after 10000000000)      ; 1e10
(search-for-primes-after 100000000000)     ; 1e11
(search-for-primes-after 1000000000000)    ; 1e12