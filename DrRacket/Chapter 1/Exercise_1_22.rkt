#lang scheme

;
; Results
; -------
;
; Average time to test if a number is a prime by order of magnitude:
;
;  1e6     0.24 ms
;  1e7     0.76 ms (x  3.16 )
;  1e8     2.42 ms (x  3.18 )
;  1e9     7.70 ms (x  3.18 )
;  1e10  110.00 ms (x 14.28 )     Probably due to: 1e9 < 2^32 = 4 294 967 296 < 1e10. (long -> double)
;  1e11  380.00 ms (x  3.45 )
;  1e12 1160.00 ms (x  3.05 )
;
; The results are compatible with the time complexity of sqrt(n).
; Each order of magnitude add a factor of sqrt(10) = 3.16... to the time.
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
        (else (find-divisor n (+ test-divisor 1)))))

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



