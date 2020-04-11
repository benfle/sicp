#lang scheme

; For (random-integer) because (random) is limited to 2^31
(require (lib "27.ss" "srfi"))

; Results
; -------
;                                k=10        k=100
;
;  1e6     0.24 ms    0.15 ms    0.29 ms     2.8
;  1e7     0.76 ms    0.47 ms    0.36 ms     3.7
;  1e8     2.42 ms    1.45 ms    0.44 ms     4.4
;  1e9     7.70 ms    4.62 ms    0.48 ms     4.8
;  1e10  110.00 ms   51.60 ms    0.87 ms    68.0     (long -> double?)
;  1e11  380.00 ms  200.00 ms    1.04 ms    70.0
;  1e12 1160.00 ms  615.00 ms    1.14 ms    87.0


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
  (fast-prime? n 100))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (square x)
  (* x x))

(search-for-primes-after 1000000)          ; 1e6
(search-for-primes-after 10000000)         ; 1e7
(search-for-primes-after 100000000)        ; 1e8
(search-for-primes-after 1000000000)       ; 1e9
(search-for-primes-after 10000000000)      ; 1e10
(search-for-primes-after 100000000000)     ; 1e11
(search-for-primes-after 1000000000000)    ; 1e12
