#lang scheme

(define (raise x) (apply-generic 'raise x))

(define (int2rat x)
  (make-rat x 1))
(put 'raise '(integer) int2rat)

(define (rat2real x)
  (make-real (/ (numerator x) (denominator x))))
(put 'raise '(rational) rat2real)

(define (real2complex x)
  (make-complex-from-real-imag x 0))
(put 'raise '(real) real2complex)
