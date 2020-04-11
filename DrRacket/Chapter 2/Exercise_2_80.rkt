#lang scheme

(define (=zero? x) (apply-generic '=zero? x))

(put '=zero? '(scheme-number) zero?)

(put '=zero? '(rational) (lambda (rat) (zero? (numer rat))))

(put '=zero? '(complex) (lambda (complex) (and (zero? (real-part complex))
                                               (zero? (imag-part complex)))))
