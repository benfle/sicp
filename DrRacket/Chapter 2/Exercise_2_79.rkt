#lang scheme

(define (equ? x y) (apply-generic 'equ? x y))

(put 'equ? '(scheme-number scheme-number) =)

(put 'equ? '(rational rational) (lambda (a b) (and (= (numer a) (numer b))
                                                   (= (denom a) (denom b)))))

(put 'equ? '(complex complex) (lambda (a b) (and (= (real-part a) (real-part b)
                                                    (imag-part a) (imag-part b)))))
