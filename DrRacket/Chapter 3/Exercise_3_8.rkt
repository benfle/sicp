#lang scheme

(define (make-f)
  (let ((value null))
    (lambda (n)
      (cond ((null? value) (set! value n) n)
            (else 0)))))

(define f (make-f))
(+ (f 0) (f 1))

(define g (make-f))
(+ (g 1) (g 0))
