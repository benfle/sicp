#lang scheme

(define rand
  (let ((x 0))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset y)
      (set! x y))
    (lambda (m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Unknown request -- RAND" m))))))

(define (rand-update x)
  (+ x 1))

(rand 'generate)  ; 1
(rand 'generate)  ; 2
((rand 'reset) 5)
(rand 'generate)  ; 6