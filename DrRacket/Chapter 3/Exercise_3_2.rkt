#lang scheme

(define (make-monitored f)
  (let ((counter 0))
    (define (call m)
      (set! counter (+ counter 1))
      (f m))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) counter)
            ((eq? m 'reset-count) (set! counter 0))
            (else (call m))))))

(define sqrt-monitored (make-monitored sqrt))

(sqrt-monitored 'how-many-calls?) ; 0
(sqrt-monitored 4)                ; 2
(sqrt-monitored 'how-many-calls?) ; 1
(sqrt-monitored 4)                ; 2
(sqrt-monitored 'how-many-calls?) ; 2
(sqrt-monitored 'reset-count)     ;
(sqrt-monitored 'how-many-calls?) ; 0
(sqrt-monitored 4)                ; 2
(sqrt-monitored 'how-many-calls?) ; 1
