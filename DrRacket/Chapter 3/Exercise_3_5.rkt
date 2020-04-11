#lang scheme

; Mont Carlo Integration

(define (estimate-integral p x1 y1 x2 y2 trials)
  (monte-carlo trials (integral-experiment p x1 y1 x2 y2)))

(define (integral-experiment p x1 y1 x2 y2)
  (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (circle-predicate cx cy r)
  (lambda (x y)
    (<= (+ (square (- x cx)) (square (- y cy))) (square r))))

(define (square x) (* x x))

(define (estimate-pi trials)
  (let ((r 0.5) (cx 1) (cy 1))
    (/ (estimate-integral (circle-predicate cx cy r) (- cx r) (- cy r) (+ cx r) (+ cy r) trials) (square r))))

(estimate-pi 100000)
