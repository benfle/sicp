#lang scheme

; Point

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; Segment

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

; Return the midpoint of a segment

(define (midpoint-segment s)
  (average-point (start-segment s) (end-segment s)))

(define (average-point p1 p2)
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))

(define (average x y)
  (/ (+ x y) 2))

(define (print-point p)
  (newline)
  (display (x-point p))
  (display "@")
  (display (y-point p)))

(print-point (midpoint-segment (make-segment (make-point 2 3) (make-point 4 9))))
