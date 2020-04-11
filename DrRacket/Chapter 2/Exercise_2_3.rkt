#lang scheme

; Point

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (sub p1 p2)
  (make-point (- (x-point p1) (x-point p2))
              (- (y-point p1) (y-point p2))))

; Rectangle defined by 2 points

(define (make-rectangle origin corner)
  (cons origin corner))

(define (rectangle-origin rectangle)
  (car rectangle))

(define (rectangle-corner rectangle)
  (cdr rectangle))

(define (rectangle-width rectangle)
  (abs (- (x-point (rectangle-corner rectangle))
          (x-point (rectangle-origin rectangle)))))

(define (rectangle-height rectangle)
  (abs (- (y-point (rectangle-corner rectangle))
          (y-point (rectangle-origin rectangle)))))

; Procedures operating on rectangles

(define (rectangle-perimeter rectangle)
  (+ (* 2 (rectangle-width rectangle))
     (* 2 (rectangle-height rectangle))))

(define (rectangle-area rectangle)
  (* (rectangle-width rectangle) (rectangle-height rectangle)))

(define (print-point point)
  (display (x-point point))
  (display "@")
  (display (y-point point)))

(define (print-rectangle rectangle)
  (newline)
  (display "(")
  (print-point (rectangle-origin rectangle))
  (display ",")
  (print-point (rectangle-corner rectangle))
  (display ")"))

(let ((rect (make-rectangle (make-point 2 6) (make-point 7 9))))
  (print-rectangle rect)
  (newline)
  (display "perimeter: ")
  (display (rectangle-perimeter rect))
  (newline)
  (display "area: ")
  (display (rectangle-area rect)))

; Rectangle defined by a point and dimensions
;
;(define (make-rectangle origin width height)
;  (cons origin (cons width height)))
;
;(define (rectangle-width rectangle)
;  (car (cdr rectangle)))
;
;(define (rectangle-height rectangle)
;  (cdr (cdr rectangle)))
;
;(define (rectangle-origin rectangle)
;  (car rectangle))
;
;(define (rectangle-corner rectangle)
;  (let ((origin (rectangle-origin rectangle)))
;    (make-point (+ (x-point origin) (rectangle-width rectangle))
;                (+ (y-point origin) (rectangle-height rectangle)))))
  