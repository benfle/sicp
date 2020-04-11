#lang scheme

; First implementation

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1 f)
  (car f))

(define (edge1-frame-1 f)
  (cadr f))

(define (edge2-frame-1 f)
  (caddr f))

; Second implementation

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 f)
  (car f))

(define (edge1-frame-2 f)
  (cadr f))

(define (edge2-frame-2 f)
  (cddr f))

; Testing

(define f1 (make-frame-1 1 2 3))
(= (origin-frame-1 f1) 1)
(= (edge1-frame-1 f1) 2)
(= (edge2-frame-1 f1) 3)

(define f2 (make-frame-2 1 2 3))
(= (origin-frame-2 f2) 1)
(= (edge1-frame-2 f2) 2)
(= (edge2-frame-2 f2) 3)
