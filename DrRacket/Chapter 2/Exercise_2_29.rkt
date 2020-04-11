#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b

(define (total-weight mobile)
  (+ (weight-branch (left-branch mobile))
     (weight-branch (right-branch mobile))))

(define (weight-branch branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (total-weight structure)
        structure)))

(define m (make-mobile (make-branch 1 (make-mobile (make-branch 1 5) (make-branch 1 6))) (make-branch 1 1)))
(total-weight m)

; c

(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (weight-branch branch)))

(balanced? m)

(define balanced-mobile (make-mobile (make-branch 1 (make-mobile (make-branch 1 6) (make-branch 2 3))) (make-branch 3 3)))
(balanced? balanced-mobile)

; d

; If we use a pair instead of a list to store a mobile and a branch,
; only the "right-branch" and "branch-structure" selectors need to be modified to use cdr instead of cadr.
