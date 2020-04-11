#lang scheme

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

; The algorithm is similar to the one in the change-coin exercises.
;
; To generate all subsets, for each element:
;  - compute set of subsets of every elements "after" it
;  - concatenate the element with each element of the subset
;  - append the subset with the set of concatenations
;
; (subsets 1 2 3)
; (subsets 2 3) + "prefix" 1 to each (subsets 2 3)
; (subsets 3) + "prefix" 2 to each (subsets 3) + "prefix" 1 to each (subsets 2 3)
; (subsets null) + "prefix" 3 to each (subsets null) + "prefix" 2 to each (subsets 3) + "prefix" 1 to each (subsets 2 3)
; ()             + (3)                               + (2) (2 3)                      + (1) (1 3) (1 2) (1 2 3)
