#lang scheme

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(define (square x) (* x x))

(square-list-iter (list 1 2 3))

; Generates a list in the reversed order
; (iter (1 2 3) null)
; (iter (2 3) (cons (square 1) null))
; (iter (3) (cons (square 2) (cons (square 1) null)))
; (iter null (cons (square 3) (cons (square 2) (cons (square 1) null))))
; (cons (square 3) (cons (square 2) (cons (square 1) null)))
; (9 4 1)
;
; We can see that the calls to 'cons' are reversed because of the iterative process.
;
; Louis' solution doesn't work either because (cons answer (square (car things)))
; generates a pair whose first element points to a list instead of the square.
;
; (cons (cons (cons null (square 1)) (square 2)) square 3)
; (cons (cons (() 1) (square 2)))
; (cons ((() 1) 4) (square 3))
; (((() 1) 4) 9)
;

; Correct implementation of the iterative square-list
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items null))

(square-list (list 1 2 3))
