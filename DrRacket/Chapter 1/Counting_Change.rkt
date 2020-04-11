#lang scheme

; Counting Change
;
; Still bug in it. Go back to it when I know how to deal with matrices

(define (count-change amount)
  (cc-iter amount 5))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Recursive Algorithm

(define (cc-rec amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-rec amount (- kinds-of-coins 1))
                 (cc-rec (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

; Iterative Algorithm
; FIXME: Program seems correct but is not very elegant.

(define (cc-iter amount kinds-of-coins)
  ; Table to retain the intermediary computation
  (define table (make-vector (+ amount 1) 0))
  (define (init-table)
    (define (init-row i)
      ; 0 everywhere except in the first row representing the count change for an ammount of 0 which is always 1
      (vector-set! table i (make-vector (+ kinds-of-coins 1) (if (= i 0) 1 0)))
      (cond ((< i amount) (init-row (+ i 1)))))
    (init-row 0))
  (init-table)
  ; Return the value at row i and column j
  (define (table-ref i j)
    (vector-ref (vector-ref table i) j))
  ; Set the given value at row i and column j
  (define (table-set! i j value)
    (vector-set! (vector-ref table i) j value))
  ; The iterative procedure
  (define (cc-iter-aux amount kinds-of-coins i j)
    ; M(i, j) = M(i, j - 1) + M(i - d, j)
    (define d (first-denomination j))
    (table-set! i j (+ (table-ref i (- j 1))
                       (if (< i d) 0 (table-ref (- i d) j))))
    (cond ((and (= amount i) (= kinds-of-coins j)) (table-ref i j))
          ((= kinds-of-coins j) (cc-iter-aux amount kinds-of-coins (+ i 1) 1))
          (else (cc-iter-aux amount kinds-of-coins i (+ j 1)))))
  (cc-iter-aux amount kinds-of-coins 1 1))
