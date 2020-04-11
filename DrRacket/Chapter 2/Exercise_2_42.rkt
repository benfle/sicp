#lang scheme

; Eight-queens Puzzle

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board null)

(define (safe? col positions)
  (define (rec d1 row d2 rest)
    (or (null? rest)
        (let ((check (car rest)))
          (and (not (= row check))
               (not (= d1 check))
               (not (= d2 check))
               (rec (- d1 1) row (+ d2 1) (cdr rest))))))
  (let ((row (car positions)))
    (rec (- row 1) row (+ row 1) (cdr positions))))

(define (adjoin-position row col positions)
  (cons row positions))

(define (enumerate-interval i j)
  (if (> i j)
      null
      (cons i (enumerate-interval (+ i 1) j))))
  
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Testing

(queens 8)
(length (queens 8))
