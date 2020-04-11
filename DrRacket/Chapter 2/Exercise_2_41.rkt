#lang scheme

(define (triple-sums s n)
  (flatmap (lambda (k)
             (filter (lambda (res) (<= (cadddr res) s))
                     (flatmap (lambda (j)
                                (map (lambda (i)
                                       (list i j k (+ i j k)))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- k 1)))))
           (enumerate-interval 1 n)))

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

(triple-sums 9 4)
(triple-sums 7 4)