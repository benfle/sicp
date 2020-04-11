#lang scheme

; Recursive version of cont-frac
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

; Iterative version of cont-frac
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

; We want to find k in order to get an approximation of 1/phi that is accurate to 4 decimal phases

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (approx-for k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define tolerance 0.0001)

(define (find-k first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (+ 1 guess)))
      (if (close-enough? (approx-for guess) (/ 1 phi))
          guess
          (try next))))
  (try first-guess))

(find-k 1)
