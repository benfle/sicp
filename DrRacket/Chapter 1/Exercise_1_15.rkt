#lang scheme

; a.
; p is applied 5 times when (sine 12.15) is evaluated because 12.15 / 3^5 < 0.1 < 12.15 / 3^4
;
; b.
;
; Space Complexity
; ----------------
;
; sine is a recursive procedure and calls itself only one time so the required space is proportional to the number of times
; sine calls itself which is the number of times p is evaluated which can be written:
;
; S(a) = O(n) with a / 3^n < 0.1
;
; We can get rid of the constants when discussing orders of growth so we're looking for n so that a = 3^n or n = log3(a)
;
; So S(a) = O(log(a))
;
; Time Complexity
; ---------------
;
; The same reasoning applies for the time complexity.
;
; T(a) = O(log(a))
;

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.5)
