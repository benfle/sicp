#lang scheme

(require "Interval.rkt")

; The easiest way to identify the different cases is to list the possible signs
; for the ends of the intervals.
;
; n n n n                (uu, ll)
; n n n p                (lu, ll)
; n n p n (impossible)
; n n p p                (lu, ul)
; n p n n                (ul, ll)
; n p n p                (min(lu, ul), max(ll, uu))
; n p p n (impossible)
; n p p p                (lu, uu)
; p n n n (impossible)
; p n n p (impossible)
; p n p n (impossible)
; p n p p (impossible)
; p p n n                (ul, lu)
; p p n p                (ul, uu)
; p p p n (impossible)
; p p p p                (ll, uu)
;
; Which gives us 9 cases.
;
; To figure out the formula (ll, uu)..., it helps to draw the intervals like this:
;
;   [   ]
;  [   ] 
; ----------+------------
;           0

(define (mul-interval2 x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (if (negative? a)
        (if (negative? b)
            (if (negative? c)
                (if (negative? d)
                    (make-interval (* b d) (* a c))
                    (make-interval (* a d) (* a c)))
                (make-interval (* a d) (* b c)))
            (if (negative? c)
                (if (negative? d)
                    (make-interval (* b c) (* a c))
                    (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))
                (make-interval (* a d) (* b d))))
        (if (negative? c)
            (if (negative? d)
                (make-interval (* b c) (* a d))
                (make-interval (* b c) (* b d)))
            (make-interval (* a c) (* b d))))))
                    
        
            
; Test Cases
(mul-interval2 (make-interval -2 -1) (make-interval -2 -1)) ; ( 1 .  4)
(mul-interval2 (make-interval -2 -1) (make-interval -2  1)) ; (-2 .  4)
(mul-interval2 (make-interval -2 -1) (make-interval  1  2)) ; (-4 . -1)
(mul-interval2 (make-interval -2  1) (make-interval -2 -1)) ; (-2 .  4)
(mul-interval2 (make-interval -2  1) (make-interval -2  1)) ; (-2 .  4)
(mul-interval2 (make-interval -2  1) (make-interval  1  2)) ; (-4 .  2)
(mul-interval2 (make-interval  1  2) (make-interval -2 -1)) ; (-4 . -1)
(mul-interval2 (make-interval  1  2) (make-interval -1  1)) ; (-2 .  2)
(mul-interval2 (make-interval  1  2) (make-interval  1  2)) ; ( 1 .  4)
(mul-interval2 (make-interval -2  1) (make-interval -1  1)) ; (-2 .  2)

  
