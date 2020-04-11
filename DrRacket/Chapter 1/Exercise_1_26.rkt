#lang scheme

; If the square is replaced by a multiplication, to compute a^2b % m,
; we have to compute twice a^b % m so the computation takes twice as many steps.
; 
; So if we double the exponent, we double the number of steps.
;
; So Louis Reasoner's algorithm has a time complexity T(n) = n.