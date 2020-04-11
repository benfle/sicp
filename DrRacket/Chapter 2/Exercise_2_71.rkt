#lang scheme

; n = 5
; ((A 1) (B 2) (C 4) (D 8) (E 16))
;
;           ABCDE
;           /   \
;         ABCD   E
;        /    \
;      ABC     D
;    /     \
;   AB      C
;  /  \
; A    B
;
; Looks like a degenerate binary tree.
;
; 1 bit required to encode most frequent symbol
; (n - 1) bits required to encode least frequent symbol
;