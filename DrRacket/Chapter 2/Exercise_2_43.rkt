#lang scheme

; Louis' solution doesn't work with my algorithm because I didn't use the "column" argument in "adjoin-position" and "safe".
; However, we can see that it would take much longer because queen_cols is called multiple times for each k.
;
; For board_size = 8,
; queen_cols(8) is called one time
; queen_cols(7) is called 8 times
; queen_cols(6) is called 8x8 = 64 times
; ...
; 
; The original algorithm was calling queen_cols only "board_size" times.
;
