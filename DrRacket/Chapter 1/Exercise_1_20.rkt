#lang scheme

; Normal-order
; ------------
;
; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; *(remainder 206 40) = 6 > 0
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; **(remainder 40 (remainder 206 40)) = 4 > 0
; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; ****(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) = 2 > 0
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; *******(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) = 0
; ****(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; 2
;
; We have 18 evaluations of reminder.
;
; Applicative-Order
; -----------------
;
; (gcd 206 40)
; *(gcd 40 (remainder 206 40))
; (gcd 40 6)
; *(gcd 6 (remainder 40 6))
; (gcd 6 4)
; *(gcd 4 (remainder 6 4))
; (gcd 4 2)
; *(gcd 2 (remainder 4 2))
; (gcd 2 0)
; 2
;
; We have 4 evaluations of reminder.
;