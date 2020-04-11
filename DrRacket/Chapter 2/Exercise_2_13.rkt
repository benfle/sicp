#lang scheme

; If we consider positive intervals:
; p([a0, b0], [a1, b1]) = p([a0a1, b0b1])
; Since p = 100 * w / c, we have:
; p([a0a1, b0b1]) = 100 * (b0b1 - a0a1) / (b0b1 + a0a1)
; Since ai = ci(1 - pi/100) and bi = ci(1 + pi/100), we have:
; p([a0a1, b0b1]) = 100 * (c0c1(1 + p0/100)(1 + p1/100) - c0c1(1 - p0/100)(1 - p1/100)) / (c0c1(1 + p0/100)(1 + p1/100) + c0c1(1 - p0/100)(1 - p1/100))
; p([a0a1, b0b1]) = 100 * ((1 + p0/100)(1 + p1/100) - (1 - p0/100)(1 - p1/100)) / ((1 + p0/100)(1 + p1/100) + (1 - p0/100)(1 - p1/100))
; p([a0a1, b0b1]) = 100 * (1 + p1/100 + p0/100 + p0p1/10000 - 1 + p1/100 + p0/100 - p0p1/10000) / (1 + p1/100 + p0/100 + p0p1/10000 + 1 - p1/100 - p0/100 + p0p1/10000)
; p([a0a1, b0b1]) = 100 * ( (2(p0 + p1)/100) / (2(1 + p0p1/10000)) )
; But if p0 and p1 are small, p0p1/10000 is close to 0 so:
; p([a0a1, b0b1]) = p0 + p1
