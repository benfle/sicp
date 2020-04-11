#lang scheme

(car ''abracadabra)
; equivalent to (car (quote (quote abracadabra)))
; or            (car '(quote abracadabra))

