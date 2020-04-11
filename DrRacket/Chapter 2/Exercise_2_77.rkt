#lang scheme

; (magnitude z)                                 ; z being a complex number ('complex tag)
; (apply-generic 'magnitude z)                  ; The 'complex tag is "removed" from the number
; (magnitude ('rectangular 3 4))                ; z being a complex number in rectangular form ('rectangular tag)
; (apply-generic 'magnitude ('rectangular 3 4)) ; The magnitude procedure form the rectangular package is called
; (magnitude (3 4))
; 5