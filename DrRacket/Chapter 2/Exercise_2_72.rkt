#lang scheme

; The encode method is repeated below.
;
; Let's call n the number of symbols in the alphabet.
;
; To encode a symbol, we need to visit at most n nodes (degenerate case of Exercise 2.71)
; For each node, we need to check if the symbol is in the left branch which contains at most n symbols.
;
; However, the most frequent symbol is at the "top" of the tree so we only need to check one
; list of symbols for it.
;
; So to encode most frequent symbol: T(n) and to encode least frequent symbol: T(n^2)
;
;


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (iter tree bits)
    (if (leaf? tree)
        (if (eq? (symbol-leaf tree) symbol)
            bits
            (error "unknown symbol -- ENCODE-SYMBOL" symbol))
        (if (contains? symbol (symbols (left-branch tree)))
            (iter (left-branch tree) (append bits '(0)))
            (iter (right-branch tree) (append bits '(1))))))
  (iter tree '()))

(define (contains? element list)
  (if (null? list)
      #f
      (or (eq? (car list) element)
          (contains? element (cdr list)))))