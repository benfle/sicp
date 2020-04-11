#lang scheme

(require "huffman.rkt")

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

; Testing

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode '(A D A B B C A) sample-tree)