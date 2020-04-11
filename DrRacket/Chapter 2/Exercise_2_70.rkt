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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge (adjoin-set (make-code-tree (cadr leaves) (car leaves)) (cddr leaves)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define alphabet '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
(define message '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(define tree (generate-huffman-tree alphabet))
tree
(define code (encode message tree))
code
(length code)

; With a fixed-length code, we need log2(8) = 3 bits per symbol
; The message has 36 symbols so we would need 36 * 3 = 108 bits