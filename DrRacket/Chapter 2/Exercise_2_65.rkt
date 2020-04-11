#lang scheme

; To implement union-set on balanced binary trees in O(n), we can:
;   tree->list2 generates a sorted list in O(n)
;   union-set generates the union of 2 sets represented as sorted list in O(n)
;   list->tree generates a balanced binary tree from an ordered list in O(n)

(define (union-set set1 set2)
  (list->tree (union-set (tree->list-2 set1)
                         (tree->list-2 set2))))

; Same idea for intersection set

(define (intersection-set set1 set2)
  (list->tree (intersection-set (tree->list-2 set1)
                                (tree->list-2 set2))))

