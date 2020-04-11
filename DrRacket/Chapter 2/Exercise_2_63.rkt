#lang scheme

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-2 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

; a.
;  The wo procedures generate a list of all the nodes in a tree in left-to-right order
;
; b.
;  Let's call n the number of nodes in the balanced tree.
;
;  tree->list-1 visits each node only one time and for each visit concatenate 2 lists no longer than n/2 nodes.
;  So its time complexity is O(n^2)
;
;  tree->list-2 also visit each node one time but only do a cons for each node so its time complexity is O(n)