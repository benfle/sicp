#lang scheme

; a.
;  partial-tree is given a list and the number of elements in the list to generate a tree for (starting at the beginning of the list)
;   1. partial-tree determines how many elements will belong to the left branch (left-size) and calls itself with the same list and this number
;   2. This calls should return a list containing the left branch and the rest of the elements to process
;   3. partial-tree determines how many elements will belong to the right branch (right-size) and calls itself with the rest of the elements to process (minus 1: this-entry)
;   4. This call should return a list containing the right branch and the rest of the elements to process
;   5. partial-tree builds the tree with the left branch, the entry and the right branch and return it with the remaining elements
;
;  Example calls:
;   (partial-tree '(1 2 3) 0) -> (() (1 2 3))
;   (partial-tree '(1 2 3) 1) -> ((1 () ()) (2 3))
;   (partial-tree '(1 2 3) 3) -> (2 (1 () ()) (3 () ()))
;
; (1 3 5 7 9 11)     6 -> ((5 (1 () (3 () ())) ((9 (7 () ()) (11 () ()))))
;   (1 3 5 7 9 11)   2 -> ((1 () (3 () ())) 5 7 9 11)
;     (1 3 5 7 9 11) 0 -> (() 1 3 5 7 9 11)
;     (3 5 7 9 11)   1 -> ((3 () ()) 5 7 9 11)
;      (3 5 7 9 11)  0 -> (() 3 5 7 9 11)
;      (5 7 9 11)    0 -> (() 5 7 9 11)
;   (7 9 11)         3 -> ((9 (7 () ()) (11 () ())))
;     (7 9 11)       1 -> ((7 () ()) 9 11)
;       (7 9 11)     0 -> (() 7 9 11)
;       (9 11)       0 -> (() 9 11)
;     (11)           1 -> ((11 () ()))
;       (11)         0 -> (() 11)
;       ()           0 -> (())
;
;        5
;     /     \
;    1       9
;     \    /   \
;      3  7     11
;
;
; b.
;  partial-tree calls itself 2 times with a half-size list so T(2*k) = 2 * T(k) so list->tree has a time complexity of O(n).
; 

