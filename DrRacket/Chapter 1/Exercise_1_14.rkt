#lang scheme

; Space Complexity
; ----------------
;
; The process is recursive with a tree-like shape. So we know that the space complexity is proportional to the depth of the tree.
; When calling (c m n), the longest path to a leaf will be by reducing n completely and then reducing m completely.
; So we have n steps to reduce n and m steps to reduce m.
;
; So S(m, n) = O(m + n) = O(m) if n is constant
;
; Time Complexity
; ---------------
;
; The time complexity is proportional to the number of leaves in the tree.
; cc(m, 1) = cc(m, 0) + cc(m - 1, 1)
; For all m, the left branch (cc(m, 0)) is a terminal call and the right branch (cc(m - 1, 1)) will terminate when m - 1 = 0.
; So the number of leaves is proportional to m.
; So T(m, 1) = O(m)
;
; By mathematical induction, we can show that cc(m, n) = cc(m, n - 1) + cc(m - d(n), n)
; will generate a number of leaves proportional to m^n.
;
; So T(m, n) = O(m^n)
;

; Process Tree
; ------------
;
; (count-change 11)
;  (cc 11 5)
;   (cc 11 4)
;    (cc 11 3)
;     (cc 11 2)
;      (cc 11 1)
;       (cc 11 0)
;        0
;       (cc 10 1)
;        (cc 10 0)
;         0
;        (cc 9 1)
;         (cc 9 0)
;          0
;         (cc 8 1)
;          (cc 8 0)
;           0
;          (cc 7 1)
;           (cc 7 0)
;            0
;           (cc 6 1)
;            (cc 6 0)
;             0
;            (cc 6 1)
;             (cc 6 0)
;              0
;             (cc 5 1)
;              (cc 5 0)
;               0
;              (cc 4 1)
;               (cc 4 0)
;                0
;               (cc 3 1)
;                (cc 3 0)
;                 0
;                (cc 2 1)
;                 (cc 2 0)
;                  0
;                 (cc 1 1)
;                  (cc 1 0)
;                   0
;                  (cc 0 1)
;                   1
;      (cc 6 2)
;       (cc 6 1)
;        (cc 6 0)
;         0
;        (cc 5 1)
;         (cc 5 0)
;          0
;         (cc 4 1)
;          (cc 4 0)
;           0
;          (cc 3 1)
;           (cc 3 0)
;            0
;           (cc 2 1)
;            (cc 2 0)
;             0
;            (cc 1 1)
;             (cc 1 0)
;              0
;             (cc 0 1)
;              1
;       (cc 1 2)
;        (cc 1 1)
;         (cc 1 0)
;          0
;         (cc 0 1)
;          1
;        (cc -4 2)
;         0
;     (cc 1 3)
;      (cc 1 2)
;       (cc 1 1)
;        (cc 1 0)
;         0
;        (cc 0 1)
;         1
;       (cc -4 2)
;        0
;      (cc -9 3)
;       0
;    (cc -14 4)
;     0
;   (cc -39 5)
;    0
