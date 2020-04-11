#lang scheme

; Try to coerce all types to t1 and return the list of coercion procedures
; Return null if it can't
(define (try-coerce t1 types)
  (define (iter types res)
    (if (null? types)
        res
        (let ((t2 (car types)))
          (let ((t2->t1 (get-coercion t2 t1)))
            (if (t2->t1)
                (try-coerce t1 (cdr types) (append res (list t2->t1)))
                null)))))
  (iter types null))

; Try to coerce all the types to the first one, then to the second one...
; until a coercion of all types to the same type is possible.
; Return null if no coercion is found.
(define (find-coercions types)
  (define (iter t1s)
    (if (null? t1s)
        null
        (let ((coercions (try-coerce  (car t1s) types)))
          (if (coercions)
              coercions
              (iter (cdr t1s))))))
  (iter types))

(define (apply-generic op . args)
  (define (iter op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (let ((coercions (find-coercions args)))
              (if (coercions)
                  (iter op (multi-map coercions args))
                  (error "No method for these types" (list op type-tags))))))))
  (iter op args))

(define (multi-map procs items)
  (if (or (null? procs) (null? items))
      null
      (cons ((car procs) (car items)) (multi-map (cdr procs) (cdr items)))))

; It is not enough to try to coerce all types with each other, we should also climb up the tree of types.
; For example, if B and C are subtypes of A and we don't have any procedure to coerce B to C or C to B, then
; we can coerce both to A and try to find a procedure on (A A).
;
; Moreover, the coercion on the types is not symmetric. If we found a coercion t1->t2, we will never try
; to find procedures whose first argument is of type t1. For example, let's say we have types (A B).
; And there is a coercion A->B and B->A but we only have a procedure that matches (A A). With the current
; implementation we will look for (A B) then (B B) and fail. We missed the procedure on (A A).
;
