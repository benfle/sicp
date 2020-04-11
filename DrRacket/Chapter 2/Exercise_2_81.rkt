#lang scheme

; a.
;  If Louis's coercion procedures are installed, apply-generic will loop in case a procedure is not found for
;  2 arguments of the same type.
;
; b.
;  If both arguments have the same type and there is no procedure defined for this type, no coercion procedure will 
; also be found and an error will be raised.
;
; c.
;  We can modify apply-generic to avoid to query the coercion table in case the types are the same.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (proc
           (apply proc (map contents args))
           (if (and (= (length args 2))
                    (not (= (car type-tags) (cadr type-tags))))
               (let ((type1 (car type-tags))
                     (type2 (cadr type-tags))
                     (a1...))
                 ())
               (error "No method for these types" (list op type-tags))))))))
                     