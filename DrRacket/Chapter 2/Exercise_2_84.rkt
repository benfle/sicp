#lang scheme

; Return the procedure to raise t1 to t2 or null if it is not possible
(define (raise-to t1 t2)
  (define (iter type op)
    (if (eq? type t2)
        op
        (let ((raise (get 'raise type)))
          (if raise
              (iter (raise type) (compose raise op))
              null))))
  (iter t1 identity))

(define (try-coercion coercion type1 type2 a1 a2)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (raise-to type1 type2))
                      (t2->t1 (raise-to type2 type1)))
                  (cond ((t1->t2) (apply-generic op (t1->t2 a1) a2))
                        ((t2->t1) (apply-generic op a1 (t2->t1 a2)))
                        (else (let ((t1->t2 (get-coercion type1 type2))
                                    (t2->t1 (get-coercion type2 type1)))
                                (cond ((t1->t2) (apply-generic op (t1->t2 a1) a2))
                                      ((t2->t1) (apply-generic op a1 (t2->t1 a2)))
                                      (else (error "No method for these types" (list op type-tags))))))))))))))
  