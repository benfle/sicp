#lang scheme

; project "pushes" an object down in the tower

(define (project x) (apply-generic 'project x))

(define (complex2real x)
  (real-part x))
(put 'project '(complex) complex2real)

(define (real2rational x)
  (make-rat 1 (div 1 x)))
(put 'project '(real) real2rational)

(define (rational2integer x)
  (floor x))
(put 'project '(rational) rational2integer)

(define (drop x)
  (let ((proc (get 'project (type-tag x))))
    (if proc
        (let ((projected-x (proc x)))
          (if (equ? (raise projected-x) x)
              (drop projected-x)
              x))
        x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args))) ; added drop here
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