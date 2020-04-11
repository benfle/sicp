#lang scheme

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp) (make-product (exponent exp)
                                             (make-product (make-exponentiation (base exp)
                                                                                (- (exponent exp) 1))
                                                           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Sum
(define (make-sum . args)
  (let ((summands (filter (lambda (x) (not (=number? x 0))) args)))
    (cond ((null? summands) 0)
          ((= (length summands) 1) (car summands))
          ((every number? summands) (accumulate + 0 summands))
          (else (cons '+ summands)))))
(define (=number? e n) (and (number? e) (= e n)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

; Product

(define (make-product . args)
  (let ((factors (filter (lambda (x) (not (=number? x 1))) args)))
    (cond ((null? factors) 1)
          ((any (lambda (x) (=number? x 0)) factors) 0)
          ((= (length factors) 1) (car factors))
          ((every number? factors) (apply * factors))
          (else (cons '* factors)))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

; Exponentiation

(define (make-exponentiation base exp)
  (cond ((not (number? exp)) (error "unknown exponent -- MAKE-EXPONENTIATION" exp))
        ((= exp 0) 1)
        ((= exp 1) base)
        (else (list '** base exp))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; Utilities

(define (every test list)
  (or (null? list)
      (and (test (car list))
           (every test (cdr list)))))

(define (any test list)
  (and (not (null? list))
       (or (test (car list))
           (any test (cdr list)))))

(define (accumulate op init list)
  (if (null? list)
      init
      (op (car list) (accumulate op init (cdr list)))))

; ---------- Testing

(deriv '(+ x 3 x 2) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)