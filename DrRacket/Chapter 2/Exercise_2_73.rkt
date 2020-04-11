#lang scheme

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

; a.
;  We replaced a static conditional expression with a dynamic data-directed dispatch based on the operator of the expression. We this model, we can extend the deriv
;  procedure incrementally without changing it.
;  We cannot assimilate number? and variable? because we chose to represent them using lisp primitive types: number and symbols which don't let us prefix tags to them.
;  
; b.
;  Derivatives of sum and product

(define (install-sum-and-product)

  (define (=number? e n) (and (number? e) (= e n)))

  (define (make-sum addend augend)
    (cond ((=number? addend 0) augend)
          ((=number? augend 0) addend)
          ((and (number? addend) (number? augend)) (+ addend augend))
          (else '(+ addend augend))))
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  (define (deriv-+ operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else '(* m1 m2))))
  (define (multipler operands) (car operands))
  (define (multiplicand operands) (cadr operands))    
  (define (deriv-* operands var)
    (make-sum (make-product (multiplier operands)
                            (deriv (multiplicand operands) var))
              (make-product (deriv (multiplier operands) var)
                            (multiplicand operands))))
  
  (put 'deriv '+ deriv-+)
  (put 'deriv '* deriv-*)
  
  'done)

; c.
;  Derivative of exponential

(define (install-exponential)

  (define (make-exponentiation base exp)
    (cond ((not (number? exp)) (error "unknown exponent -- MAKE-EXPONENTIATION" exp))
          ((= exp 0) 1)
          ((= exp 1) base)
          (else '(** base exp))))
  (define (exponent operands) (cadr operands))
  (define (base operands) (car operands))
  (define (deriv-** operands var)
    (make-product (exponent operands)
                  (make-product (make-exponentiation (base operands)
                                                     (- (exponent operands) 1))
                                (deriv (base operands) var))))
  (put 'deriv '** derive-**)
  
  'done)

; d.
;  If we index the procedures in the opposite way, we only have to change the calls to "put" in the same way: (put '+ 'deriv deriv-+)

