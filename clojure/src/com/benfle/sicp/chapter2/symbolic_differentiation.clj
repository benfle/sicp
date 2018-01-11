(ns com.benfle.sicp.chapter2.symbolic-differentiation
  (:refer-clojure :exclude [derive]))

;; Utils

(def third #(nth % 2))

(defn error
  [message]
  (throw (Exception. message)))

;; Algebraic Expressions

(def variable? symbol?)
(def same-variable? =)

(defn =number? [exp num] (and (number? exp) (= exp num)))

(defn make-sum
  [addend augend]
  (cond (=number? addend 0) augend ; do not really make a sum here, sloppy
        (=number? augend 0) addend
        (and (number? addend) (number? augend)) (+ addend augend)
        :else (list '+ addend augend)))

(defn sum?
  [[op :as exp]]
  (= op '+))

(defn addend
  [[_ addend]]
  addend)

(defn augend
  [[_ _ & augend]]
  (if (= 1 (count augend))
    (first augend)
    (cons '+ augend)))

(defn make-product
  [multiplier multiplicand]
  (cond
    (or (=number? multiplier 0) (=number? multiplicand 0)) 0
    (=number? multiplier 1) multiplicand
    (=number? multiplicand 1) multiplier
    (and (number? multiplier) (number? multiplicand)) (* multiplier multiplicand)
    :else (list '* multiplier multiplicand)))

(defn product?
  [[op]]
  (= '* op))

(defn multiplier
  [[_ multiplier]]
  multiplier)

(defn multiplicand
  [[_ multiplier & multiplicand]]
  (if (= 1 (count multiplicand))
    (first multiplicand)
    (cons '* multiplicand)))

(defn make-exponentiation
  [base exponent]
  (list '** base exponent))

(defn exponentiation?
  [[op]]
  (= '** op))

(defn base
  [[_ base]]
  base)

(defn exponent
  [[_ _ exponent]]
  exponent)

(defn derive
  [exp var]
  (cond
    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var) 1 0)

    (sum? exp)
    (make-sum (derive (addend exp) var)
              (derive (augend exp) var))

    (product? exp)
    (make-sum
     (make-product (multiplier exp)
                   (derive (multiplicand exp) var))
     (make-product (derive (multiplier exp) var)
                   (multiplicand exp)))

    (exponentiation? exp)
    (let [base (base exp)
          exponent (exponent exp)]
      (make-product
       (make-product exponent
                     (make-exponentiation base (dec exponent)))
       (derive base var)))

    :else (error "unknown expression type -- DERIV" exp)))

(comment
  (derive '(+ x 3) 'x)
  (derive '(* x y) 'x)
  (derive '(* (* x y) (+ x 3)) 'x)
  (derive '(** (* x 3) 5) 'x)
  (derive '(** (* x 3) 5) 'x) ; do not get simplified
  (derive '(* x y (+ x 3)) 'x)
  )
