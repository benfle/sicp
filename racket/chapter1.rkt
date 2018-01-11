
;; 1 Building Abstractions with Procedures

;; 1.1 The Elements of Programming

;; This section presents the primitive data and procedures,
;; means of combination, and means of abstraction (define,
;; and procedural abstraction) of our programming language.

;; User-defined procedures are used the same way as primitive
;; procedures. We can't tell wether a procedure is built into
;; the interpreter (primitive) or defined as a compound procedure.

;; The evaluation semantics of different kinds of expressions
;; is presented as well as a simple evaluation model, the
;; substitution model for procedure application.

;; A distinction is made between normal order, lazy evaluation
;; of the arguments of a procedure and applicative order.

;; Exercies 1.1

;; The REPL gives us direct access to the interpreter. Several
;; examples of primitive and compound expressions are evaluated.

;; Exercise 1.2

(define e (/ (+ 5 4 (- 2 (- 3 (+ 6
                                 (/ 4 5)))))
             (* 3 (- 6 2) (- 2 7))))

;; Exercise 1.3

(define (square a)
  (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-squares-of-two-larger a b c)
  (cond ((or (<= a b c) (<= a c b)) (sum-of-squares b c))
        ((or (<= b a c) (<= b c a)) (sum-of-squares a c))
        (#t                         (sum-of-squares a b))))

;; Exercise 1.4

;; The operands as well as the operator is evaluated before
;; evaluating the compound expression.

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; b > 0 -> a + b
;; b < 0 -> a - b
;; a + |b|

;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Applicative Order: (test 0 (p)) does not terminate
;; Normal Order:      (test 0 (p)) -> 0

;; Declarative (what) vs imperative (how) descriptions:
;; As opposed to mathematical functions, computer procedures
;; must be effective. The definition of (sqrt x) as y such that
;; y >= 0 and (square y) = x does not describe a procedure!

;; We could compute the square root of a number using Newton's method
;; of successive approximations.

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6

;; Alyssa P. Hacker's new-if procedure evaluates its arguments
;; before evaluating the condition. That would cause the iterative
;; procedure sqrt-iter to never terminate.

;; Exercise 1.7

(define (err f x y)
  (/ (- (f x) y)
     y))

;; (err sqrt 0.0001 0.01) -> 2.23, an error of 223%, 0.001 is not precise enough.

;; (sqrt 1e50) doesn't terminate because we cannot represent the number
;;             with the appropriate precision (0.001).

;; (sqrt 0.0001) -> 0.0323084... but should have been 0.01. An error of 223%.

(define (better-sqrt-iter guess previous-guess x)
  (if (better-good-enough? guess previous-guess)
      guess
      (better-sqrt-iter (improve guess x) guess x)))

(define (better-good-enough? guess previous-guess)
  (< (abs (- guess previous-guess))
     0.001))

(define (better-sqrt x)
  (better-sqrt-iter 1.0 0 x))

;; Exercise 1.8

(define (cube-root-iter guess previous-guess x)
  (if (better-good-enough? guess previous-guess)
      guess
      (cube-root-iter (improve-cube-root-guess guess x) guess x)))

(define (improve-cube-root-guess guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-root x)
  (cube-root-iter 1.0 0 x))

;; Decomposition:
;; We decomposed the problem of computing the square root of a
;; number into smaller manageable problems and wrote a procedure
;; for each one of them.

;; Procedural abstraction:
;; Each procedure accomplishes an "identifiable task that can be used
;; as a module in defining other procedures." It doesn't matter how
;; the procedure is implemented.

;; Scoping:
;; A procedure definition binds its formal parameters. The scope of
;; these bound variables are the body of the procedure. The other
;; variables in the procedure definition are free.
;; We can also define local variables inside procedures to limit
;; their scope (block structure). Which sometimes allow us to take
;; advantage of lexical scoping to reduce the number of arguments of
;; the procedures.

;; 1.2 Procedures and the Process They Generate

;; "To become experts, we must learn to visualize the process generated
;; by various types of procedures."

;; "A procedure is a pattern for the local evolution of a computation
;; process. It specifies how each stage of the process is built upon the
;; previous stage."

;; Recursive and Iterative processes
;; Iterative processes have their state summarized by a fixed number of
;; state variables.
;; We must distinguish recursive processes from recursive  procedures.
;; A recursive procedure can generate either a recursive process or
;; an iterative process (tail-call recursive).

;; Exercise 1.9

;; The following two procedures compute the sum of 2 numbers. The first
;; one generates a recursive process, the second one an iterative one.

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (+rec a b)
  (if (= a 0)
      b
      (inc (+rec (dec a) b))))

(define (+iter a b)
  (if (= a 0)
      b
      (+iter (dec a) (inc b))))

;; Exercise 1.10

; Ackerman function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;(A 1 10)
;(A 2 4)
;(A 3 3)

; f(n) = 2n
(define (f n) (A 0 n))

; g(n) = 2^n
(define (g n) (A 1 n))

; h(n) = 2^(2^(n - 1))
(define (h n) (A 2 n))

(define (samples f n)
  (when (not (zero? n))
    (display n)
    (display " ")
    (displayln (f n))
    (samples f (dec n))))

;; (samples h 4)

;; Example: Counting Change
;;
;; It is interesting to see how the problem has been simplified:
;; The number of ways to change amount a using n kinds of coins
;; equals:
;;  - the number of ways to change amount a using all but the
;;    first kind of coin, plus
;;  - the number of ways to change amount (a - d) using all n
;;    kinds of coins, where d is the denomination of the first
;;    kind of coin.
;; This can be translated relatively easily into a recursive
;; procedure. Using "dynamic programming" we can then replace
;; this inefficient version by an iterative process.
;; This would require manipulating vectors to store the
;; state variables.

;; Exercise 1.11

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (dec n))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

;; Iterative algorithm:

(define (f n)
  (define (f-iter a b c count)
    (if (zero? count)
        c
        (f-iter (+ a (+ (* 2 b) (* 3 c))) a b (dec count))))
  (f-iter 2 1 0 n))

;; Exercise 1.12

(define (pascal i j)
  (cond ((or (> j i) (or (< i 0) (< j 0))) 0)
        ((or (= j 0) (= j i)) 1)
        (else (+ (pascal (- i 1) (- j 1))
                 (pascal (- i 1) j)))))

;; Exercise 1.13

;; Fib(n) is the closest integer to Phi^n / sqrt(5) if | Fib(n) - Phi^n / sqrt(5) | < 1/2
;; Let's prove that Fib(n) = (Phi^n - Psi^n) / sqrt(5)
;; Fib(0) = (Phi^0 - Psi^0) / sqrt(5) = 0
;; Fib(1) = (Phi^1 - Psi^1) / sqrt(5) = 1

;; By induction, let's prove that Fib(n) = (Phi^n - Psi^n) / sqrt(5)
;; Using the recursive definition of a Fibonacci number, we have: Fib(n) = Fib(n - 2) + Fib(n - 1)
;; Using the asumption of the induction, we can replace the terms on the right hand side: Fib(n) = ( Phi^(n - 2) - Psi^(n - 2) + Phi^(n - 1) - Psi^(n - 1) ) / sqrt(5)
;; So Fib(n) = ( Phi^(n - 2) (1 + Phi) - Psi^(n - 2) (1 + Psi) ) / sqrt(5)
;; We know that Phi^2 = 1 + Phi and Psi^2 = 1 + Psi.
;; So Fib(n) = (Phi^n - Psi^n) / sqrt(5)
;;; So we can rewrite the first inequality: | (Phi^n - Psi^n) / sqrt(5) - Phi^n / sqrt(5) | < 1/2
;; Or | Psi^n / sqrt(5) | < 1/2
;; Or | Psi^n | < sqrt(5) / 2
;; Which is true since |Psi| < 1, so |Psi^n| < 1 < sqrt(5) / 2

;; Orders of Growth:
;; The rate at which computational processes consume computational
;; resources differ considerably.
;; Let's call R(n) the amount of resources the computational process
;; requires for a problem of size n.
;; R(n) has order of groth O(f(n)) if there are positive constants
;; k1 and k2 independent of n such that: k1f(n) <= R(n) <= k2f(n).

;; Order of growth provides a useful indication of how the
;; process behaves as we change the size of the problem.

;; Exercise 1.14

;; Space Complexity
;; ----------------
;;
;; The process is recursive with a tree-like shape. So we know
;; that the space complexity is proportional to the depth of the tree.
;; When calling (c m n), the longest path to a leaf will be by
;; reducing n completely and then reducing m completely. So we have
;; n steps to reduce n and m steps to reduce m.
;;
;; So S(m, n) = O(m + n) = O(m) if n is constant
;;
;; Time Complexity
;; ---------------
;;
;; The time complexity is proportional to the number of leaves
;; in the tree: cc(m, 1) = cc(m, 0) + cc(m - 1, 1)
;; For all m, the left branch (cc(m, 0)) is a terminal call and the
;; right branch (cc(m - 1, 1)) will terminate when m - 1 = 0.
;;
;; So the number of leaves is proportional to m.
;; So T(m, 1) = O(m)
;;
;; By mathematical induction, we can show that:
;;   cc(m, n) = cc(m, n - 1) + cc(m - d(n), n)
;; will generate a number of leaves proportional to m^n.
;;
;; So T(m, n) = O(m^n)
;;
;; Process Tree
;; ------------
;;
;; (count-change 11)
;;  (cc 11 5)
;;   (cc 11 4)
;;    (cc 11 3)
;;     (cc 11 2)
;;      (cc 11 1)
;;       (cc 11 0)
;;        0
;;       (cc 10 1)
;;        (cc 10 0)
;;         0
;;        (cc 9 1)
;;         (cc 9 0)
;;          0
;;         (cc 8 1)
;;          (cc 8 0)
;;           0
;;          (cc 7 1)
;;           (cc 7 0)
;;            0
;;           (cc 6 1)
;;            (cc 6 0)
;;             0
;;            (cc 6 1)
;;             (cc 6 0)
;;              0
;;             (cc 5 1)
;;              (cc 5 0)
;;               0
;;              (cc 4 1)
;;               (cc 4 0)
;;                0
;;               (cc 3 1)
;;                (cc 3 0)
;;                 0
;;                (cc 2 1)
;;                 (cc 2 0)
;;                  0
;;                 (cc 1 1)
;;                  (cc 1 0)
;;                   0
;;                  (cc 0 1)
;;                   1
;;      (cc 6 2)
;;       (cc 6 1)
;;        (cc 6 0)
;;         0
;;        (cc 5 1)
;;         (cc 5 0)
;;          0
;;         (cc 4 1)
;;          (cc 4 0)
;;           0
;;          (cc 3 1)
;;           (cc 3 0)
;;            0
;;           (cc 2 1)
;;            (cc 2 0)
;;             0
;;            (cc 1 1)
;;             (cc 1 0)
;;              0
;;             (cc 0 1)
;;              1
;;       (cc 1 2)
;;        (cc 1 1)
;;         (cc 1 0)
;;          0
;;         (cc 0 1)
;;          1
;;        (cc -4 2)
;;         0
;;     (cc 1 3)
;;      (cc 1 2)
;;       (cc 1 1)
;;        (cc 1 0)
;;         0
;;        (cc 0 1)
;;         1
;;       (cc -4 2)
;;        0
;;      (cc -9 3)
;;       0
;;    (cc -14 4)
;;     0
;;   (cc -39 5)
;;    0

;; Exercise 1.15

;; a. p is applied 5 times when (sine 12.15) is evaluated.
;;    12.15 / 3^5 < 0.1 < 12.15 / 3^4.
;;
;; b.
;;
;; Space Complexity
;; ----------------
;;
;; sine is a recursive procedure and calls itself only one
;; time so the required space is proportional to the number of times
;; sine calls itself which is the number of times p is evaluated
;; which can be written:
;;   S(a) = O(n) with a / 3^n < 0.1
;;
;; We can get rid of the constants when discussing orders of growth
;; so we're looking for n so that a = 3^n or n = log3(a)
;;
;; So S(a) = O(log(a))
;;
;; Time Complexity
;; ---------------
;;
;; The same reasoning applies for the time complexity.
;;
;; T(a) = O(log(a))
;;

;; Exercise 1.16

(define (fast-expt b n)
  (define (iter b n a)
    (cond ((zero? n) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

;; Exercise 1.17

(define (fast-* a b)
  (cond ((zero? b) 0)
        ((= b 1) a)
        ((even? b) (* (fast-* a (/ b 2)) 2))
        (else (+ a (fast-* a (- b 1))))))

;; Exercise 1.18

(define (fast-iter-* a b)
  (define (iter a b c) ; Invariant: ab + c
    (cond ((zero? b) c)
          ((even? b) (iter (* a 2) (/ b 2) c))
          (else (iter a (- b 1) (+ c a)))))
  (iter a b 0))

;; Exercise 1.19

;; T(p, q)^2 = a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;             b <- (bp + aq)p + (bq + aq + ap)q
;;
;; T(p, q)^2 = a <- b (2qp + q^2) + a (2qp + q^2) + a (p^2 + q^2)
;;             b <- b (p^2 + q^2) + a (2qp + q^2)
;;
;; T(p, q)^2 = T(p', q') with p' = (p^2 + q^2) and q' = (2qp + q^2)

(define (fib n)
  (define (iter a b p q c)
    (cond ((zero? c) b)
          ((even? c) (iter a
                           b
                           (+ (square p) (square q))
                           (+ (* 2 q p) (square q))
                           (/ c 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (dec c)))))
  (iter 1 0 0 1 n))

;; Exercise 1.20

;; Normal-order
;; ------------
;;
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; *(remainder 206 40) = 6 > 0
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; **(remainder 40 (remainder 206 40)) = 4 > 0
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; ****(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) = 2 > 0
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;; *******(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) = 0
;; ****(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; 2
;;
;; We have 18 evaluations of reminder.
;;
;; Applicative-Order
;; -----------------
;;
;; (gcd 206 40)
;; *(gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; *(gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; *(gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; *(gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;;
;; We have 4 evaluations of reminder.
;;

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

;; Exercise 1.21

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (inc test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercise 1.22

(define (search-for-primes-after prime? a total)
  (define (iter n c)
    (when (not (= c total))
      (iter (+ n 2)
            (if (timed-prime? prime? n)
                (inc c)
                c))))
  (iter (if (even? a) (inc a) a) 0))

(define (prime? n)
    (if (< n 2)
        (error "prime? not define for n < 2.")
        (= n (smallest-divisor n))))

(define (timed-prime? prime? n)
  (define start-time (runtime))
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (runtime)
  (current-inexact-milliseconds))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (displayln elapsed-time))

(define (search-for-primes)

  (define (search n)
    (search-for-primes-after prime? n 3))

  (search 1000000) ;1e6
  (search 10000000) ; 1e7
  (search 100000000) ; 1e8
  (search 1000000000) ; 1e9
  (search 10000000000) ; 1e10
  (search 100000000000) ; 1e11
  (search 1000000000000) ; 1e12
  )

;; Exercise 1.23

(define (faster-search-for-primes)

  (define (prime? n)
    (if (< n 2)
        (error "prime? not define for n < 2.")
        (= n (find-divisor n 2))))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (next test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))

  (define (search n)
    (search-for-primes-after prime? n 3))

  (search 1000000) ;1e6
  (search 10000000) ; 1e7
  (search 100000000) ; 1e8
  (search 1000000000) ; 1e9
  (search 10000000000) ; 1e10
  (search 100000000000) ; 1e11
  (search 1000000000000) ; 1e12
  )

;; Exercise 1.24

; For (random-integer) because (random) is limited to 2^31
(require (lib "27.ss" "srfi"))

;; Return a random number between 1 and n, n >= 1
(define (rand n)
  (if (= 1 n)
      1
      (inc (random-integer (dec n)))))

(define (fermat-search-for-primes)

  (define (prime? n)
    (define (iter n times)
      (cond ((zero? times) true)
            ((fermat-test n) (iter n (dec times)))
            (else false)))
    (define (fermat-test n)
      (define (expmod base exp m)
        (cond ((zero? exp) 1)
              ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                      m))
              (else (remainder (* base (expmod base (- exp 1) m))
                               m))))
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (rand n)))
    (iter n 100))

  (define (search n)
    (search-for-primes-after prime? n 3))

  (search 1000000) ;1e6
  (search 10000000) ; 1e7
  (search 100000000) ; 1e8
  (search 1000000000) ; 1e9
  (search 10000000000) ; 1e10
  (search 100000000000) ; 1e11
  (search 1000000000000) ; 1e12
  )

;; Exercise 1.25

;; The problem with Alyssa P. Hacker's solution is that the exponential is actually calculated
;; which makes the program run out of memory for 2^1e9 for example.
;;
;; The advantage of the solution proposed in the book (as explained in footnote 46) is that
;; by breaking down the computation we don't have to deal with integers much larger than m.

;; Exercise 1.26

;; If the square is replaced by a multiplication, to compute a^2b % m,
;; we have to compute twice a^b % m so the computation takes twice as many steps.
;;
;; So if we double the exponent, we double the number of steps.
;;
;; So Louis Reasoner's algorithm has a time complexity T(n) = n.

;; Exercise 1.27

; Camichael numbers before 1e4: 561, 1105, 1729, 2465, 2821, 6601, 8911
; Unfortunately the find-carmichael-before procedure has a time complexity of n^2 x log(n) so I stopped at 1e4.

(define (find-carmichael-before n)
  (define (iter n k)
    (cond ((test-carmichael k) (newline) (display k)))
    (unless (= k n) (iter n (+ k 1))))
  (iter n 1))

(define (test-carmichael n)
  (define (prime? n)
    (= n (smallest-divisor n)))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (next test-divisor)
    (if (= test-divisor 2)
        3
        (+ test-divisor 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (expmod base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                  m))
          (else (remainder (* base (expmod base (dec exp) m))
                           m))))
  (define (iter n a res)
    (cond ((not res) res)
          ((= a n) (not (prime? n)))
          (else (iter n (inc a) (= (expmod a n n) a)))))
  (if (> n 2)
      (iter n 2 #t)
      #f))

;; Exercise 1.28

;; Miller-Rabin test

(define (miller-rabin-prime? n)
  (define (iter n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (iter n (dec times)))
          (else false)))
  (define (miller-rabin-test n)
    (define (try-it a)
      (= (expmod a (dec n) n) 1))
    (define (check x m)
      (if (and (not (= x 1))
               (not (= x (dec m)))
               (= (remainder (square x) m) 1))
          0
          (remainder (square x) m)))
    (define (expmod base exp m)
      (cond ((zero? exp) 1)
            ((even? exp) (check (expmod base (/ exp 2) m) m))
            (else (remainder (* base (expmod base (dec exp) m))
                             m))))
    (try-it (rand n)))
  (iter n 10))

;; 1.3 Formulating Abstractions with Higher-Order Procedures

;; Higher-order procedures:
;; They can take other procedures as argument or return a procedure.

;; Exercise 1.29

; Simpson's Rule for integral approximation

(define (simpson-integral f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (g k)
    (* (cond ((or (zero? k) (= k n)) 1.0)
             ((even? k) 2.0)
             (else 4.0))
       (y k)))
  (* (/ h 3)
     (sum g 0 inc n)))

; The integral approximation proposed in the book

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (compare-integral-approximations f a b n)
  (display "Simpson's Rule: ")
  (displayln (simpson-integral f a b n))
  (display "Approximation: ")
  (displayln (integral f a b (/ (- b a) n))))

;; Exercise 1.30

; Iterative version of the higher-order procedure sum

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (identity n) n)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (pi-next n) (+ n 1))
  (* 4.0 (product pi-term 1 pi-next n)))

; Recursive version of product
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter c result)
    (if (> c b)
        result
        (iter (next c)
              (combiner result
                        (term c)))))
  (iter a null-value))

(define (acc-sum term a next b)
  (accumulate + 0 term a next b))

(define (acc-product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

;; Exercise 1.33

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter c result)
    (if (> c b)
        result
        (iter (next c)
              (if (predicate c)
                  (combiner result
                            (term c))
                  result))))
  (iter a null-value))

;; We could also have leverage the existing accumulate by simply
;; modifying the combiner:

(define (filtered-accumulate* predicate combiner null-value term a next b)
  (define (filtered-combiner result c)
    (if (predicate c)
        (combiner result c)
        result))
  (accumulate filtered-combiner null-value term a next b))

;; One of the problem with this kind of function is that it
;; complects too many things at once. Better to have different
;; function to walk through a sequence, accumulate, and filter
;; and be able to compose them at will.

;; a. sum of the square of the prime numbers between a and b

(define (sum-of-square-of-primes-between a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; b. product of all positive integers i < n such that gcd(i, n) = 1
(define (product-of-relative-primes-under n)
  (define (relative-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

;; Exercise 1.34

;; (f f) = (f 2) = (2 2) but 2 is not a procedure so the call fails.

;; Exercise 1.35

;; We know that phi^2 = 1 + phi
;; If we divide by phi on both side: phi = 1/phi + 1 = f(phi)
;;
;; So phi is a fixed-point of f: x -> 1 + 1/x

(define tolerance 0.001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi-as-fixed-point
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define phi (/ (+ 1 (sqrt 5)) 2))

;; Exercise 1.36

(define (logged-fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (displayln tolerance)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; (logged-fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;; (logged-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)

;; Exercise 1.37

; Recursive version of cont-frac
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (rec (inc i))))))
  (rec 1))

; Iterative version of cont-frac
(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (zero? i)
        res
        (iter (dec i) (/ (n i)
                         (+ (d i) res)))))
  (iter k 0))

; We want to find k in order to get an approximation of 1/phi that is accurate to 4 decimal phases

(define (approx-for k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k))

(define tolerance 0.0001)

(define (find-k first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (+ 1 guess)))
      (if (close-enough? (approx-for guess) (/ 1 phi))
          guess
          (try next))))
  (try first-guess))

;; (find-k 1)

;; Exercise 1.38

(define (approx-e k)
  (+ 2 (cont-frac-iter (lambda (x) 1.0)
                       (lambda (x) (if (= (modulo x 3) 2)
                                       (/ (* 2 (+ x 1)) 3)
                                       1))
                       10)))

;; (approx-e 10)

;; Exercise 1.39

;; Lambert's formula to approximate tan(x)
(define (tan-cf x k)
  (cont-frac-iter (lambda (n) (if (= n 1) x (- (square x))))
                  (lambda (n) (dec (* 2.0 n)))
                  k))

;; (tan-cf (/ pi 4) 5)

;; Higher-order procedures let us express our ideas
;; much more clearly:

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y)))))

(define (average-dump f)
  (lambda (x) (average x (f x))))

;; "Experienced programmers know how to choose procedural formulations
;; that are particularly perspicuous, and where useful elements of the
;; process are exposed as separate entities that can be reused in other
;; applications."

;; It is not clear how to decompose correctly: the term "useful" is not
;; clear enough and "reuse" might not be specific enough to guide
;; the design of the software. How to decompose software correctly is
;; a hard question. (See Parnas, Alexander).

;; Our programming languages should not restrict the ways in which
;; computational elements can be manipulated.
;; Elements with hte fewaest restrictions are said to have the
;; "first-class" status, they may be:
;;  - named by variables
;;  - passed as argument to procedures
;;  - returned as the results of procedures
;;  - included in data structures

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; Exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

; (((double (double double)) inc) 5)

;; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

;; Exercise 1.44

(define (average a b c)
  (/ (+ a b c) 3))

(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (n-smooth f n)
  (repeated (smooth f) n))

;; Exercise 1.45

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (cubic-root x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-damp
                            1.0))

(define (fourth-root x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            (repeated average-damp 2)
                            1.0))

(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (floor (/ (log x) (log 2))))
                            1.0))

; (nth-root 49 32)


;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))
  try)

(define (sqrt x)
  ((iterative-improve (lambda (guess next) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   1.0))

(sqrt 9)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (v1 v2) (< (abs (- v1 v2)) 0.00001))
                      (lambda (x) (f x)))
   first-guess))
