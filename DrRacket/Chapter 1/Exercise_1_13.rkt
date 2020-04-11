#lang scheme

; Fib(n) is the closest integer to Phi^n / sqrt(5) if | Fib(n) - Phi^n / sqrt(5) | < 1/2

; Let's prove that Fib(n) = (Phi^n - Psi^n) / sqrt(5)

; Fib(0) = (Phi^0 - Psi^0) / sqrt(5) = 0
; Fib(1) = (Phi^1 - Psi^1) / sqrt(5) = 1

; By induction, let's prove that Fib(n) = (Phi^n - Psi^n) / sqrt(5)

; Using the recursive definition of a Fibonacci number, we have: Fib(n) = Fib(n - 2) + Fib(n - 1)

; Using the asumption of the induction, we can replace the terms on the right hand side: Fib(n) = ( Phi^(n - 2) - Psi^(n - 2) + Phi^(n - 1) - Psi^(n - 1) ) / sqrt(5)

; So Fib(n) = ( Phi^(n - 2) (1 + Phi) - Psi^(n - 2) (1 + Psi) ) / sqrt(5)

; We know that Phi^2 = 1 + Phi and Psi^2 = 1 + Psi. So Fib(n) = (Phi^n - Psi^n) / sqrt(5)

; So we can rewrite the first inequality: | (Phi^n - Psi^n) / sqrt(5) - Phi^n / sqrt(5) | < 1/2

; Or | Psi^n / sqrt(5) | < 1/2

; Or | Psi^n | < sqrt(5) / 2

; Which is true since |Psi| < 1, so |Psi^n| < 1 < sqrt(5) / 2
