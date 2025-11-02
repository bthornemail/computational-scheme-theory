#lang racket/base

(require racket/match
         "../src/algorithms/unified-pipeline.rkt"
         "../src/algorithms/dependency-graph.rkt"
         "../src/algorithms/incidence-structure.rkt")

(module+ test
  (require racket/function)

  ;; Test 1: Simple recursive factorial
  (define test-factorial
    "(define (factorial n)
       (if (<= n 1)
           1
           (* n (factorial (- n 1)))))")

  ;; Test 2: Mutual recursion
  (define test-mutual-recursion
    "(define (even? n)
       (if (= n 0)
           #t
           (odd? (- n 1))))
     (define (odd? n)
       (if (= n 0)
           #f
           (even? (- n 1))))")

  ;; Test 3: Simple linear program (no cycles expected)
  (define test-linear
    "(define x 10)
     (define y 20)
     (define z (+ x y))")

  (printf "=== Cycle Detection Integration Test ===\n\n")

  ;; Test factorial
  (printf "Test 1: Recursive Factorial\n")
  (printf "Program: ~a\n" test-factorial)
  (let-values ([(h1-cech h1-incidence dep-graph incidence-struct error-msg)
                (compute-h1-from-source test-factorial)])
    (if error-msg
        (printf "ERROR: ~a\n\n" error-msg)
        (begin
          (printf "H¹ (Čech): ~a\n" h1-cech)
          (printf "H¹ (Incidence): ~a\n" h1-incidence)
          (when dep-graph
            (let ([cycles (detect-cycles dep-graph)]
                  [recursive-calls (find-recursive-calls dep-graph)])
              (printf "Dependency cycles detected: ~a\n" (length cycles))
              (printf "Recursive calls: ~a\n" (length recursive-calls))))
          (printf "\n"))))

  ;; Test mutual recursion
  (printf "Test 2: Mutual Recursion (even/odd)\n")
  (printf "Program: ~a\n" test-mutual-recursion)
  (let-values ([(h1-cech h1-incidence dep-graph incidence-struct error-msg)
                (compute-h1-from-source test-mutual-recursion)])
    (if error-msg
        (printf "ERROR: ~a\n\n" error-msg)
        (begin
          (printf "H¹ (Čech): ~a\n" h1-cech)
          (printf "H¹ (Incidence): ~a\n" h1-incidence)
          (when dep-graph
            (let ([cycles (detect-cycles dep-graph)])
              (printf "Dependency cycles detected: ~a\n" (length cycles))))
          (printf "\n"))))

  ;; Test linear (should have H¹ = 0)
  (printf "Test 3: Linear Program (no cycles expected)\n")
  (printf "Program: ~a\n" test-linear)
  (let-values ([(h1-cech h1-incidence dep-graph incidence-struct error-msg)
                (compute-h1-from-source test-linear)])
    (if error-msg
        (printf "ERROR: ~a\n\n" error-msg)
        (begin
          (printf "H¹ (Čech): ~a\n" h1-cech)
          (printf "H¹ (Incidence): ~a\n" h1-incidence)
          (when dep-graph
            (let ([cycles (detect-cycles dep-graph)])
              (printf "Dependency cycles detected: ~a (expected 0)\n" (length cycles))))
          (printf "\n"))))

  (printf "=== Integration Test Complete ===\n"))

