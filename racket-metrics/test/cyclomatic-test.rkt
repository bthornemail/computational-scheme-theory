#lang racket

;; Unit tests for cyclomatic complexity calculator

(require rackunit
         "../cyclomatic.rkt"
         "../cfg-builder.rkt"
         "../cfg-types.rkt"
         "../r5rs-parser.rkt")

(define (test-program source expected-vg)
  (let* ([ast-list (parse-r5rs source)]
         [cfg (build-cfg (first ast-list))]
         [metrics (compute-cyclomatic-complexity cfg)])
    (check-equal? (complexity-metrics-v-g metrics) expected-vg
                  (format "V(G) for ~a" source))))

(module+ test
  ;; Test cases from literature
  
  ;; Linear code: V(G) = 1
  (test-case "Simple variable definition"
    (test-program "(define x 42)" 1))
  
  ;; One branch: V(G) = 2
  (test-case "Simple if statement"
    (test-program "(if (> x 0) 1 -1)" 2))
  
  ;; Factorial: V(G) = 2 (one if, one recursion)
  (test-case "Factorial function"
    (test-program "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))" 2))
  
  ;; Multiple branches: V(G) = 4 (3 conditions + 1)
  (test-case "Cond with multiple clauses"
    (test-program "(cond [(< x 0) 'negative] [(= x 0) 'zero] [(> x 0) 'positive])" 4)))

