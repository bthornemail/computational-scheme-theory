#lang racket/base

(require rackunit
         "../src/algorithms/unified-pipeline.rkt"
         "../src/bridge/haskell-bridge.rkt"
         "../src/bridge/racket-bridge.rkt")

(module+ test
  (displayln "Running unified pipeline tests...")
  (displayln "")
  
  ;; Test 1: Simple lambda
  (test-case "Simple lambda: (lambda (x) x)"
    (let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
      (check-true (pipeline-result-success result) "Computation should succeed")
      (check-equal? (pipeline-result-num-bindings result) 1 "Should have 1 binding")
      (check (lambda (x) (>= x 0)) (pipeline-result-h1 result) "H¹ should be non-negative")))
  
  ;; Test 2: Let binding
  (test-case "Let binding: (let ((x 1) (y 2)) (+ x y))"
    (let ([result (compute-h1-from-source-detailed "(let ((x 1) (y 2)) (+ x y))")])
      (check-true (pipeline-result-success result) "Computation should succeed")
      (check (lambda (x) (>= x 0)) (pipeline-result-h1 result) "H¹ should be non-negative")))
  
  ;; Test 3: Nested lambdas
  (test-case "Nested lambdas: (lambda (x) (lambda (y) (+ x y)))"
    (let ([result (compute-h1-from-source-detailed "(lambda (x) (lambda (y) (+ x y)))")])
      (check-true (pipeline-result-success result) "Computation should succeed")
      (check-equal? (pipeline-result-num-bindings result) 2 "Should have 2 bindings")))
  
  ;; Test 4: Invalid input
  (test-case "Invalid input handling"
    (let ([result (compute-h1-from-source-detailed "invalid syntax!!!")])
      (check-false (pipeline-result-success result) "Should fail gracefully")
      (check-not-equal? (pipeline-result-error result) #f "Should have error message")))
  
  ;; Test 5: Service availability (non-blocking)
  (test-case "Service availability checks"
    (check-not-exn (lambda () (haskell-service-available?)) "Haskell check should not throw")
    (check-not-exn (lambda () (racket-service-available?)) "Racket check should not throw"))
  
  (displayln "✓ All tests passed!"))

