#lang racket/base

(require "algorithms/unified-pipeline.rkt")

(module+ main
  (displayln "=== Testing Unified Pipeline ===")
  (displayln "")
  
  ;; Test 1: Simple lambda
  (displayln "Test 1: (lambda (x) x)")
  (let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
    (if (pipeline-result-success result)
        (begin
          (displayln (format "✓ Success: H¹ = ~a" (pipeline-result-h1 result)))
          (displayln (format "  Bindings: ~a" (pipeline-result-num-bindings result)))
          (displayln (format "  Simplices: 0-simplices: ~a, 1-simplices: ~a, 2-simplices: ~a"
                             (pipeline-result-num-simplices0 result)
                             (pipeline-result-num-simplices1 result)
                             (pipeline-result-num-simplices2 result))))
        (displayln (format "✗ Error: ~a" (pipeline-result-error result)))))
  
  (displayln "")
  
  ;; Test 2: Let binding
  (displayln "Test 2: (let ((x 1) (y 2)) (+ x y))")
  (let ([result (compute-h1-from-source-detailed "(let ((x 1) (y 2)) (+ x y))")])
    (if (pipeline-result-success result)
        (begin
          (displayln (format "✓ Success: H¹ = ~a" (pipeline-result-h1 result)))
          (displayln (format "  Bindings: ~a" (pipeline-result-num-bindings result))))
        (displayln (format "✗ Error: ~a" (pipeline-result-error result)))))
  
  (displayln "")
  (displayln "=== Pipeline Tests Complete ==="))

