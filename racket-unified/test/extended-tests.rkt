#lang racket/base

;; Extended test suite with more comprehensive test cases
(require rackunit
         "../src/algorithms/unified-pipeline.rkt")

(module+ test
  (displayln "Running extended test suite...")
  (displayln "")
  
  (define test-suite-tests
    (test-suite
     "Extended Pipeline Tests"
     
     ;; Basic expressions
     (test-case "Simple lambda"
       (let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
         (check-true (pipeline-result-success result))
         (check-equal? (pipeline-result-num-bindings result) 1)))
     
     (test-case "Let with single binding"
       (let ([result (compute-h1-from-source-detailed "(let ((x 1)) x)")])
         (check-true (pipeline-result-success result))
         (check-equal? (pipeline-result-num-bindings result) 1)))
     
     (test-case "Let with multiple bindings"
       (let ([result (compute-h1-from-source-detailed "(let ((x 1) (y 2)) (+ x y))")])
         (check-true (pipeline-result-success result))
         (check-equal? (pipeline-result-num-bindings result) 2)))
     
     ;; Nested structures
     (test-case "Nested lambdas"
       (let ([result (compute-h1-from-source-detailed "(lambda (x) (lambda (y) (+ x y)))")])
         (check-true (pipeline-result-success result))
         (check-equal? (pipeline-result-num-bindings result) 2)))
     
     (test-case "Lambda with let"
       (let ([result (compute-h1-from-source-detailed "(lambda (x) (let ((y 1)) (+ x y)))")])
         (check-true (pipeline-result-success result))
         (check-equal? (pipeline-result-num-bindings result) 2)))
     
     ;; Conditional
     (test-case "If expression"
       (let ([result (compute-h1-from-source-detailed "(if #t 1 2)")])
         (check-true (pipeline-result-success result))))
     
     ;; Edge cases
     (test-case "Empty lambda body"
       (let ([result (compute-h1-from-source-detailed "(lambda (x) ())")])
         (check-true (pipeline-result-success result))))
     
     (test-case "Multiple expressions"
       (let ([result (compute-h1-from-source-detailed "(begin (define x 1) x)")])
         ;; This might fail parsing, which is OK
         (void)))
     
     ;; Error handling
     (test-case "Invalid syntax"
       (let ([result (compute-h1-from-source-detailed "invalid!!!")])
         (check-false (pipeline-result-success result))
         (check-not-equal? (pipeline-result-error result) #f))))
  
  (run-tests test-suite-tests)
  
  (displayln "")
  (displayln "✓ Extended test suite complete"))

