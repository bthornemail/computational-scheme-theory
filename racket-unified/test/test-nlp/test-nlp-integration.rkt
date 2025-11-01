#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/layer4-core.rkt"
         "../../src/nlp/layer1-interface.rkt"
         "../../src/m-expression.rkt")

(provide
 nlp-integration-tests)

(define nlp-integration-tests
  (test-suite
   "NLP Integration Tests"
   
   (test-case "End-to-end NL query processing"
              (define-values (m-expr events kg)
                (process-nl-query "compute H1 for program test"))
              (check-true (or (m-expr? m-expr) (not m-expr)))
              (check-true (list? events)))
   
   (test-case "NL to M-expression conversion"
              (define m-expr (nl-to-m-expression "compute H1"))
              (check-true (m-expr? m-expr))
              (check-equal? (m-expr-op m-expr) 'computeH1))
   
   (test-case "Validate NL query"
              (define-values (valid? event)
                (validate-nl-query "compute H1" #f))
              (check-true (boolean? valid?)))))

(run-tests nlp-integration-tests)

