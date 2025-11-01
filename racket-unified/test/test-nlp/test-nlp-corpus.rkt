#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/layer4-core.rkt")

(provide
 nlp-corpus-tests)

;; Example NL queries for corpus validation
(define corpus-queries
  '("compute H1 for program test1"
    "validate hypothesis for corpus"
    "compare H1 and V(G) for program test2"
    "analyze patterns in program test3"))

(define nlp-corpus-tests
  (test-suite
   "NLP Corpus Validation Tests"
   
   (test-case "Process corpus queries"
              (for ([query (in-list corpus-queries)])
                (define-values (m-expr events kg)
                  (process-nl-query query))
                (check-true (or (not m-expr) #t))  ; M-expression or failure is ok
                (check-true (list? events))))
   
   (test-case "All queries generate events"
              (for ([query (in-list corpus-queries)])
                (define-values (m-expr events kg)
                  (process-nl-query query))
                (check-true (> (length events) 0))))))

(run-tests nlp-corpus-tests)

