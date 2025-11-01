#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/intent-mapper.rkt"
         "../../src/nlp/semantic-frame.rkt"
         "../../src/nlp/semantic-lattice.rkt"
         "../../src/m-expression.rkt")

(provide
 intent-mapper-tests)

(define intent-mapper-tests
  (test-suite
   "Intent Mapper Tests"
   
   (test-case "Classify compute operation"
              (define frame (semantic-frame '((action-verb "compute") (object "H1"))
                                            '() '() "compute"))
              (define op-type (classify-operation frame))
              (check-eq? op-type 'compute-h1))
   
   (test-case "Validate type constraints"
              (define frame (semantic-frame '((action-verb "compute") (object "H1"))
                                            '() '() "compute"))
              (check-true (validate-type-constraints frame)))
   
   (test-case "Map to M-expression"
              (define frame (semantic-frame '((action-verb "compute") (object "H1"))
                                            '() '() "compute"))
              (define lattice (empty-lattice))
              (define enriched (enrich-frame frame lattice))
              (define m-expr (map-to-m-expression enriched))
              (check-true (m-expr? m-expr))
              (check-equal? (m-expr-op m-expr) 'computeH1))))

(run-tests intent-mapper-tests)

