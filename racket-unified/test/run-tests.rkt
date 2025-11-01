#lang racket/base

;; Test runner script
(require rackunit/text-ui
         "test-pipeline.rkt")

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║           UNIFIED LISP SUBSTRATE TEST SUITE             ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  (run-tests (test-suite
              "Unified Pipeline Tests"
              test-pipeline-tests))
  
  (displayln "")
  (displayln "Test suite complete."))

