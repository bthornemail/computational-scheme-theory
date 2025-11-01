#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/grammar-parser.rkt")

(provide
 grammar-tests)

(define grammar-tests
  (test-suite
   "Grammar Parser Tests"
   
   (test-case "Tokenize basic query"
              (define tokens (tokenize "compute H1 for program X"))
              (check-equal? (length tokens) 5))
   
   (test-case "Tokenize action verb"
              (define tokens (tokenize "compute"))
              (check-equal? (token-type (car tokens)) 'action-verb))
   
   (test-case "Tokenize object"
              (define tokens (tokenize "H1"))
              (check-equal? (token-type (car tokens)) 'object))
   
   (test-case "Parse simple compute query"
              (define-values (frame1 frame2) (parse-query "compute H1"))
              (check-true (semantic-frame? frame1))
              (check-true (semantic-frame-intent-type frame1)))
   
   (test-case "Parse query with entity"
              (define-values (frame1 frame2) (parse-query "compute H1 for program test"))
              (check-true (semantic-frame? frame1)))))
   
(run-tests grammar-tests)

