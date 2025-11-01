#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/parsing-fsm.rkt"
         "../../src/nlp/grammar-parser.rkt")

(provide
 fsm-tests)

(define fsm-tests
  (test-suite
   "Parsing FSM Tests"
   
   (test-case "Initial state creation"
              (define tokens (tokenize "compute H1"))
              (define state (make-initial-parse-state tokens))
              (check-equal? (parse-state-current-state state) 'StartParse))
   
   (test-case "FSM transition - action verb"
              (define tokens (tokenize "compute"))
              (define state (make-initial-parse-state tokens))
              (define token (car tokens))
              (let-values ([(new-state event) (parse-step state token)])
                (check-not-equal? (parse-state-current-state new-state) 'StartParse)))
   
   (test-case "Parse complete query"
              (define-values (state events) (parse-query-fsm "compute H1"))
              (check-eq? (parse-state-current-state state) 'ParseComplete)
              (check-true (> (length events) 0)))))

(run-tests fsm-tests)

