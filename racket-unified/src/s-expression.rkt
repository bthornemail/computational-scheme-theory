#lang racket/base

(require racket/match)

(provide
 s-expr
 s-expr?
 s-expr-type
 s-expr-data
 make-s-expr
 execute-s-expr
 apply-binding-created
 apply-scope-entered
 append-event!
 event-store
 initialize-event-store)

;; ============================================================
;; S-EXPRESSIONS (Object-Language)
;; ============================================================

;; S-expressions are NATIVE LISP - just tagged lists
;; They are homoiconic - can be evaluated directly

(struct s-expr (type data) #:transparent)

;; Create S-expression
(define (make-s-expr type data)
  (s-expr type data))

;; Event store (append-only log of S-expressions)
;; Now backed by file persistence
(define event-store (make-parameter '()))

;; Initialize event store from file
(define (initialize-event-store)
  "Initialize event store by loading events from file"
  (dynamic-require "persistence/event-store-file.rkt" 'ensure-event-log-directory)
  (let* ([load-events (dynamic-require "persistence/event-store-file.rkt" 'load-events-from-file)]
         [loaded-events (load-events)])
    (event-store loaded-events)
    (length loaded-events)))

;; Append event to store (both in-memory and file)
(define (append-event! event)
  "Append S-expression event to event store and file"
  ;; Update in-memory store
  (event-store (append (event-store) (list event)))
  ;; Persist to file
  (let ([append-fn (dynamic-require "persistence/event-store-file.rkt" 'append-event-to-file)])
    (append-fn event)))

;; Execute S-expression (homoiconicity in action)
(define (execute-s-expr se)
  "S-expressions are EXECUTABLE - this is the power of homoiconicity"
  (match se
    [(s-expr 'binding-created data)
     (apply-binding-created data)]
    [(s-expr 'scope-entered data)
     (apply-scope-entered data)]
    [(s-expr 'state-updated data)
     (apply-state-updated data)]
    ;; Combinator Algebra Extension (Appendix Z) events
    [(s-expr 'y-ring-created data)
     (apply-y-ring-created data)]
    [(s-expr 'z-field-created data)
     (apply-z-field-created data)]
    [(s-expr 'recursive-structure-defined data)
     (apply-recursive-structure-defined data)]
    [(s-expr 'fixed-point-computed data)
     (apply-fixed-point-computed data)]
    [(s-expr 'fixed-point-found data)
     (apply-fixed-point-found data)]
    [(s-expr 'iterative-refinement-converged data)
     (apply-iterative-refinement-converged data)]
    [(s-expr 'consensus-started data)
     (apply-consensus-started data)]
    [(s-expr 'consensus-reached data)
     (apply-consensus-reached data)]
    [else
     (error "Unknown S-expression type:" (s-expr-type se))]))

;; Apply binding-created event
(define (apply-binding-created data)
  "Execute binding-created event"
  (let ([id (cadr (assoc 'identifier data))]
        [scope (cadr (assoc 'scope data))])
    (printf "✓ Binding ~a created in ~a\n" id scope)
    ;; Return updated state (simplified for now)
    `((binding ,id) (scope ,scope))))

;; Apply scope-entered event
(define (apply-scope-entered data)
  "Execute scope-entered event"
  (let ([scope-id (cadr (assoc 'scope-id data))]
        [parent (cadr (assoc 'parent-scope data))])
    (printf "✓ Entered scope ~a (parent: ~a)\n" scope-id parent)
    `((current-scope ,scope-id))))

;; Apply state-updated event (placeholder)
(define (apply-state-updated data)
  "Execute state-updated event"
  data)

;; Combinator Algebra Extension event handlers (Appendix Z)

;; Apply y-ring-created event
(define (apply-y-ring-created data)
  "Execute y-ring-created event"
  (let ([name (cadr (assoc 'name data))]
        [base-ring (cadr (assoc 'base-ring data))])
    (printf "✓ Y-combinator ring '~a' created with base ring '~a'\n" name base-ring)
    `((y-ring ,name) (base-ring ,base-ring))))

;; Apply z-field-created event
(define (apply-z-field-created data)
  "Execute z-field-created event"
  (let ([name (cadr (assoc 'name data))]
        [base-field (cadr (assoc 'base-field data))])
    (printf "✓ Z-combinator field '~a' created with base field '~a'\n" name base-field)
    `((z-field ,name) (base-field ,base-field))))

;; Apply recursive-structure-defined event
(define (apply-recursive-structure-defined data)
  "Execute recursive-structure-defined event"
  (let ([ring (cadr (assoc 'ring data))]
        [generator (cadr (assoc 'generator data))]
        [fixed-point (cadr (assoc 'fixed-point data))])
    (printf "✓ Recursive structure defined in ring '~a': fixed point = ~a\n" ring fixed-point)
    `((ring ,ring) (fixed-point ,fixed-point))))

;; Apply fixed-point-computed event
(define (apply-fixed-point-computed data)
  "Execute fixed-point-computed event"
  (let ([ring (cadr (assoc 'ring data))]
        [result (cadr (assoc 'result data))]
        [iterations (cadr (assoc 'iterations data))])
    (printf "✓ Fixed point computed in ring '~a': result = ~a (iterations: ~a)\n" ring result iterations)
    `((ring ,ring) (result ,result))))

;; Apply fixed-point-found event
(define (apply-fixed-point-found data)
  "Execute fixed-point-found event"
  (let ([field (cadr (assoc 'field data))]
        [result (cadr (assoc 'result data))]
        [iterations (cadr (assoc 'iterations data))])
    (printf "✓ Fixed point found in field '~a': result = ~a (iterations: ~a)\n" field result iterations)
    `((field ,field) (result ,result))))

;; Apply iterative-refinement-converged event
(define (apply-iterative-refinement-converged data)
  "Execute iterative-refinement-converged event"
  (let ([field (cadr (assoc 'field data))]
        [result (cadr (assoc 'result data))])
    (printf "✓ Iterative refinement converged in field '~a': result = ~a\n" field result)
    `((field ,field) (result ,result))))

;; Apply consensus-started event
(define (apply-consensus-started data)
  "Execute consensus-started event"
  (let ([type (cadr (assoc 'type data))]
        [nodes (cadr (assoc 'nodes data))])
    (printf "✓ Consensus protocol started: type = ~a, nodes = ~a\n" type nodes)
    `((consensus-type ,type) (nodes ,nodes))))

;; Apply consensus-reached event
(define (apply-consensus-reached data)
  "Execute consensus-reached event"
  (let ([consensus-id (cadr (assoc 'consensus-id data))]
        [final-state (cadr (assoc 'final-state data))]
        [iterations (cadr (assoc 'iterations data))])
    (printf "✓ Consensus reached: id = ~a, state = ~a (iterations: ~a)\n" consensus-id final-state iterations)
    `((consensus-id ,consensus-id) (final-state ,final-state))))

