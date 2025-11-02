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

