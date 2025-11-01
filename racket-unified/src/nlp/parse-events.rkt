#lang racket/base

(require racket/match
         "../s-expression.rkt")

(provide
 emit-parse-event
 replay-parse-events
 parse-event-type?
 query-parsed-event
 verb-parsed-event
 entity-resolved-event
 parse-step-event
 parse-failed-event)

;; ============================================================
;; PARSE EVENT TYPES
;; ============================================================

(define PARSE-EVENT-TYPES '(query-parsed verb-parsed entity-resolved parse-step parse-failed))

;; Check if event type is a parse event
(define (parse-event-type? event-type)
  "Check if event type is a parse event"
  (member event-type PARSE-EVENT-TYPES))

;; Emit parse event as S-expression
(define (emit-parse-event event-type data)
  "Emit parse event as immutable S-expression"
  (define event (make-s-expr event-type data))
  (append-event! event)
  event)

;; Create query-parsed event
(define (query-parsed-event intent semantic-frame)
  "Create query-parsed S-expression event"
  (emit-parse-event 'query-parsed
                    `((intent ,intent)
                      (semantic-frame ,semantic-frame)
                      (timestamp ,(current-inexact-milliseconds)))))

;; Create verb-parsed event
(define (verb-parsed-event verb)
  "Create verb-parsed S-expression event"
  (emit-parse-event 'verb-parsed
                    `((verb ,verb)
                      (timestamp ,(current-inexact-milliseconds)))))

;; Create entity-resolved event
(define (entity-resolved-event entity)
  "Create entity-resolved S-expression event"
  (emit-parse-event 'entity-resolved
                    `((entity ,entity)
                      (timestamp ,(current-inexact-milliseconds)))))

;; Create parse-step event (FSM transition)
(define (parse-step-event from-state to-state token)
  "Create parse-step S-expression event"
  (emit-parse-event 'parse-step
                    `((from-state ,from-state)
                      (to-state ,to-state)
                      (token ,token)
                      (timestamp ,(current-inexact-milliseconds)))))

;; Create parse-failed event
(define (parse-failed-event token error-message)
  "Create parse-failed S-expression event"
  (emit-parse-event 'parse-failed
                    `((token ,token)
                      (error ,error-message)
                      (timestamp ,(current-inexact-milliseconds)))))

;; Replay parse events to reconstruct knowledge graph
(define (replay-parse-events event-store)
  "Replay parse events to reconstruct knowledge graph"
  (define kg '())  ; Placeholder - will be replaced with actual knowledge graph
  (for ([event (in-list event-store)])
    (when (parse-event-type? (s-expr-type event))
      (set! kg (apply-parse-event kg event))))
  kg)

;; Apply single parse event to knowledge graph
(define (apply-parse-event kg event)
  "Apply single parse event to knowledge graph (placeholder)"
  (match (s-expr-type event)
    ['query-parsed
     (let ([data (s-expr-data event)])
       ;; Extract semantic frame and add to knowledge graph
       kg)]
    ['verb-parsed
     ;; Add verb concept to knowledge graph
     kg]
    ['entity-resolved
     ;; Add entity to knowledge graph
     kg]
    ['parse-step
     ;; Record parse transition
     kg]
    ['parse-failed
     ;; Record failure (don't update KG)
     kg]
    [else
     kg]))

