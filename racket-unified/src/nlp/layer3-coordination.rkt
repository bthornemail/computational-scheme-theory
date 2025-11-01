#lang racket/base

(require racket/match
         "../s-expression.rkt")

(provide
 broadcast-parse-event
 subscribe-parse-events)

;; ============================================================
;; LAYER 3 COORDINATION - Event Broadcasting
;; ============================================================

;; Event subscribers (pub/sub pattern)
(define event-subscribers (make-parameter '()))

;; Broadcast parse event (pub/sub)
(define (broadcast-parse-event event)
  "Broadcast parse event to subscribers"
  (for ([subscriber (in-list (event-subscribers))])
    (subscriber event)))

;; Subscribe to parse events
(define (subscribe-parse-events handler)
  "Subscribe to parse events"
  (event-subscribers (cons handler (event-subscribers)))
  'subscribed)

