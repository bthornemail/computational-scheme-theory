#lang racket/base

(require racket/match
         "../m-expression.rkt"
         "../s-expression.rkt"
         "grammar-parser.rkt"
         "parsing-fsm.rkt"
         "semantic-frame.rkt"
         "intent-mapper.rkt"
         "knowledge-graph.rkt"
         "semantic-lattice.rkt"
         "layer3-coordination.rkt")

(provide
 validate-nl-query
 process-nl-query)

;; ============================================================
;; LAYER 4 CORE - FSM Extension for NL Processing
;; ============================================================

;; Validate NL query using grammar
(define (validate-nl-query nl-text current-state)
  "Validate NL query - returns (values validated? event?)"
  (define-values (parse-state events) (parse-query-fsm nl-text))
  (define is-valid (eq? (parse-state-current-state parse-state) 'ParseComplete))
  (values is-valid (if (not (null? events)) (car events) #f)))

;; Process NL query - full pipeline
(define (process-nl-query nl-text)
  "Process NL query: parse → enrich → map to M-expression → generate events"
  ;; Step 1: Parse using FSM
  (define-values (parse-state events) (parse-query-fsm nl-text))
  
  ;; Step 2: Extract semantic frame
  (define frame (make-semantic-frame))
  
  ;; Step 3: Enrich with knowledge graph (placeholder - would use actual KG)
  (define lattice (empty-lattice))
  (define enriched-frame (enrich-frame frame lattice))
  
  ;; Step 4: Map to M-expression
  (define m-expr-result
    (if (eq? (parse-state-current-state parse-state) 'ParseComplete)
        (map-to-m-expression enriched-frame)
        #f))
  
  ;; Step 5: Broadcast events to Layer 3
  (for ([event (in-list events)])
    (broadcast-parse-event event))
  
  ;; Step 6: Update knowledge graph from events (event-sourced)
  (define kg (empty-knowledge-graph))
  (define updated-kg
    (foldl (lambda (event acc)
             (update-graph-from-event acc event))
           kg
           events))
  
  (values m-expr-result events updated-kg))

