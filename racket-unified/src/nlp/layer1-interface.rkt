#lang racket/base

(require racket/match
         "../m-expression.rkt"
         "grammar-parser.rkt"
         "parsing-fsm.rkt"
         "semantic-frame.rkt"
         "intent-mapper.rkt"
         "semantic-lattice.rkt"
         "knowledge-graph.rkt")

(provide
 nl-to-m-expression
 dispatch-parsed-intent)

;; ============================================================
;; LAYER 1 INTERFACE - UI Layer Integration
;; ============================================================

;; Convert natural language query to M-expression
(define (nl-to-m-expression nl-query)
  "Accept NL query and convert to M-expression (UDF pattern)"
  ;; Parse query using FSM
  (define-values (parse-state events) (parse-query-fsm nl-query))
  
  ;; Extract semantic frame from parsing
  (define frame (make-semantic-frame))
  (define enriched-frame (enrich-frame frame (empty-lattice)))
  
  ;; Map to M-expression
  (map-to-m-expression enriched-frame))

;; Dispatch parsed intent as command to Layer 4
(define (dispatch-parsed-intent intent)
  "Dispatch parsed intent as command to Layer 4 (unidirectional flow)"
  (void))  ; Will integrate with unified-pipeline.rkt
