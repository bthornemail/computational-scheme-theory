#lang racket/base

(provide
 (all-from-out "grammar-parser.rkt")
 (all-from-out "parsing-fsm.rkt")
 (all-from-out "parse-events.rkt")
 (all-from-out "semantic-lattice.rkt")
 (all-from-out "knowledge-graph.rkt")
 (all-from-out "lattice-ops.rkt")
 (all-from-out "semantic-frame.rkt")
 (all-from-out "intent-mapper.rkt")
 (all-from-out "domain-mappings.rkt")
 (all-from-out "learning-engine.rkt")
 (all-from-out "context-manager.rkt")
 (all-from-out "layer1-interface.rkt")
 (all-from-out "layer2-query.rkt")
 (all-from-out "layer3-coordination.rkt")
 (all-from-out "layer4-core.rkt"))

(require "grammar-parser.rkt"
         "parsing-fsm.rkt"
         "parse-events.rkt"
         "semantic-lattice.rkt"
         "knowledge-graph.rkt"
         "lattice-ops.rkt"
         "semantic-frame.rkt"
         "intent-mapper.rkt"
         "domain-mappings.rkt"
         "learning-engine.rkt"
         "context-manager.rkt"
         "layer1-interface.rkt"
         "layer2-query.rkt"
         "layer3-coordination.rkt"
         "layer4-core.rkt")

;; ============================================================
;; NLP MAIN - Unified Export for SGP-ASLN
;; ============================================================

;; Main entry point for NLP processing
(define (process-natural-language-query nl-text)
  "Main entry point: process natural language query through full pipeline"
  (process-nl-query nl-text))

