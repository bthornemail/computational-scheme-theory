#lang racket/base

(require racket/match
         racket/string
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
  ;; Parse query using grammar parser first to get semantic frame
  (define-values (parsed-frame frame2) (parse-query nl-query))
  
  ;; Use parsed frame or create default frame
  (define frame (if (semantic-frame? parsed-frame) parsed-frame (make-semantic-frame)))
  
  ;; Extract intent-type and concepts from query if not in frame
  (define intent-type
    (or (semantic-frame-intent-type frame)
        (let ([words (string-split (string-downcase nl-query))])
          (ormap (lambda (verb)
                   (if (member verb words) verb #f))
                 '("compute" "validate" "analyze" "compare" "export" "get")))))
  
  ;; Extract objects/concepts from query
  (define concepts
    (if (not (null? (semantic-frame-concepts frame)))
        (semantic-frame-concepts frame)
        (let ([words (string-split (string-downcase nl-query))])
          (append
           (if (ormap (lambda (w) (string-prefix? w "h1")) words)
               '((object "H1"))
               '())
           (if (ormap (lambda (w) (or (equal? w "vg") (string-prefix? w "v(g)"))) words)
               '((object "V(G)"))
               '())
           (if (member "polynomial" words)
               '((object "polynomial"))
               '())
           (if (member "pattern" words)
               '((object "pattern"))
               '())))))
  
  ;; Create frame with extracted information
  (define enriched-frame-base
    (if intent-type
        (semantic-frame concepts
                       (semantic-frame-relationships frame)
                       (semantic-frame-modifiers frame)
                       intent-type)
        frame))
  
  (define enriched-frame (enrich-frame enriched-frame-base (empty-lattice)))
  
  ;; Map to M-expression
  (map-to-m-expression enriched-frame))

;; Dispatch parsed intent as command to Layer 4
(define (dispatch-parsed-intent intent)
  "Dispatch parsed intent as command to Layer 4 (unidirectional flow)"
  (void))  ; Will integrate with unified-pipeline.rkt
