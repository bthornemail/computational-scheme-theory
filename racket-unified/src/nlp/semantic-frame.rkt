#lang racket/base

(require racket/match
         racket/list
         racket/set
         "semantic-lattice.rkt"
         "semantic-lexicon.rkt")

(provide
 semantic-frame
 semantic-frame?
 semantic-frame-concepts
 semantic-frame-relationships
 semantic-frame-modifiers
 semantic-frame-intent-type
 make-semantic-frame
 enhanced-frame
 enhanced-frame?
 enhanced-frame-frame
 enhanced-frame-types
 enhanced-frame-parent-concepts
 enhanced-frame-inferred-relations
 enrich-frame)

;; ============================================================
;; SEMANTIC FRAME - Parsed Query Structure
;; ============================================================

;; Semantic frame structure (from grammar parser)
(struct semantic-frame (concepts relationships modifiers intent-type) #:transparent)

;; Enhanced frame with lattice enrichment
(struct enhanced-frame (frame types parent-concepts inferred-relations) #:transparent)

;; Create empty semantic frame
(define (make-semantic-frame)
  "Create empty semantic frame"
  (semantic-frame '() '() '() #f))

;; Enrich frame with lattice information
(define (enrich-frame frame lattice)
  "Enrich semantic frame with lattice data (enhanced with domain knowledge)"
  ;; Use domain lattice if provided, otherwise build default
  (define domain-lattice 
    (if (set-empty? (semantic-lattice-nodes lattice))
        (initialize-domain-knowledge)
        lattice))
  
  (define concepts (semantic-frame-concepts frame))
  
  ;; Extract type information from concepts
  (define types
    (map (lambda (concept)
           (match concept
             [`(action-verb ,verb)
              (list 'action-verb verb)]
             [`(object ,obj)
              (list 'object obj)]
             [`(entity ,type ,id)
              (list 'entity type)]
             [else
              (list 'unknown concept)]))
         concepts))
  
  ;; Extract parent concepts using domain lattice
  (define parent-concepts
    (map (lambda (concept)
           (match concept
             [`(action-verb ,verb)
              (let ([node (find-concept-in-lattice domain-lattice verb)])
                (if node
                    (map (lambda (p-id) 
                           (let ([p-node (find-concept-in-lattice domain-lattice p-id)])
                             (if p-node (symbol->string (lattice-node-id p-node)) "")))
                         (lattice-node-parents node))
                    '("Operation" "ProgramOperation")))]
             [`(object ,obj)
              (let ([node (find-concept-in-lattice domain-lattice obj)])
                (if node
                    (map (lambda (p-id)
                           (let ([p-node (find-concept-in-lattice domain-lattice p-id)])
                             (if p-node (symbol->string (lattice-node-id p-node)) "")))
                         (lattice-node-parents node))
                    '("Metric" "ComplexityMetric")))]
             [`(entity ,type ,id)
              (list type)]
             [else
              '()]))
         concepts))
  
  ;; Infer relations (simplified)
  (define inferred-relations
    (if (and (memq 'action-verb (map car types))
             (memq 'object (map car types)))
        '((operation-target (action-verb object)))
        '()))
  
  (enhanced-frame frame types parent-concepts inferred-relations))

