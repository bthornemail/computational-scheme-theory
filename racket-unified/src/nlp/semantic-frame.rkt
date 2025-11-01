#lang racket/base

(require racket/match
         racket/list)

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
  "Enrich semantic frame with lattice data"
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
  
  ;; Extract parent concepts (simplified - would use actual lattice lookups)
  (define parent-concepts
    (map (lambda (concept)
           (match concept
             [`(action-verb ,verb)
              '("Operation" "ProgramOperation")]
             [`(object ,obj)
              '("Metric" "ComplexityMetric")]
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

