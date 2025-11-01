#lang racket/base

(require racket/match
         racket/set
         "../s-expression.rkt"
         "semantic-lattice.rkt"
         "semantic-frame.rkt")

(provide
 knowledge-graph
 knowledge-graph?
 knowledge-graph-vertices
 knowledge-graph-edges
 knowledge-graph-labels
 empty-knowledge-graph
 add-concept
 infer-parents
 enrich-semantic-frame
 update-graph-from-event)

;; ============================================================
;; KNOWLEDGE GRAPH - Persistent Graph Representation
;; ============================================================

;; Knowledge graph structure: (V, E, L)
(struct knowledge-graph (vertices edges labels) #:transparent)

;; Create empty knowledge graph
(define (empty-knowledge-graph)
  "Create empty knowledge graph"
  (knowledge-graph (set) (set) (make-hash)))

;; Add concept to knowledge graph
(define (add-concept graph concept-type name parents)
  "Add concept to knowledge graph"
  (define vertex-id (gensym (string->symbol (format "~a-~a" concept-type name))))
  (define vertex (lattice-node vertex-id concept-type
                               `((name ,name))
                               parents
                               '()))
  
  (define new-vertices (set-add (knowledge-graph-vertices graph) vertex))
  
  ;; Add parent edges with "subsumes" label
  (define new-edges
    (foldl (lambda (parent-id edges)
             (set-add edges (list vertex-id parent-id 'subsumes)))
           (knowledge-graph-edges graph)
           parents))
  
  ;; Update labels
  (define new-labels (hash-copy (knowledge-graph-labels graph)))
  (hash-set! new-labels vertex-id concept-type)
  
  (knowledge-graph new-vertices new-edges new-labels))

;; Infer parent concepts for a node
(define (infer-parents node existing-nodes)
  "Infer parent concepts based on subsumption"
  (define node-type (lattice-node-type node))
  (filter (lambda (existing-node)
            (subsumes?-helper existing-node node))
          (set->list existing-nodes)))

;; Helper: Check subsumption
(define (subsumes?-helper parent-node child-node)
  "Check if parent-node subsumes child-node"
  (define parent-type (lattice-node-type parent-node))
  (define child-type (lattice-node-type child-node))
  
  ;; Simple type-based subsumption (can be extended)
  (or (eq? parent-type child-type)
      (member child-type (lattice-node-parents parent-node))))

;; Enrich semantic frame with knowledge graph information
(define (enrich-semantic-frame frame graph)
  "Enrich semantic frame with knowledge graph data"
  (define vertices (knowledge-graph-vertices graph))
  (define concepts (semantic-frame-concepts frame))
  
  (define enriched-concepts
    (map (lambda (concept)
           (let ([concept-name (if (pair? concept) (cadr concept) concept)])
             (find-concept-in-graph concept-name vertices)))
         concepts))
  
  ;; Create enhanced frame (will be defined in semantic-frame.rkt)
  (struct-copy semantic-frame frame
               [concepts enriched-concepts]))

;; Helper: Find concept in graph
(define (find-concept-in-graph concept-name vertices)
  "Find concept node in graph vertices"
  (for/first ([vertex (in-set vertices)]
              #:when (equal? (lattice-node-type vertex) concept-name))
    vertex))

;; Update graph from parse event (event-sourced updates)
(define (update-graph-from-event graph event)
  "Update knowledge graph from parse event (immutable S-expression)"
  (match (s-expr-type event)
    ['query-parsed
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'semantic-frame event-data))
           (let ([semantic-frame (cadr (assoc 'semantic-frame event-data))])
             (add-concepts-from-frame graph semantic-frame))
           graph))]
    ['verb-parsed
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'verb event-data))
           (let ([verb (cadr (assoc 'verb event-data))])
             (add-concept graph 'action-verb verb '("ProgramOperation")))
           graph))]
    ['entity-resolved
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'entity event-data))
           (let ([entity (cadr (assoc 'entity event-data))])
             (add-concept graph 'entity entity '("ProgramEntity")))
           graph))]
    [else
     graph]))

;; Helper: Add concepts from semantic frame
(define (add-concepts-from-frame graph frame)
  "Add concepts from semantic frame to knowledge graph"
  (define concepts (semantic-frame-concepts frame))
  (foldl (lambda (concept g)
           (match concept
             [`(action-verb ,verb)
              (add-concept g 'action-verb verb '("Operation"))]
             [`(object ,obj)
              (add-concept g 'object obj '("Metric"))]
             [`(entity ,type ,id)
              (add-concept g 'entity (format "~a-~a" type id) (list type))]
             [else g]))
         graph
         concepts))

