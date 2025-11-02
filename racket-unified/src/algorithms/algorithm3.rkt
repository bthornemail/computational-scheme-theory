#lang racket/base

(require racket/match
         racket/set
         racket/list
         "algorithm2.rkt"
         "dependency-graph.rkt")

(provide
 simplicial-complex
 simplicial-complex?
 simplicial-complex-simplices0
 simplicial-complex-simplices1
 simplicial-complex-simplices2
 simplex
 simplex?
 simplex-vertices
 compute-nerve
 build-cech-complex
 get-simplices
 have-overlap
 simplicial-complex-0
 simplicial-complex-1
 simplicial-complex-2)

;; Simplicial complex: collection of simplices of different dimensions
(struct simplicial-complex (simplices0 simplices1 simplices2) #:transparent #:mutable)

;; Convenience accessors (Racket struct accessors use full name)
(define simplicial-complex-0 simplicial-complex-simplices0)
(define simplicial-complex-1 simplicial-complex-simplices1)
(define simplicial-complex-2 simplicial-complex-simplices2)

;; ============================================================
;; ALGORITHM 3: ČECH COMPLEX CONSTRUCTION
;; ============================================================

;; Simplex: set of indices (for nerve of open cover)
(struct simplex (vertices) #:transparent)

;; Compute nerve of open cover (with optional dependency graph)
(define (compute-nerve open-cover #:dependency-graph [dep-graph #f])
  "Compute nerve N(U) of open cover, optionally using dependency graph for cycle detection"
  (define cover-list (hash->list open-cover))
  (define n (length cover-list))
  (define indices (range n))
  
  ;; 0-simplices: one per open set
  (define simplices0 (list->set indices))
  
  ;; 1-simplices: pairs with non-empty intersection
  (define simplices1 (mutable-set))
  (for* ([i indices]
         [j indices]
         #:when (< i j))
    (let ([region1 (cdr (list-ref cover-list i))]
          [region2 (cdr (list-ref cover-list j))])
      (when (have-overlap region1 region2 #:dependency-graph dep-graph)
        (set-add! simplices1 (simplex (set i j))))))
  
  ;; 2-simplices: triples with non-empty intersection
  (define simplices2 (mutable-set))
  (for* ([i indices]
         [j indices]
         [k indices]
         #:when (and (< i j) (< j k)))
    (let ([region1 (cdr (list-ref cover-list i))]
          [region2 (cdr (list-ref cover-list j))]
          [region3 (cdr (list-ref cover-list k))])
      (when (and (have-overlap region1 region2 #:dependency-graph dep-graph)
                 (have-overlap region2 region3 #:dependency-graph dep-graph)
                 (have-overlap region1 region3 #:dependency-graph dep-graph))
        (set-add! simplices2 (simplex (set i j k))))))
  
  (simplicial-complex simplices0 simplices1 simplices2))

;; Check if two regions overlap (with optional dependency graph for cycle detection)
(define (have-overlap region1 region2 #:dependency-graph [dep-graph #f])
  "Check if two visibility regions have non-empty intersection.
   Enhanced version: checks expanded scope-ids from enhanced-visibility-region
   to capture transitive scope relationships and closure captures.
   Also checks dependency cycles if dependency graph is provided."
  (define base-overlap
    (cond
    [(and (visibility-region? region1) (visibility-region? region2))
     ;; Basic visibility regions: check direct scope or binding intersection
     (let ([scopes1 (visibility-region-scopes region1)]
           [bindings1 (visibility-region-bindings region1)]
           [scopes2 (visibility-region-scopes region2)]
           [bindings2 (visibility-region-bindings region2)])
       ;; Overlap if they share scopes or bindings
       (or (and (set? scopes1) (set? scopes2) (not (set-empty? (set-intersect scopes1 scopes2))))
           (and (set? bindings1) (set? bindings2) (not (set-empty? (set-intersect bindings1 bindings2))))))]
    [(and (enhanced-visibility-region? region1) (enhanced-visibility-region? region2))
     ;; Enhanced visibility regions: check both base region and expanded scope-ids
     ;; This captures transitive scope relationships (parent-child, ancestor-descendant)
     (let ([scope-ids1 (enhanced-visibility-region-scope-ids region1)]
           [scope-ids2 (enhanced-visibility-region-scope-ids region2)]
           [base1 (enhanced-visibility-region-base-region region1)]
           [base2 (enhanced-visibility-region-base-region region2)])
       ;; Check expanded scope-ids intersection (includes descendant scopes)
       (define scope-overlap (and (set? scope-ids1) (set? scope-ids2) 
                                  (not (set-empty? (set-intersect scope-ids1 scope-ids2)))))
       ;; Also check base region for binding overlaps
       (define base-overlap (have-overlap base1 base2 #:dependency-graph dep-graph))
       ;; Overlap if either expanded scopes overlap OR base regions overlap
       (or scope-overlap base-overlap))]
    [(enhanced-visibility-region? region1)
     ;; Mixed case: enhanced vs basic - check base region of enhanced
     (let ([base1 (enhanced-visibility-region-base-region region1)])
       (have-overlap base1 region2 #:dependency-graph dep-graph))]
    [(enhanced-visibility-region? region2)
     ;; Mixed case: basic vs enhanced - check base region of enhanced
     (let ([base2 (enhanced-visibility-region-base-region region2)])
       (have-overlap base2 region1 #:dependency-graph dep-graph))]
    [else #f]))
  
  ;; Also check dependency cycles if graph provided
  (or base-overlap
      (if dep-graph
          (dependency-cycle-overlap region1 region2 dep-graph)
          #f)))

;; Check if two regions overlap due to dependency cycle
(define (dependency-cycle-overlap region1 region2 dep-graph)
  "Check if two visibility regions overlap due to dependency cycle"
  (let ([bindings1 (cond
                    [(visibility-region? region1)
                     (visibility-region-bindings region1)]
                    [(enhanced-visibility-region? region1)
                     (visibility-region-bindings (enhanced-visibility-region-base-region region1))]
                    [else (set)])]
        [bindings2 (cond
                    [(visibility-region? region2)
                     (visibility-region-bindings region2)]
                    [(enhanced-visibility-region? region2)
                     (visibility-region-bindings (enhanced-visibility-region-base-region region2))]
                    [else (set)])])
    ;; Check if any bindings from region1 and region2 are in a dependency cycle
    (for/or ([b1 (in-set bindings1)])
      (for/or ([b2 (in-set bindings2)])
        (in-dependency-cycle? b1 b2 dep-graph)))))

;; Build Čech complex from topology (with optional dependency graph)
(define (build-cech-complex topology #:dependency-graph [dep-graph #f])
  "Build complete Čech complex from topology, optionally using dependency graph"
  (let ([open-cover (build-open-cover topology)])
    (compute-nerve open-cover #:dependency-graph dep-graph)))

;; Get simplices of given dimension
(define (get-simplices complex dim)
  "Extract simplices of dimension dim"
  (cond
    [(= dim 0) (simplicial-complex-0 complex)]
    [(= dim 1) (simplicial-complex-1 complex)]
    [(= dim 2) (simplicial-complex-2 complex)]
    [else (set)]))

