#lang racket/base

(require racket/match
         racket/set
         racket/list
         "algorithm2.rkt")

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

;; Compute nerve of open cover
(define (compute-nerve open-cover)
  "Compute nerve N(U) of open cover"
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
      (when (have-overlap region1 region2)
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
      (when (and (have-overlap region1 region2)
                 (have-overlap region2 region3)
                 (have-overlap region1 region3))
        (set-add! simplices2 (simplex (set i j k))))))
  
  (simplicial-complex simplices0 simplices1 simplices2))

;; Check if two regions overlap
(define (have-overlap region1 region2)
  "Check if two visibility regions have non-empty intersection"
  (if (and (visibility-region? region1) (visibility-region? region2))
      (let ([scopes1 (visibility-region-scopes region1)]
            [bindings1 (visibility-region-bindings region1)]
            [scopes2 (visibility-region-scopes region2)]
            [bindings2 (visibility-region-bindings region2)])
        ;; Overlap if they share scopes or bindings
        (or (not (set-empty? (set-intersect scopes1 scopes2)))
            (not (set-empty? (set-intersect bindings1 bindings2)))))
      #f))

;; Build Čech complex from topology
(define (build-cech-complex topology)
  "Build complete Čech complex from topology"
  (let ([open-cover (build-open-cover topology)])
    (compute-nerve open-cover)))

;; Get simplices of given dimension
(define (get-simplices complex dim)
  "Extract simplices of dimension dim"
  (cond
    [(= dim 0) (simplicial-complex-0 complex)]
    [(= dim 1) (simplicial-complex-1 complex)]
    [(= dim 2) (simplicial-complex-2 complex)]
    [else (set)]))

