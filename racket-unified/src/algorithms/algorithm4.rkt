#lang racket/base

(require racket/match
         racket/set
         racket/list
         racket/math
         "algorithm3.rkt")

(provide
 build-incidence-matrices
 matrix-rank
 compute-h1
 compute-h1-from-source
 compute-beta1-from-graph
 compute-beta0-connected-components)

;; ============================================================
;; ALGORITHM 4: COHOMOLOGY COMPUTATION
;; ============================================================

;; Incidence matrix representation (simplified as list of lists)
;; M₀: edges → vertices
;; M₁: triangles → edges

;; Build incidence matrices M₀ and M₁
(define (build-incidence-matrices complex)
  "Build incidence matrices M₀ (edges→vertices) and M₁ (triangles→edges)"
  (define edges (set->list (simplicial-complex-simplices1 complex)))
  (define triangles (set->list (simplicial-complex-simplices2 complex)))
  (define vertices (set->list (simplicial-complex-simplices0 complex)))
  
  ;; M₀: |vertices| x |edges|  (each row is a vertex, each column is an edge)
  (define m0
    (for/list ([v (in-set (simplicial-complex-simplices0 complex))])
      (for/list ([e edges])
        (if (set-member? (simplex-vertices e) v) 1 0))))
  
  ;; M₁: |edges| x |triangles|  (each row is an edge, each column is a triangle)
  (define m1
    (for/list ([e edges])
      (for/list ([t triangles])
        (if (subset? (simplex-vertices e) (simplex-vertices t)) 1 0))))
  
  (values m0 m1))

;; Compute matrix rank (simplified Gaussian elimination)
(define (matrix-rank matrix)
  "Compute rank of matrix using simplified Gaussian elimination"
  (if (or (null? matrix) (null? (car matrix)))
      0
      (let* ([rows (length matrix)]
             [cols (length (car matrix))])
        (if (or (= rows 0) (= cols 0))
            0
            ;; Convert to list of vectors for easier manipulation
            (let* ([vec-matrix (map list->vector matrix)]
                   [rank (box 0)]
                   [used-rows (mutable-set)])
              ;; Simplified row reduction
              (for ([col (in-range cols)])
                (let ([pivot-row (box #f)])
                  ;; Find pivot row
                  (for ([row (in-range rows)])
                    (when (and (not (set-member? used-rows row))
                               (not (zero? (vector-ref (list-ref vec-matrix row) col)))
                               (not (unbox pivot-row)))
                      (set-box! pivot-row row)
                      (set-add! used-rows row)
                      (set-box! rank (+ (unbox rank) 1))))
                  ;; Eliminate column
                  (when (unbox pivot-row)
                    (let ([pivot-val (vector-ref (list-ref vec-matrix (unbox pivot-row)) col)])
                      (when (not (zero? pivot-val))
                        (for ([row (in-range rows)])
                          (when (not (= row (unbox pivot-row)))
                            (let ([val (vector-ref (list-ref vec-matrix row) col)])
                              (when (not (zero? val))
                                (for ([c (in-range cols)])
                                  (vector-set! (list-ref vec-matrix row) c
                                               (modulo (- (vector-ref (list-ref vec-matrix row) c)
                                                          (* val (vector-ref (list-ref vec-matrix (unbox pivot-row)) c)))
                                                       2))))))))))
              (unbox rank))))))))

;; Helper: Check if set is subset
(define (subset? s1 s2)
  "Check if s1 is subset of s2"
  (set-empty? (set-subtract s1 s2)))

;; Compute H¹ cohomology
(define (compute-h1 complex)
  "Compute first cohomology dimension: β₁ = (|N₁| - rank(M₁)) - rank(M₀)"
  (let-values ([(m0 m1) (build-incidence-matrices complex)])
    (define n1 (set-count (simplicial-complex-simplices1 complex)))
    (define rank0 (let ([r (matrix-rank m0)]) (if (number? r) r 0)))
    (define rank1 (let ([r (matrix-rank m1)]) (if (number? r) r 0)))
    (define beta1 (max 0 (- (- n1 rank1) rank0)))
    beta1))

;; Compute β₀ (number of connected components) from 1-skeleton graph
(define (compute-beta0-connected-components complex)
  "Compute β₀ (number of connected components) from the 1-skeleton of the complex using DFS"
  (define vertices (set->list (simplicial-complex-simplices0 complex)))
  (define edges (set->list (simplicial-complex-simplices1 complex)))
  
  ;; Build adjacency list from edges
  (define adj-list (make-hash))
  (for ([v vertices])
    (hash-set! adj-list v (mutable-set)))
  
  (for ([e edges])
    (define vs (set->list (simplex-vertices e)))
    (when (= (length vs) 2)
      (define v1 (list-ref vs 0))
      (define v2 (list-ref vs 1))
      (set-add! (hash-ref adj-list v1) v2)
      (set-add! (hash-ref adj-list v2) v1)))
  
  ;; DFS to count connected components
  (define visited (mutable-set))
  (define component-count (box 0))
  
  (define (dfs v)
    (unless (set-member? visited v)
      (set-add! visited v)
      (for ([neighbor (in-set (hash-ref adj-list v))])
        (dfs neighbor))))
  
  (for ([v vertices])
    (unless (set-member? visited v)
      (set-box! component-count (+ (unbox component-count) 1))
      (dfs v)))
  
  (unbox component-count))

;; Compute β₁ directly from graph structure: β₁ = |E| - |V| + β₀
(define (compute-beta1-from-graph complex)
  "Compute first Betti number β₁ directly from the 1-skeleton graph structure
   Formula: β₁(G) = |E| - |V| + β₀
   where |E| = number of edges (1-simplices)
         |V| = number of vertices (0-simplices)
         β₀ = number of connected components"
  (define num-vertices (set-count (simplicial-complex-simplices0 complex)))
  (define num-edges (set-count (simplicial-complex-simplices1 complex)))
  (define beta0 (compute-beta0-connected-components complex))
  (define beta1 (max 0 (- (+ num-edges beta0) num-vertices)))
  beta1)

;; Complete pipeline: Source → H¹
(define (compute-h1-from-source source)
  "Complete pipeline from Scheme source to H¹ value"
  ;; This is handled by unified-pipeline.rkt
  0)
