#lang racket/base

(require racket/match
         racket/set
         "cfg-types.rkt")

(provide compute-cyclomatic-complexity
         compute-vg-from-source)

;; ============================================================
;; CYCLOMATIC COMPLEXITY CALCULATOR
;; ============================================================

;; Main entry point: compute V(G) from CFG
;; Formula: V(G) = E - N + 2P
;; where:
;;   E = number of edges
;;   N = number of nodes
;;   P = number of connected components (usually 1)
(define (compute-cyclomatic-complexity cfg)
  "Compute cyclomatic complexity V(G) = E - N + 2P from CFG"
  (let ([nodes (hash-count (cfg-nodes cfg))]
        [edges (length (cfg-edges cfg))])
    
    ;; Compute connected components using DFS
    (let ([components (count-connected-components cfg)])
      
      ;; V(G) = E - N + 2P
      (let ([v-g (+ (- edges nodes) (* 2 components))])
        (complexity-metrics v-g nodes edges components "E - N + 2P")))))

;; Compute V(G) directly from source code
;; Note: This function requires cfg-builder, but we avoid circular dependency
;; by having callers import both modules
(define (compute-vg-from-source cfg)
  "Compute V(G) from CFG (callers should build CFG separately)"
  (compute-cyclomatic-complexity cfg))

;; Count connected components via DFS
(define (count-connected-components cfg)
  "Count connected components in CFG using depth-first search"
  (let ([visited (mutable-set)]
        [component-count 0])
    
    (for ([node-id (hash-keys (cfg-nodes cfg))])
      (unless (set-member? visited node-id)
        (dfs cfg node-id visited)
        (set! component-count (+ component-count 1))))
    
    component-count))

;; Depth-first search
(define (dfs cfg node-id visited)
  "Depth-first search traversal"
  (set-add! visited node-id)
  
  ;; Visit all neighbors
  (for ([edge (cfg-edges cfg)])
    (when (= (cfg-edge-from edge) node-id)
      (let ([to (cfg-edge-to edge)])
        (unless (set-member? visited to)
          (dfs cfg to visited))))))

;; Alternative: compute V(G) by counting decision points + 1
;; V(G) = number of decision points + 1
;; This is equivalent to E - N + 2P for a connected graph
(define (compute-cyclomatic-by-decisions cfg)
  "Alternative: compute V(G) by counting decision points"
  (let ([decision-count 0])
    (for ([(node-id node) (in-hash (cfg-nodes cfg))])
      (when (eq? (cfg-node-type node) BRANCH-NODE)
        (set! decision-count (+ decision-count 1))))
    (+ decision-count 1)))

