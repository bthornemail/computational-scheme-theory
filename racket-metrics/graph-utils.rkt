#lang racket

;; Graph Utility Functions
;; Helper functions for graph algorithms

(require "cfg-types.rkt")

(provide count-connected-components)

;; Count connected components using DFS
;; This is a duplicate from cyclomatic.rkt but kept here for utility
(define (count-connected-components cfg)
  (let ([visited (mutable-set)]
        [component-count 0])
    
    (for ([node-id (hash-keys (cfg-nodes cfg))])
      (unless (set-member? visited node-id)
        (dfs cfg node-id visited)
        (set! component-count (+ component-count 1))))
    
    component-count))

;; Depth-first search helper
(define (dfs cfg node-id visited)
  (set-add! visited node-id)
  
  (for ([edge (cfg-edges cfg)])
    (when (= (cfg-edge-from edge) node-id)
      (let ([to (cfg-edge-to edge)])
        (unless (set-member? visited to)
          (dfs cfg to visited))))))

