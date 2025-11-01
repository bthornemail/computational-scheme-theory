#lang racket/base

(require racket/match
         racket/set)

(provide
 cfg-node
 cfg-node?
 cfg-node-id
 cfg-node-type
 cfg-node-statements
 cfg-node-source-loc
 cfg-edge
 cfg-edge?
 cfg-edge-from
 cfg-edge-to
 cfg-edge-condition
 cfg-edge-type
 cfg
 cfg?
 cfg-entry
 cfg-exit
 cfg-nodes
 cfg-edges
 cfg-metadata
 complexity-metrics
 complexity-metrics?
 complexity-metrics-v-g
 complexity-metrics-nodes
 complexity-metrics-edges
 complexity-metrics-components
 make-empty-cfg
 add-cfg-node
 add-cfg-edge)

;; ============================================================
;; CFG TYPES - Control Flow Graph Data Structures
;; ============================================================

;; CFG node types
(define ENTRY-NODE 'entry)
(define EXIT-NODE 'exit)
(define BASIC-NODE 'basic)
(define BRANCH-NODE 'branch)
(define JOIN-NODE 'join)

;; CFG node structure
(struct cfg-node (id type statements source-loc) #:transparent)

;; CFG edge structure
(struct cfg-edge (from to condition type) #:transparent)

;; Edge types
(define EDGE-NORMAL 'normal)
(define EDGE-TRUE-BRANCH 'true-branch)
(define EDGE-FALSE-BRANCH 'false-branch)
(define EDGE-RETURN 'return)
(define EDGE-BACK 'back)  ; For loops/recursion

;; Complete control flow graph
(struct cfg (entry exit nodes edges metadata) #:transparent)

;; Cyclomatic complexity metrics
(struct complexity-metrics (v-g nodes edges components formula) #:transparent)

;; Create empty CFG
(define (make-empty-cfg)
  "Create empty control flow graph"
  (cfg #f #f (make-hash) '() (make-hash)))

;; Add node to CFG
(define (add-cfg-node graph node-id node)
  "Add node to control flow graph"
  (let* ([nodes (cfg-nodes graph)]
         [new-nodes (hash-set nodes node-id node)])
    (struct-copy cfg graph [nodes new-nodes])))

;; Add edge to CFG
(define (add-cfg-edge graph edge)
  "Add edge to control flow graph"
  (let* ([current-edges (cfg-edges graph)]
         [new-edges (cons edge current-edges)])
    (struct-copy cfg graph [edges new-edges])))

;; Export node types
(provide
 ENTRY-NODE
 EXIT-NODE
 BASIC-NODE
 BRANCH-NODE
 JOIN-NODE
 EDGE-NORMAL
 EDGE-TRUE-BRANCH
 EDGE-FALSE-BRANCH
 EDGE-RETURN
 EDGE-BACK)

