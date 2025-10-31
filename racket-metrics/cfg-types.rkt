#lang racket

;; Control Flow Graph (CFG) Data Structures
;; Represents the control flow structure of a Scheme program

(provide (all-defined-out))

;; CFG node represents a basic block
(struct cfg-node
  (id            ; Unique node identifier
   type          ; 'entry | 'exit | 'basic | 'branch | 'join
   statements    ; List of non-branching statements
   source-loc)   ; Source location for debugging
  #:transparent)

;; CFG edge represents control flow
(struct cfg-edge
  (from          ; Source node ID
   to            ; Target node ID
   condition     ; #f for unconditional, expression for conditional
   type)         ; 'normal | 'true-branch | 'false-branch | 'return | 'back
  #:transparent)

;; Complete control flow graph
(struct cfg
  (entry         ; Entry node ID
   exit          ; Exit node ID (may be multiple for continuations)
   nodes         ; Hash: node-id -> cfg-node
   edges         ; List of cfg-edge
   metadata)     ; Additional info (function name, etc.)
  #:transparent)

;; Cyclomatic complexity result
(struct complexity-metrics
  (v-g           ; Cyclomatic complexity V(G)
   nodes         ; Number of nodes N
   edges         ; Number of edges E
   components    ; Connected components P
   formula)      ; Which formula used: "E - N + 2P"
  #:transparent)

;; Node types
(define NODE-ENTRY 'entry)
(define NODE-EXIT 'exit)
(define NODE-BASIC 'basic)
(define NODE-BRANCH 'branch)
(define NODE-JOIN 'join)

;; Edge types
(define EDGE-NORMAL 'normal)
(define EDGE-TRUE 'true-branch)
(define EDGE-FALSE 'false-branch)
(define EDGE-RETURN 'return)
(define EDGE-BACK 'back)

