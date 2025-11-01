#lang racket/base

(require racket/match
         racket/set
         "semantic-lattice.rkt"
         "semantic-frame.rkt")

(provide
 compute-subsumption-path
 resolve-query-intent)

;; ============================================================
;; LATTICE OPERATIONS - Inference Operations
;; ============================================================

;; Compute subsumption path (transitive closure)
(define (compute-subsumption-path graph from-id to-id)
  "Compute subsumption path from from-id to to-id"
  (define nodes (semantic-lattice-nodes graph))
  ;; Simple implementation - find path via DFS
  (define visited (set))
  (define (dfs current-id target-id path)
    (if (eq? current-id target-id)
        (reverse (cons current-id path))
        (if (set-member? visited current-id)
            #f
            (let* ([visited (set-add visited current-id)]
                   [node (for/first ([n (in-set nodes)]
                                     #:when (eq? (lattice-node-id n) current-id))
                            n)])
              (if node
                  (ormap (lambda (child-id)
                           (dfs child-id target-id (cons current-id path)))
                         (lattice-node-children node))
                  #f)))))
  (dfs from-id to-id '()))

;; Resolve query intent using lattice
(define (resolve-query-intent lattice intent-frame)
  "Resolve query intent by computing join in lattice"
  (define concepts (semantic-frame-concepts intent-frame))
  
  (cond
    [(null? concepts)
     #f]
    [(= (length concepts) 1)
     ;; Single concept - find direct operation
     (find-operation-for-concept lattice (car concepts))]
    [else
     ;; Multiple concepts - compute join to find common operation
     (let ([operation (find-join-for-concepts lattice concepts)])
       operation)]))

;; Helper: Find operation for single concept
(define (find-operation-for-concept lattice concept)
  "Find operation for single concept"
  (match concept
    [`(action-verb "compute")
     'compute-operation]
    [`(action-verb "validate")
     'validate-operation]
    [`(object "H1")
     'compute-h1]
    [`(object "V(G)")
     'compute-vg]
    [else
     #f]))

;; Helper: Find join for multiple concepts
(define (find-join-for-concepts lattice concepts)
  "Find join (least upper bound) for multiple concepts"
  (define concept-ids (map concept-to-id concepts))
  (if (= (length concept-ids) 2)
      (find-join lattice (car concept-ids) (cadr concept-ids))
      #f))

;; Helper: Convert concept to ID
(define (concept-to-id concept)
  "Convert concept to lattice node ID"
  (match concept
    [`(action-verb ,verb)
     (string->symbol verb)]
    [`(object ,obj)
     (string->symbol obj)]
    [`(entity ,type ,id)
     (string->symbol (format "~a-~a" type id))]
    [else
     #f]))

