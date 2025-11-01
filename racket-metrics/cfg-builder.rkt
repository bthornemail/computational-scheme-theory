#lang racket

;; Control Flow Graph Builder
;; Builds CFG from AST representation

(require "ast-types.rkt"
         "cfg-types.rkt")

(provide build-cfg)

;; Global state for node ID generation
(define next-node-id 0)

(define (fresh-node-id!)
  (let ([id next-node-id])
    (set! next-node-id (+ id 1))
    id))

;; Main entry point
(define (build-cfg ast)
  (set! next-node-id 0)
  
  (let* ([entry-id (fresh-node-id!)]
         [exit-id (fresh-node-id!)]
         [entry-node (cfg-node entry-id NODE-ENTRY '() #f)]
         [exit-node (cfg-node exit-id NODE-EXIT '() #f)]
         [start-node-id (fresh-node-id!)]
         [start-node (cfg-node start-node-id NODE-BASIC '() #f)])
    
    ;; Build CFG for AST body
    (let-values ([(body-nodes body-edges) (ast->cfg ast start-node-id exit-id)])
      
      ;; Combine everything
      (let ([all-nodes (hash-set* (make-hash)
                                  entry-id entry-node
                                  exit-id exit-node
                                  start-node-id start-node
                                  body-nodes)])
        (cfg entry-id
             exit-id
             all-nodes
             (cons (cfg-edge entry-id start-node-id #f EDGE-NORMAL)
                   (append body-edges
                           (map (λ (n-id) (cfg-edge n-id exit-id #f EDGE-RETURN))
                                (get-exit-nodes body-nodes body-edges))))
             (hash))))))

;; Convert AST to CFG nodes and edges
;; Returns: (values nodes edges)
(define (ast->cfg node entry-id exit-id)
  (match node
    ;; Constant: straight-line code
    [(ast-const loc val)
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id NODE-BASIC (list node) loc))
               '()))]

    ;; Variable: straight-line code
    [(ast-var loc name)
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id NODE-BASIC (list node) loc))
               '()))]

    ;; If: creates branch
    [(ast-if loc test then else)
     (let* ([branch-id (fresh-node-id!)]
            [join-id (fresh-node-id!)])
       
       ;; Build CFG for test, then, else
       (let-values ([(test-nodes test-edges) (ast->cfg test entry-id branch-id)]
                     [(then-nodes then-edges) (ast->cfg then branch-id join-id)]
                     [(else-nodes else-edges) (ast->cfg else branch-id join-id)])
         
         ;; Create branch and join nodes
         (let ([branch-node (cfg-node branch-id NODE-BRANCH (list node) loc)]
               [join-node (cfg-node join-id NODE-JOIN '() loc)])
           
           ;; Combine
           (values (hash-set* (make-hash)
                              branch-id branch-node
                              join-id join-node
                              test-nodes
                              then-nodes
                              else-nodes)
                   (append test-edges
                           then-edges
                           else-edges
                           (list (cfg-edge branch-id (get-first-node-id then-nodes) test EDGE-TRUE)
                                 (cfg-edge branch-id (get-first-node-id else-nodes) test EDGE-FALSE)))))))]

    ;; Lambda: creates nested CFG (could be analyzed separately)
    [(ast-lambda loc params body)
     ;; For simplicity, treat lambda as atomic (no internal CFG)
     ;; Advanced: recursively build CFG for body
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id NODE-BASIC (list node) loc))
               '()))]

    ;; Application: may create cycle if recursive
    [(ast-app loc operator operands)
     (let ([id (fresh-node-id!)])
       (let-values ([(op-nodes op-edges) (ast->cfg operator entry-id exit-id)])
         (let-values ([(arg-nodes arg-edges)
                        (for/fold ([all-nodes '()]
                                   [all-edges '()])
                                  ([arg operands])
                          (let-values ([(n e) (ast->cfg arg entry-id exit-id)])
                            (values (cons n all-nodes)
                                    (append all-edges e))))])
           (values (hash-set* (for/fold ([result (make-hash)])
                                         ([node-hash arg-nodes])
                                 (hash-union result node-hash))
                             id (cfg-node id NODE-BASIC (list node) loc)
                             op-nodes)
                   (append op-edges arg-edges)))))]

    ;; Begin: sequence of statements
    [(ast-begin loc exprs)
     (let-values ([(nodes edges)
                    (for/fold ([all-nodes (make-hash)]
                               [all-edges '()])
                              ([expr exprs])
                        (let-values ([(expr-nodes expr-edges) (ast->cfg expr entry-id exit-id)])
                          (values (hash-union all-nodes expr-nodes)
                                  (append all-edges expr-edges))))])
       (values nodes edges))]

    ;; Let: bindings then body
    [(ast-let loc bindings body)
     (let-values ([(binding-nodes binding-edges)
                    (for/fold ([all-nodes (make-hash)]
                               [all-edges '()])
                              ([binding (map cdr bindings)])
                        (let-values ([(n e) (ast->cfg binding entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e))))]
                  [(body-nodes body-edges)
                    (for/fold ([all-nodes (make-hash)]
                               [all-edges '()])
                              ([expr body])
                        (let-values ([(n e) (ast->cfg expr entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e))))])
       (values (hash-union binding-nodes body-nodes)
               (append binding-edges body-edges)))]

    ;; Letrec: similar to let but with back-edges
    [(ast-letrec loc bindings body)
     (let-values ([(binding-nodes binding-edges)
                    (for/fold ([all-nodes (make-hash)]
                               [all-edges '()])
                              ([binding (map cdr bindings)])
                        (let-values ([(n e) (ast->cfg binding entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e))))]
                  [(body-nodes body-edges)
                    (for/fold ([all-nodes (make-hash)]
                               [all-edges '()])
                              ([expr body])
                        (let-values ([(n e) (ast->cfg expr entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e))))])
       (values (hash-union binding-nodes body-nodes)
               (append binding-edges body-edges)))]
    
    ;; Define: extract the value/body and build CFG for it
    [(ast-define loc name value)
     (ast->cfg value entry-id exit-id)]))

;; Helper: get first node ID from a hash of nodes
(define (get-first-node-id nodes)
  (if (zero? (hash-count nodes))
      (error "No nodes in graph")
      (car (hash-keys nodes))))

;; Helper: get exit nodes (nodes with no outgoing edges)
(define (get-exit-nodes nodes edges)
  (let ([has-outgoing (make-hash)])
    (for ([edge edges])
      (hash-set! has-outgoing (cfg-edge-from edge) #t))
    (filter (λ (node-id) (not (hash-ref has-outgoing node-id #f)))
            (hash-keys nodes))))

;; Helper: union of two hashes
(define (hash-union h1 h2)
  (for/fold ([result h1])
            ([(k v) (in-hash h2)])
    (hash-set result k v)))

