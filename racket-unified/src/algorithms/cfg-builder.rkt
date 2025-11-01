#lang racket/base

(require racket/match
         racket/set
         racket/hash
         "algorithm1.rkt"
         "cfg-types.rkt")

(provide build-cfg-from-ast build-cfg-from-source)

;; ============================================================
;; CFG BUILDER - Build Control Flow Graph from AST
;; ============================================================

;; Global state for node ID generation
(define next-node-id (box 0))

(define (fresh-node-id!)
  "Generate fresh node ID"
  (let ([id (unbox next-node-id)])
    (set-box! next-node-id (+ id 1))
    id))

(define (reset-node-id!)
  "Reset node ID counter"
  (set-box! next-node-id 0))

;; Main entry point: build CFG from source code
(define (build-cfg-from-source source)
  "Build CFG from Scheme source code"
  (let* ([ast-nodes (parse-r5rs source)]
         [main-ast (if (null? ast-nodes)
                      (ast-const (source-loc "" 1 1) #f)
                      (if (= (length ast-nodes) 1)
                          (car ast-nodes)
                          (ast-app (source-loc "" 1 1)
                                  (ast-var (source-loc "" 1 1) 'begin)
                                  ast-nodes)))])
    (build-cfg-from-ast main-ast)))

;; Main entry point: build CFG from AST
(define (build-cfg-from-ast ast)
  "Build control flow graph from AST"
  (reset-node-id!)
  
  (let* ([entry-id (fresh-node-id!)]
         [exit-id (fresh-node-id!)]
         [entry-node (cfg-node entry-id ENTRY-NODE '() #f)]
         [exit-node (cfg-node exit-id EXIT-NODE '() #f)]
         [start-node-id (fresh-node-id!)]
         [start-node (cfg-node start-node-id BASIC-NODE '() #f)])
    
    ;; Build CFG for AST body
    (let-values ([(body-nodes body-edges start-nodes) (ast->cfg ast start-node-id exit-id)])
      
      ;; Find all nodes that should connect to exit
      (let ([exit-candidates (get-exit-nodes body-nodes body-edges)])
        (let ([exit-edges (if (null? exit-candidates)
                             (list (cfg-edge start-node-id exit-id #f EDGE-NORMAL))
                             (map (lambda (n-id) (cfg-edge n-id exit-id #f EDGE-RETURN))
                                  exit-candidates))])
          
          ;; Combine everything
          (let ([base-nodes (hash entry-id entry-node
                                 exit-id exit-node
                                 start-node-id start-node)])
            (let ([all-nodes (hash-union base-nodes body-nodes)])
              (cfg entry-id
                   exit-id
                   all-nodes
                   (cons (cfg-edge entry-id start-node-id #f EDGE-NORMAL)
                         (append body-edges exit-edges))
                   (make-hash)))))))))

;; Convert AST to CFG nodes and edges
;; Returns: (values nodes edges start-nodes)
(define (ast->cfg node entry-id exit-id)
  "Convert AST node to CFG representation"
  (match node
    ;; Constant: straight-line code
    [(ast-const loc val)
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id BASIC-NODE (list node) loc))
               '()
               (list id)))]
    
    ;; Variable: straight-line code
    [(ast-var loc name)
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id BASIC-NODE (list node) loc))
               '()
               (list id)))]
    
    ;; If: creates branch
    [(ast-if loc test then else)
     (let* ([branch-id (fresh-node-id!)]
            [join-id (fresh-node-id!)])
       
       ;; Build CFG for test, then, else
       (let-values ([(test-nodes test-edges test-starts) (ast->cfg test entry-id branch-id)]
                     [(then-nodes then-edges then-starts) (ast->cfg then branch-id join-id)]
                     [(else-nodes else-edges else-starts) (ast->cfg else branch-id join-id)])
         
         ;; Create branch and join nodes
         (let ([branch-node (cfg-node branch-id BRANCH-NODE (list node) loc)]
               [join-node (cfg-node join-id JOIN-NODE '() loc)])
           
           ;; Get first nodes from then/else branches
           (let ([then-first (if (null? then-starts) branch-id (car then-starts))]
                 [else-first (if (null? else-starts) branch-id (car else-starts))])
             
             ;; Combine
             (let ([combined-nodes (hash-union (hash-union test-nodes then-nodes) else-nodes)])
               (values (hash-set* (hash-set combined-nodes branch-id branch-node) join-id join-node)
                       (append test-edges
                               then-edges
                               else-edges
                               (list (cfg-edge branch-id then-first test EDGE-TRUE-BRANCH)
                                     (cfg-edge branch-id else-first test EDGE-FALSE-BRANCH)))
                       (list join-id)))))))]
    
    ;; Lambda: treat as atomic for now (could be analyzed separately)
    [(ast-lambda loc params body)
     (let ([id (fresh-node-id!)])
       (values (hash id (cfg-node id BASIC-NODE (list node) loc))
               '()
               (list id)))]
    
    ;; Let: bindings then body
    [(ast-let loc bindings body)
     (let-values ([(binding-nodes binding-edges binding-starts)
                    (for/fold ([all-nodes (hash)]
                               [all-edges '()]
                               [all-starts '()])
                              ([binding (map cdr bindings)])
                        (let-values ([(n e s) (ast->cfg binding entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e)
                                  (append all-starts s))))]
                   [(body-nodes body-edges body-starts)
                     (if (list? body)
                         (if (null? body)
                             (values (hash) '() '())
                             (ast->cfg (ast-app loc (ast-var loc 'begin) body) entry-id exit-id))
                         (ast->cfg body entry-id exit-id))])
       (values (hash-union binding-nodes body-nodes)
               (append binding-edges body-edges)
               (if (null? body-starts) binding-starts body-starts)))]
    
    ;; Letrec: similar to let
    [(ast-letrec loc bindings body)
     (let-values ([(binding-nodes binding-edges binding-starts)
                    (for/fold ([all-nodes (hash)]
                               [all-edges '()]
                               [all-starts '()])
                              ([binding (map cdr bindings)])
                        (let-values ([(n e s) (ast->cfg binding entry-id exit-id)])
                          (values (hash-union all-nodes n)
                                  (append all-edges e)
                                  (append all-starts s))))]
                   [(body-nodes body-edges body-starts)
                     (if (list? body)
                         (if (null? body)
                             (values (hash) '() '())
                             (ast->cfg (ast-app loc (ast-var loc 'begin) body) entry-id exit-id))
                         (ast->cfg body entry-id exit-id))])
       (values (hash-union binding-nodes body-nodes)
               (append binding-edges body-edges)
               (if (null? body-starts) body-starts binding-starts)))]
    
    ;; Define: extract the value and build CFG for it
    [(ast-define loc name value)
     (ast->cfg value entry-id exit-id)]
    
    ;; Application: may create cycle if recursive
    [(ast-app loc func args)
     (let ([id (fresh-node-id!)])
       (let-values ([(op-nodes op-edges op-starts) (ast->cfg func entry-id exit-id)])
         (let-values ([(arg-nodes arg-edges arg-starts)
                        (for/fold ([all-nodes (hash)]
                                   [all-edges '()]
                                   [all-starts '()])
                                  ([arg args])
                          (let-values ([(n e s) (ast->cfg arg entry-id exit-id)])
                            (values (hash-union all-nodes n)
                                    (append all-edges e)
                                    (append all-starts s))))])
           (values (hash-set (hash-union arg-nodes op-nodes) id (cfg-node id BASIC-NODE (list node) loc))
                   (append op-edges arg-edges)
                   (list id)))))]
    
    [else
     (error "Unhandled AST node in CFG builder:" node)]))

;; Helper: get first node ID from a hash of nodes
(define (get-first-node-id nodes)
  "Get first node ID from hash"
  (if (zero? (hash-count nodes))
      #f
      (car (hash-keys nodes))))

;; Helper: get exit nodes (nodes with no outgoing edges)
(define (get-exit-nodes nodes edges)
  "Find nodes with no outgoing edges"
  (let ([has-outgoing (make-hash)])
    (for ([edge edges])
      (hash-set! has-outgoing (cfg-edge-from edge) #t))
    (filter (lambda (node-id) (not (hash-ref has-outgoing node-id #f)))
            (hash-keys nodes))))

;; Helper: union of two hashes
(define (hash-union h1 h2)
  "Union of two hashes"
  (for/fold ([result h1])
            ([(k v) (in-hash h2)])
    (hash-set result k v)))

