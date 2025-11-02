#lang racket/base

(require racket/match
         racket/set
         "algorithm1.rkt")

(provide
 dependency-graph?
 dependency-graph-edges
 dependency-graph-vertices
 build-dependency-graph
 detect-cycles
 find-recursive-calls
 in-dependency-cycle?
 dependency-edge?
 dependency-edge-from
 dependency-edge-to)

;; ============================================================
;; DEPENDENCY GRAPH CONSTRUCTION AND CYCLE DETECTION
;; ============================================================

;; Dependency edge: from binding A to binding B means A depends on B
(struct dependency-edge (from to edge-type) #:transparent)
;; edge-type: 'call, 'reference, 'recursive-call

;; Dependency graph: collection of edges between bindings
(struct dependency-graph (edges vertices) #:transparent)

;; Build dependency graph from AST
(define (build-dependency-graph ast bindings)
  "Build dependency graph tracking which bindings depend on which others"
  (define edges (mutable-set))
  (define defined-functions (make-hash))  ; Track function definitions
  
  ;; First pass: collect function definitions
  (define (collect-functions expr current-func)
    (match expr
      [(? ast-define? define-expr)
       (let ([name (ast-define-name define-expr)]
             [value (ast-define-value define-expr)])
         (cond
           ;; Function definition: (define (name params...) body...)
           [(list? name)
            (let ([func-name (car name)])
              (hash-set! defined-functions func-name value)
              (collect-functions value func-name))]  ; Set current function context
           ;; Variable definition: (define name value) where value is lambda
           [(ast-lambda? value)
            (hash-set! defined-functions name value)
            (collect-functions value name)]  ; Set current function context
           [else
            (collect-functions value current-func)]))]
      [(? ast-lambda? lambda-expr)
       (for ([body-expr (ast-lambda-body lambda-expr)])
         (collect-functions body-expr current-func))]
      [(? ast-let? let-expr)
       (for ([binding (ast-let-bindings let-expr)])
         (collect-functions (cdr binding) current-func))
       (for ([body-expr (ast-let-body let-expr)])
         (collect-functions body-expr current-func))]
      [(? ast-letrec? letrec-expr)
       (for ([binding (ast-letrec-bindings letrec-expr)])
         (let ([name (car binding)])
           (hash-set! defined-functions name (cdr binding)))
         (collect-functions (cdr binding) current-func))
       (for ([body-expr (ast-letrec-body letrec-expr)])
         (collect-functions body-expr current-func))]
      [(? ast-app? app-expr)
       (let ([func (ast-app-func app-expr)])
         (collect-functions func current-func)
         (for ([arg (ast-app-args app-expr)])
           (collect-functions arg current-func)))]
      [(? ast-if? if-expr)
       (collect-functions (ast-if-test if-expr) current-func)
       (collect-functions (ast-if-then if-expr) current-func)
       (collect-functions (ast-if-else if-expr) current-func)]
      [else '()]))
  
  ;; Second pass: track dependencies
  (define (track-dependencies expr current-func used-vars)
    (match expr
      [(ast-var loc var-name)
       ;; If this variable is a defined function, it's a call
       (cond
         [(hash-has-key? defined-functions var-name)
          ;; Check if it's a recursive call
          (if (and current-func (equal? var-name current-func))
              (set-add! edges (dependency-edge current-func var-name 'recursive-call))
              (set-add! edges (dependency-edge current-func var-name 'call)))
          (set-add! used-vars var-name)]
         [(set-member? bindings var-name)
          ;; Regular variable reference
          (when current-func
            (set-add! edges (dependency-edge current-func var-name 'reference)))
          (set-add! used-vars var-name)]
         [else used-vars])]
      
      [(? ast-define? define-expr)
       (let ([name (ast-define-name define-expr)]
             [value (ast-define-value define-expr)])
         (cond
           ;; Function definition: (define (name params...) body...)
           [(list? name)
            (let ([func-name (car name)])
              (track-dependencies value func-name used-vars))]
           [else
            ;; Variable definition: (define name value)
            (track-dependencies value name used-vars)]))]
      
      [(? ast-lambda? lambda-expr)
       (let ([params (ast-lambda-params lambda-expr)]
             [body (ast-lambda-body lambda-expr)])
         (define body-vars (mutable-set))
         (for ([body-expr body])
           (set! body-vars (track-dependencies body-expr current-func body-vars)))
         ;; Remove parameters from used vars (they're bound here)
         (for ([param params])
           (set-remove! body-vars param))
         body-vars)]
      
      [(? ast-let? let-expr)
       (let ([let-bindings (ast-let-bindings let-expr)]
             [body (ast-let-body let-expr)])
         (define let-bound-names (list->set (map car let-bindings)))
         (define used-vars* used-vars)
         ;; Track dependencies in binding values
         (for ([binding let-bindings])
           (set! used-vars* (track-dependencies (cdr binding) current-func used-vars*)))
         ;; Track dependencies in body
         (for ([body-expr body])
           (set! used-vars* (track-dependencies body-expr current-func used-vars*)))
         ;; Remove let-bound names
         (set-subtract used-vars* let-bound-names))]
      
      [(? ast-letrec? letrec-expr)
       (let ([letrec-bindings (ast-letrec-bindings letrec-expr)]
             [body (ast-letrec-body letrec-expr)])
         (define letrec-bound-names (list->set (map car letrec-bindings)))
         (define used-vars* used-vars)
         ;; Track dependencies in binding values (can reference each other)
         (for ([binding letrec-bindings])
           (let ([name (car binding)])
             (set! used-vars* (track-dependencies (cdr binding) name used-vars*))))
         ;; Track dependencies in body
         (for ([body-expr body])
           (set! used-vars* (track-dependencies body-expr current-func used-vars*)))
         ;; Remove letrec-bound names
         (set-subtract used-vars* letrec-bound-names))]
      
      [(? ast-app? app-expr)
       (let ([func (ast-app-func app-expr)]
             [args (ast-app-args app-expr)])
         (define used-vars* used-vars)
         ;; Track function dependency
         (set! used-vars* (track-dependencies func current-func used-vars*))
         ;; Track argument dependencies
         (for ([arg args])
           (set! used-vars* (track-dependencies arg current-func used-vars*)))
         used-vars*)]
      
      [(? ast-if? if-expr)
       (let ([test (ast-if-test if-expr)]
             [then-branch (ast-if-then if-expr)]
             [else-branch (ast-if-else if-expr)])
         (define used-vars* used-vars)
         (set! used-vars* (track-dependencies test current-func used-vars*))
         (set! used-vars* (track-dependencies then-branch current-func used-vars*))
         (set! used-vars* (track-dependencies else-branch current-func used-vars*))
         used-vars*)]
      
      [else used-vars]))
  
  ;; Run both passes
  (collect-functions ast #f)
  (track-dependencies ast #f (mutable-set))
  
  ;; Extract all vertices from edges
  (define vertices (mutable-set))
  (for ([edge (in-set edges)])
    (set-add! vertices (dependency-edge-from edge))
    (set-add! vertices (dependency-edge-to edge)))
  
  (dependency-graph (set->list edges) vertices))

;; Detect cycles in dependency graph using DFS
(define (detect-cycles graph)
  "Find all cycles in dependency graph"
  (define edges (dependency-graph-edges graph))
  (define vertices (dependency-graph-vertices graph))
  (define cycles (box '()))
  
  ;; Build adjacency list
  (define adj-list (make-hash))
  (for ([v vertices])
    (hash-set! adj-list v (mutable-set)))
  (for ([edge edges])
    (let ([from (dependency-edge-from edge)]
          [to (dependency-edge-to edge)])
      (when (set-member? vertices from)
        (set-add! (hash-ref adj-list from) to))))
  
  ;; DFS to find cycles
  (define (dfs-cycles vertex path visited)
    (cond
      [(member vertex path)
       ;; Found cycle: from first occurrence of vertex to end
       (let ([cycle-start (member vertex path)])
         (when cycle-start
           (set-box! cycles (cons (append cycle-start (list vertex)) (unbox cycles)))))
       (void)]
      [(set-member? visited vertex)
       ;; Already fully explored, no new cycle
       (void)]
      [else
       (set-add! visited vertex)
       (let ([new-path (cons vertex path)])
         (for ([neighbor (hash-ref adj-list vertex '())])
           (when (set-member? vertices neighbor)
             (dfs-cycles neighbor new-path visited)))
         (set-remove! visited vertex))]))
  
  (define visited (mutable-set))
  (for ([vertex vertices])
    (unless (set-member? visited vertex)
      (dfs-cycles vertex '() visited)))
  
  (unbox cycles))

;; Find recursive calls (self-references in function bodies)
(define (find-recursive-calls graph)
  "Find all recursive function calls (self-dependencies)"
  (define recursive-calls '())
  (for ([edge (dependency-graph-edges graph)])
    (when (eq? (dependency-edge-edge-type edge) 'recursive-call)
      (set! recursive-calls (cons edge recursive-calls))))
  recursive-calls)

;; Check if two bindings are in a dependency cycle
(define (in-dependency-cycle? binding1 binding2 graph)
  "Check if two bindings participate in the same dependency cycle"
  (define cycles (detect-cycles graph))
  (for/or ([cycle cycles])
    (and (member binding1 cycle)
         (member binding2 cycle))))

