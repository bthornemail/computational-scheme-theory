#lang racket/base

(require racket/match
         racket/set
         racket/format
         racket/list
         racket/struct
         "algorithm1.rkt"
         "algorithm2.rkt"
         "algorithm3.rkt"
         "algorithm4.rkt"
         "dependency-graph.rkt"
         "incidence-structure.rkt"
         "prolog-export.rkt"
         "datalog-export.rkt"
         "combinator-detector.rkt"
         "../m-expression.rkt"
         "m-expr-to-logic.rkt")

;; ============================================================
;; UNIFIED PIPELINE: Source → H¹
;; ============================================================

;; Result structure (defined before provide)
(struct pipeline-result (h1-value
                         h2-value
                         h3-value
                         h4-value
                         beta0
                         beta1
                         num-bindings
                         num-simplices0
                         num-simplices1
                         num-simplices2
                         success
                         error-message) #:transparent)

;; Debug result structure with intermediate data
(struct debug-result (h1-value
                     beta0
                     beta1
                     num-bindings
                     num-simplices0
                     num-simplices1
                     num-simplices2
                     scope-tree-json
                     visibility-regions
                     cech-complex-structure
                     overlap-detection-log
                     success
                     error-message) #:transparent)

;; Convenience aliases (defined before provide)
(define pipeline-result-h1 pipeline-result-h1-value)
(define pipeline-result-error pipeline-result-error-message)

(provide
 compute-h1-from-source
 compute-h1-from-source-detailed
 compute-h1-with-debug
 pipeline-result?
 pipeline-result-h1-value
 pipeline-result-h1
 pipeline-result-h2-value
 pipeline-result-h3-value
 pipeline-result-h4-value
 pipeline-result-beta0
 pipeline-result-beta1
 pipeline-result-num-bindings
 pipeline-result-num-simplices0
 pipeline-result-num-simplices1
 pipeline-result-num-simplices2
 pipeline-result-success
 pipeline-result-error-message
 pipeline-result-error
 debug-result?
 debug-result-h1-value
 debug-result-scope-tree-json
 debug-result-visibility-regions
 debug-result-cech-complex-structure
 debug-result-overlap-detection-log)

;; R_Scheme rig structure (binding algebra)
(struct r-scheme (bindings) #:transparent)

;; Build R_Scheme from bindings set
(define (build-r-scheme bindings)
  "Build R_Scheme rig from bindings set"
  (r-scheme bindings))

;; Complete pipeline from Scheme source to H¹-H⁴
(define (compute-h1-from-source source)
  "Complete pipeline: Source → Parse → Bindings → Topology → Complex → H¹-H⁴
   Returns: (values h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct error)"
  (with-handlers ([exn? (lambda (e) (values #f #f #f #f #f #f #f (exn-message e)))])
    (cond
      [(string? source)
       ;; Parse as Scheme code string
       (let* ([sexpr (read (open-input-string source))]
              [ast (sexpr->ast sexpr)]
              ;; Build dependency graph BEFORE alpha-conversion (to preserve function names)
              [dep-graph (build-dependency-graph ast (extract-bindings ast))]
              [alpha-ast (alpha-convert ast)]
              [bindings (extract-bindings alpha-ast)])
         (let-values ([(h1-cech h1-incidence h2-incidence h3-incidence h4-incidence returned-dep-graph incidence-struct)
                       (compute-h1-from-ast-with-dep-graph alpha-ast bindings dep-graph)])
           (values h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct #f)))]
      [(list? source)
       ;; Already parsed S-expression
       (let* ([ast (sexpr->ast source)]
              [alpha-ast (alpha-convert ast)]
              [bindings (extract-bindings alpha-ast)]
              [dep-graph (build-dependency-graph ast (extract-bindings ast))])
         (let-values ([(h1-cech h1-incidence h2-incidence h3-incidence h4-incidence returned-dep-graph incidence-struct)
                       (compute-h1-from-ast-with-dep-graph alpha-ast bindings dep-graph)])
           (values h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct #f)))]
      [else
       (error "Invalid source format" source)])))

;; Compute H¹ from AST and bindings (with enhanced cycle detection and incidence structure)
;; This version accepts a pre-built dependency graph (from original AST before alpha-conversion)
(define (compute-h1-from-ast-with-dep-graph ast bindings dep-graph)
  "Compute H¹ from parsed AST with pre-built dependency graph and incidence structure"
  (let* ([r-scheme-rig (build-r-scheme bindings)]
         ;; Step 1: Use provided dependency graph (built from original AST before alpha-conversion)
         ;; Step 2: Enhanced scope analysis (returns two values)
         [enhanced-scope-map-and-tree (call-with-values
                                       (lambda () (analyze-scopes-enhanced ast))
                                       list)]
         [enhanced-scope-map (car enhanced-scope-map-and-tree)]
         [scope-tree (cadr enhanced-scope-map-and-tree)]
         ;; Step 3: Build topology
         [topology (compute-topology-enhanced r-scheme-rig enhanced-scope-map scope-tree)]
         ;; Step 4: Build Čech complex (with dependency graph for cycle detection)
         [complex (build-cech-complex topology #:dependency-graph dep-graph)]
         ;; Step 5: Compute H¹ via Čech complex
         [h1-cech (compute-h1 complex)]
         ;; Step 6: Detect combinators
         [combinators (detect-all-combinators ast)]
         ;; Step 7: Build incidence structure (with dependency graph and combinators)
         [incidence-struct (build-incidence-structure ast bindings enhanced-scope-map scope-tree
                                                       #:dependency-graph dep-graph
                                                       #:combinators combinators)]
         ;; Step 8: Compute H¹ through H⁴ via incidence structure
         [h1-incidence (compute-h1-incidence incidence-struct)]
         [h2-incidence (compute-h2-incidence incidence-struct)]
         [h3-incidence (compute-h3-incidence incidence-struct)]
         [h4-incidence (compute-h4-incidence incidence-struct)]
         ;; Step 9: Prolog/Datalog analysis for combinators (optional)
         [m-expr-ast (ast-to-m-expr ast)]
         [prolog-facts (ast-to-prolog-facts ast bindings enhanced-scope-map)]
         [datalog-facts (ast-to-datalog-facts ast bindings enhanced-scope-map incidence-struct)])
    (values h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct)))

;; Enhanced topology computation
(define (compute-topology-enhanced rig enhanced-scope-map scope-tree)
  "Build topology from R_Scheme and enhanced scope map"
  ;; Convert list to hash if needed
  (define scope-map-hash
    (if (hash? enhanced-scope-map)
        enhanced-scope-map
        (make-hash (if (list? enhanced-scope-map) enhanced-scope-map '()))))
  
  ;; Build open-sets hash with visibility regions
  (define open-sets (make-hash))
  (for ([(k v) (in-hash scope-map-hash)])
    (hash-set! open-sets k
               (if (enhanced-visibility-region? v)
                   (enhanced-visibility-region-base-region v)
                   v)))
  
  (hash 'open-sets open-sets
        'binding-count (set-count (r-scheme-bindings rig))
        'scope-tree scope-tree
        'enhanced-regions scope-map-hash))

;; Build Čech complex from topology (now uses dependency graph)
(define (build-cech-complex topology #:dependency-graph [dep-graph #f])
  "Build Čech complex from topology, optionally using dependency graph"
  (let ([open-cover (build-open-cover topology)])
    (if (hash? open-cover)
        (compute-nerve open-cover #:dependency-graph dep-graph)
        (error "build-open-cover did not return a hash"))))

;; Detailed pipeline result
(define (compute-h1-from-source-detailed source)
  "Complete pipeline with detailed statistics including β₁ cross-validation"
  (let ([result (with-handlers ([exn? (lambda (e)
                                         (pipeline-result 0 0 0 0 0 0 0 0 0 0 #f (exn-message e)))])
                  (let-values ([(h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct error-msg) 
                                (compute-h1-from-source source)])
                    (if error-msg
                        (pipeline-result 0 0 0 0 0 0 0 0 0 0 #f error-msg)
                        (let* ([sexpr (if (string? source)
                                          (read (open-input-string source))
                                          source)]
                               [ast (sexpr->ast sexpr)]
                               [alpha-ast (alpha-convert ast)]
                               [bindings (extract-bindings alpha-ast)]
                               [r-scheme-rig (build-r-scheme bindings)]
                               [enhanced-scope-map-and-tree (call-with-values
                                                              (lambda () (analyze-scopes-enhanced alpha-ast))
                                                              list)]
                               [enhanced-scope-map (car enhanced-scope-map-and-tree)]
                               [scope-tree (cadr enhanced-scope-map-and-tree)]
                               [topology (compute-topology-enhanced r-scheme-rig enhanced-scope-map scope-tree)]
                               [complex (build-cech-complex topology #:dependency-graph dep-graph)]
                               ;; Cross-validation: compute β₁ independently from graph structure
                               [beta0-graph (compute-beta0-connected-components complex)]
                               [beta1-graph (compute-beta1-from-graph complex)])
                          ;; Use H¹ from Čech complex for compatibility (or could use incidence)
                          (pipeline-result (or h1-incidence 0)  ; Use incidence H¹
                                           (or h2-incidence 0)  ; H² from incidence structure
                                           (or h3-incidence 0)  ; H³ from incidence structure
                                           (or h4-incidence 0)  ; H⁴ from incidence structure
                                           beta0-graph  ; beta0 from graph structure
                                           beta1-graph  ; beta1 from graph structure (for cross-validation)
                                           (set-count bindings)
                                           (set-count (simplicial-complex-simplices0 complex))
                                           (set-count (simplicial-complex-simplices1 complex))
                                           (set-count (simplicial-complex-simplices2 complex))
                                           #t
                                           #f)))))])
    result))

;; Serialize scope tree to JSON-like structure
(define (serialize-scope-tree scope-tree)
  "Convert scope tree to JSON-serializable format"
  (define nodes-hash (scope-tree-nodes scope-tree))
  (define result (make-hash))
  (hash-set! result 'root (symbol->string (scope-tree-root scope-tree)))
  (hash-set! result 'nodes (make-hash))
  (for ([(id node) (in-hash nodes-hash)])
    (hash-set! (hash-ref result 'nodes) (symbol->string id)
               (hash 'id (symbol->string id)
                     'parent (symbol->string (scope-node-parent node))
                     'children (map symbol->string (scope-node-children node))
                     'bindings (set->list (scope-node-bindings node))
                     'depth (scope-node-depth node))))
  result)

;; Serialize visibility regions
(define (serialize-visibility-regions enhanced-scope-map)
  "Convert visibility regions to JSON-serializable format"
  (define result (make-hash))
  (for ([(binding-id region) (in-hash enhanced-scope-map)])
    (hash-set! result (symbol->string binding-id)
               (cond
                 [(enhanced-visibility-region? region)
                  (hash 'scope-ids (map symbol->string (set->list (enhanced-visibility-region-scope-ids region)))
                        'usage-pattern (symbol->string (enhanced-visibility-region-usage-pattern region))
                        'base-scopes (if (visibility-region? (enhanced-visibility-region-base-region region))
                                        (map symbol->string (set->list (visibility-region-scopes (enhanced-visibility-region-base-region region))))
                                        '())
                        'base-bindings (if (visibility-region? (enhanced-visibility-region-base-region region))
                                          (map symbol->string (set->list (visibility-region-bindings (enhanced-visibility-region-base-region region))))
                                          '()))]
                 [else
                  (hash 'type 'basic)])))
  result)

;; Serialize Čech complex structure
(define (serialize-cech-complex complex)
  "Convert Čech complex to JSON-serializable format"
  (hash 'simplices0 (map (lambda (s) (set->list (simplex-vertices s))) (set->list (simplicial-complex-simplices0 complex)))
        'simplices1 (map (lambda (s) (set->list (simplex-vertices s))) (set->list (simplicial-complex-simplices1 complex)))
        'simplices2 (map (lambda (s) (set->list (simplex-vertices s))) (set->list (simplicial-complex-simplices2 complex)))
        'count0 (set-count (simplicial-complex-simplices0 complex))
        'count1 (set-count (simplicial-complex-simplices1 complex))
        'count2 (set-count (simplicial-complex-simplices2 complex))))

;; Log overlap detection results
(define (log-overlap-detection open-cover)
  "Log overlap detection results for all pairs and triples"
  (define cover-list (hash->list open-cover))
  (define n (length cover-list))
  (define indices (range n))
  
  ;; Log pairs
  (define pair-log
    (for*/list ([i indices]
                [j indices]
                #:when (< i j))
      (let ([region1 (cdr (list-ref cover-list i))]
            [region2 (cdr (list-ref cover-list j))])
        (hash 'pair (list i j)
              'overlaps (have-overlap region1 region2)
              'type 'pair))))
  
  ;; Log triples
  (define triple-log
    (for*/list ([i indices]
                [j indices]
                [k indices]
                #:when (and (< i j) (< j k)))
      (let ([region1 (cdr (list-ref cover-list i))]
            [region2 (cdr (list-ref cover-list j))]
            [region3 (cdr (list-ref cover-list k))])
        (define pair12 (have-overlap region1 region2))
        (define pair23 (have-overlap region2 region3))
        (define pair13 (have-overlap region1 region3))
        (hash 'triple (list i j k)
              'pair12 pair12
              'pair23 pair23
              'pair13 pair13
              'all-pairs-overlap (and pair12 pair23 pair13)
              'type 'triple))))
  
  (append pair-log triple-log))

;; Compute H¹ with comprehensive debug output
(define (compute-h1-with-debug source)
  "Complete pipeline with comprehensive debug output for diagnostic analysis"
  (with-handlers ([exn? (lambda (e)
                          (debug-result 0 0 0 0 0 0 0 #f #f #f #f #f (exn-message e)))])
    (let* ([sexpr (if (string? source)
                      (read (open-input-string source))
                      source)]
           [ast (sexpr->ast sexpr)]
           [alpha-ast (alpha-convert ast)]
           [bindings (extract-bindings alpha-ast)]
           [dep-graph (build-dependency-graph alpha-ast bindings)]
           [r-scheme-rig (build-r-scheme bindings)]
           [enhanced-scope-map-and-tree (call-with-values
                                         (lambda () (analyze-scopes-enhanced alpha-ast))
                                         list)]
           [enhanced-scope-map (car enhanced-scope-map-and-tree)]
           [scope-tree (cadr enhanced-scope-map-and-tree)]
           [topology (compute-topology-enhanced r-scheme-rig enhanced-scope-map scope-tree)]
           [open-cover (build-open-cover topology)]
           [overlap-log (log-overlap-detection open-cover)]
           [complex (build-cech-complex topology #:dependency-graph dep-graph)]
           [h1 (compute-h1 complex)]
           [beta0-graph (compute-beta0-connected-components complex)]
           [beta1-graph (compute-beta1-from-graph complex)])
      (debug-result h1
                    beta0-graph
                    beta1-graph
                    (set-count bindings)
                    (set-count (simplicial-complex-simplices0 complex))
                    (set-count (simplicial-complex-simplices1 complex))
                    (set-count (simplicial-complex-simplices2 complex))
                    (serialize-scope-tree scope-tree)
                    (serialize-visibility-regions enhanced-scope-map)
                    (serialize-cech-complex complex)
                    overlap-log
                    #t
                    #f))))
