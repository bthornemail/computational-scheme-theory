#lang racket/base

(require racket/match
         racket/set
         "algorithm1.rkt"
         "algorithm2.rkt"
         "algorithm3.rkt"
         "algorithm4.rkt"
         racket/struct)

;; ============================================================
;; UNIFIED PIPELINE: Source → H¹
;; ============================================================

;; Result structure (defined before provide)
(struct pipeline-result (h1-value
                         beta0
                         beta1
                         num-bindings
                         num-simplices0
                         num-simplices1
                         num-simplices2
                         success
                         error-message) #:transparent)

;; Convenience aliases (defined before provide)
(define pipeline-result-h1 pipeline-result-h1-value)
(define pipeline-result-error pipeline-result-error-message)

(provide
 compute-h1-from-source
 compute-h1-from-source-detailed
 pipeline-result?
 pipeline-result-h1-value
 pipeline-result-h1
 pipeline-result-beta0
 pipeline-result-beta1
 pipeline-result-num-bindings
 pipeline-result-num-simplices0
 pipeline-result-num-simplices1
 pipeline-result-num-simplices2
 pipeline-result-success
 pipeline-result-error-message
 pipeline-result-error)

;; R_Scheme rig structure (binding algebra)
(struct r-scheme (bindings) #:transparent)

;; Build R_Scheme from bindings set
(define (build-r-scheme bindings)
  "Build R_Scheme rig from bindings set"
  (r-scheme bindings))

;; Complete pipeline from Scheme source to H¹
(define (compute-h1-from-source source)
  "Complete pipeline: Source → Parse → Bindings → Topology → Complex → H¹"
  (with-handlers ([exn? (lambda (e) (values #f (exn-message e)))])
    (cond
      [(string? source)
       ;; Parse as Scheme code string
       (let* ([sexpr (read (open-input-string source))]
              [ast (sexpr->ast sexpr)]
              [alpha-ast (alpha-convert ast)]
              [bindings (extract-bindings alpha-ast)])
         (compute-h1-from-ast alpha-ast bindings))]
      [(list? source)
       ;; Already parsed S-expression
       (let* ([ast (sexpr->ast source)]
              [alpha-ast (alpha-convert ast)]
              [bindings (extract-bindings alpha-ast)])
         (compute-h1-from-ast alpha-ast bindings))]
      [else
       (error "Invalid source format" source)])))

;; Compute H¹ from AST and bindings
(define (compute-h1-from-ast ast bindings)
  "Compute H¹ from parsed AST"
  (let* ([r-scheme-rig (build-r-scheme bindings)]
         ;; Step 2: Enhanced scope analysis (returns two values)
         [enhanced-scope-map-and-tree (call-with-values
                                       (lambda () (analyze-scopes-enhanced ast))
                                       list)]
         [enhanced-scope-map (car enhanced-scope-map-and-tree)]
         [scope-tree (cadr enhanced-scope-map-and-tree)]
         ;; Step 3: Build topology
         [topology (compute-topology-enhanced r-scheme-rig enhanced-scope-map scope-tree)]
         ;; Step 4: Build Čech complex
         [complex (build-cech-complex topology)]
         ;; Step 5: Compute H¹
         [h1 (compute-h1 complex)])
    (values h1 #f)))

;; Enhanced topology computation
(define (compute-topology-enhanced rig enhanced-scope-map scope-tree)
  "Build topology from R_Scheme and enhanced scope map"
  (hash 'open-sets (hash-map enhanced-scope-map
                              (lambda (k v)
                                (if (enhanced-visibility-region? v)
                                    (cons k (enhanced-visibility-region-base-region v))
                                    (cons k v))))
        'binding-count (set-count (r-scheme-bindings rig))
        'scope-tree scope-tree
        'enhanced-regions enhanced-scope-map))

;; Build Čech complex from topology
(define (build-cech-complex topology)
  "Build Čech complex from topology"
  (let ([open-cover (build-open-cover topology)])
    (compute-nerve open-cover)))

;; Detailed pipeline result
(define (compute-h1-from-source-detailed source)
  "Complete pipeline with detailed statistics"
  (let ([result (with-handlers ([exn? (lambda (e)
                                         (pipeline-result 0 0 0 0 0 0 0 #f (exn-message e)))])
                  (let-values ([(h1 error-msg) (compute-h1-from-source source)])
                    (if error-msg
                        (pipeline-result 0 0 0 0 0 0 0 #f error-msg)
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
                               [complex (build-cech-complex topology)])
                          (pipeline-result h1
                                           1  ; beta0 (simplified)
                                           h1 ; beta1 = h1
                                           (set-count bindings)
                                           (set-count (simplicial-complex-simplices0 complex))
                                           (set-count (simplicial-complex-simplices1 complex))
                                           (set-count (simplicial-complex-simplices2 complex))
                                           #t
                                           #f)))))])
    result))
