#lang racket/base

(require racket/match
         racket/file
         racket/string
         "nlp/nlp-main.rkt"
         "nlp/layer1-interface.rkt"
         "nlp/layer4-core.rkt"
         "nlp/llm-bridge.rkt"
         "algorithms/unified-pipeline.rkt"
         "algorithms/cfg-builder.rkt"
         "algorithms/cyclomatic.rkt"
         "algorithms/incidence-structure.rkt"
         "m-expression.rkt")

(provide
 process-nl-query-to-computation
 process-nl-query-enhanced
 execute-nl-query
 execute-nl-query-enhanced)

;; ============================================================
;; NLP INTEGRATION - Connect NL Queries to Unified Pipeline
;; ============================================================

;; Process NL query with optional LLM enhancement
(define (process-nl-query-enhanced nl-text)
  "Process NL query with LLM enhancement: Try LLM first, fallback to rule-based"
  (if (llm-available?)
      (let ([llm-result (llm-classify-intent nl-text)])
        (let ([confidence (hash-ref llm-result 'confidence 0.0)])
          (if (>= confidence 0.7)
              ;; High confidence LLM result - use it
              (let ([operation (hash-ref llm-result 'operation 'unknown)]
                    [program-name (hash-ref llm-result 'program_name #f)]
                    [params (hash-ref llm-result 'parameters (hash))])
                (if (eq? operation 'unknown)
                    ;; LLM couldn't classify - fallback to rule-based
                    (process-nl-query-to-computation nl-text)
                    ;; Use LLM result
                    (let ([args (if program-name (list program-name) '())])
                      (execute-operation operation args))))
              ;; Low confidence - fallback to rule-based
              (process-nl-query-to-computation nl-text))))
      ;; No LLM available - use rule-based
      (process-nl-query-to-computation nl-text)))

;; Execute operation from LLM result
(define (execute-operation op args)
  "Execute operation extracted from LLM classification"
  (case op
    [(computeH1) (handle-compute-h1 args)]
    [(computeVG) (handle-compute-vg args)]
    [(validateHypothesis) (handle-validate-hypothesis args)]
    [(analyzePatterns) (handle-analyze-patterns args)]
    [(compareMetrics) (handle-compare-metrics args)]
    [(exportPolynomial) (handle-export-polynomial args)]
    [(getPatternDimensions) (handle-get-pattern-dimensions args)]
    [else (values #f (format "Unknown operation: ~a" op))]))

;; Process NL query and execute computation (original rule-based)
(define (process-nl-query-to-computation nl-text)
  "Process NL query through full pipeline: NL → Parse → M-expression → Execute"
  ;; Step 1: Parse NL to M-expression
  (define-values (m-expr events kg) (process-nl-query nl-text))
  
  (if (not m-expr)
      (values #f "Parse failed: Invalid query")
      ;; Step 2: Extract operation and arguments from M-expression
      (let ([op (m-expr-op m-expr)]
            [args (m-expr-args m-expr)])
        ;; Step 3: Map M-expression to actual computation
        (case op
          [(computeH1)
           (handle-compute-h1 args)]
          [(computeVG)
           (handle-compute-vg args)]
          [(validateHypothesis)
           (handle-validate-hypothesis args)]
          [(analyzePatterns)
           (handle-analyze-patterns args)]
          [(compareMetrics)
           (handle-compare-metrics args)]
          [(exportPolynomial)
           (handle-export-polynomial args)]
          [(getPatternDimensions)
           (handle-get-pattern-dimensions args)]
          [else
           (values #f (format "Unknown operation: ~a" op))]))))

;; Handle compute H1 operation
(define (handle-compute-h1 args)
  "Handle compute H1 operation from NL query"
  (let* ([program-arg (if (null? args) #f (car args))]
         [source (if program-arg (get-program-source program-arg) "(lambda (x) x)")])
    (if source
        (let ([result (compute-h1-from-source-detailed source)])
          (if (pipeline-result-success result)
              (values result #t)
              (values #f (pipeline-result-error result))))
        (values #f "Could not resolve program source"))))

;; Handle compute V(G) operation
(define (handle-compute-vg args)
  "Handle compute V(G) operation from NL query"
  (let* ([program-arg (if (null? args) #f (car args))]
         [source (if program-arg (get-program-source program-arg) "(lambda (x) x)")])
    (if source
        (with-handlers ([exn? (lambda (e)
                                (values #f (format "V(G) computation failed: ~a" (exn-message e))))])
          (let* ([cfg (build-cfg-from-source source)]
                 [metrics (compute-cyclomatic-complexity cfg)])
            (values metrics #t)))
        (values #f "Could not resolve program source"))))

;; Handle validate hypothesis operation
(define (handle-validate-hypothesis args)
  "Handle validate hypothesis operation H¹ = V(G) - k"
  (let* ([program-arg (if (null? args) #f (car args))]
         [source (if program-arg (get-program-source program-arg) "(lambda (x) x)")]
         [k (if (and (not (null? args)) (> (length args) 1)) (cadr args) 0)])
    (if source
        (with-handlers ([exn? (lambda (e)
                                (values #f (format "Hypothesis validation failed: ~a" (exn-message e))))])
          (let* ([h1-result (compute-h1-from-source-detailed source)]
                 [cfg (build-cfg-from-source source)]
                 [vg-metrics (compute-cyclomatic-complexity cfg)]
                 [h1 (pipeline-result-h1 h1-result)]
                 [vg (complexity-metrics-v-g vg-metrics)]
                 [difference (abs (- h1 (- vg k)))]
                 [matches? (< difference 0.001)])
            (values `((h1 ,h1)
                      (vg ,vg)
                      (k ,k)
                      (difference ,difference)
                      (matches? ,matches?)) #t)))
        (values #f "Could not resolve program source"))))

;; Handle analyze patterns operation
(define (handle-analyze-patterns args)
  "Handle analyze patterns operation"
  (values "Pattern analysis (placeholder)" #t))

;; Handle compare metrics operation
(define (handle-compare-metrics args)
  "Handle compare metrics operation"
  (values "Metrics comparison (placeholder)" #t))

;; Handle export polynomial operation
(define (handle-export-polynomial args)
  "Handle export polynomial representation from NL query
   Args: program identifier or source code"
  (let* ([program-arg (if (null? args) #f (car args))]
         [source (if program-arg (get-program-source program-arg) "(lambda (x) x)")])
    (if source
        (with-handlers ([exn? (lambda (e)
                                (values #f (format "Polynomial export failed: ~a" (exn-message e))))])
          ;; Build incidence structure and export polynomial ring
          (let* ([result (compute-h1-from-source-detailed source)]
                 [incidence-struct (pipeline-result-incidence-structure result)])
            (if (and (pipeline-result-success result) incidence-struct)
                (let ([poly-ring (incidence-structure->polynomial-ring incidence-struct)])
                  (values poly-ring #t))
                (values #f "Could not build incidence structure"))))
        (values #f "Could not resolve program source"))))

;; Handle get pattern dimensions operation
(define (handle-get-pattern-dimensions args)
  "Handle get pattern dimensions from NL query
   Args: program identifier or source code"
  (let* ([program-arg (if (null? args) #f (car args))]
         [source (if program-arg (get-program-source program-arg) "(lambda (x) x)")])
    (if source
        (with-handlers ([exn? (lambda (e)
                                (values #f (format "Pattern dimension analysis failed: ~a" (exn-message e))))])
          (let* ([result (compute-h1-from-source-detailed source)]
                 [incidence-struct (pipeline-result-incidence-structure result)])
            (if (and (pipeline-result-success result) incidence-struct)
                (let ([points (incidence-structure-points incidence-struct)]
                      [points-list (if (hash? points) (hash->list points) points)])
                  ;; Extract dimensional information
                  (let ([dims (for/list ([p-entry points-list])
                                (let ([p (if (pair? p-entry) (cdr p-entry) p-entry)])
                                  (if (incidence-point? p)
                                      `((binding ,(incidence-point-binding-id p))
                                        (dimension ,(incidence-point-dimension p))
                                        (access-count ,(incidence-point-access-count p))
                                        (type ,(incidence-point-type p)))
                                      '())))])
                    (values dims #t)))
                (values #f "Could not build incidence structure"))))
        (values #f "Could not resolve program source"))))

;; Get program source from identifier
(define (get-program-source identifier)
  "Get program source code from identifier"
  (let ([id-str (format "~a" identifier)])
    (cond
      ;; Check if identifier is a file path
      [(file-exists? id-str)
       (file->string id-str)]
      ;; Check test-corpus directory (relative to project root)
      [(file-exists? (format "../../test-corpus/~a" id-str))
       (file->string (format "../../test-corpus/~a" id-str))]
      [(file-exists? (format "../../test-corpus/~a.scm" id-str))
       (file->string (format "../../test-corpus/~a.scm" id-str))]
      ;; Check if identifier is source code itself (starts with paren)
      [(and (> (string-length id-str) 0) (eq? (string-ref id-str 0) #\())
       id-str]
      ;; For demo: return a simple test source if no file found
      [else
       "(lambda (x) x)"])))  ; Default test source

;; Execute NL query with result display (rule-based)
(define (execute-nl-query nl-text)
  "Execute NL query and display results (rule-based parsing)"
  (printf "Processing NL query: ~a\n" nl-text)
  (printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  
  (define-values (result success) (process-nl-query-to-computation nl-text))
  
  (if success
      (begin
        (printf "✓ Query processed successfully\n")
        (cond
          [(pipeline-result? result)
           (printf "  H¹ = ~a\n" (pipeline-result-h1 result))
           (printf "  Bindings: ~a\n" (pipeline-result-num-bindings result))
           (printf "  Simplices: 0:~a, 1:~a, 2:~a\n"
                   (pipeline-result-num-simplices0 result)
                   (pipeline-result-num-simplices1 result)
                   (pipeline-result-num-simplices2 result))]
          [(string? result)
           (printf "  Result: ~a\n" result)]
          [else
           (printf "  Result: ~a\n" result)]))
      (printf "✗ Error: ~a\n" result))
  
  (printf "\n")
  (values result success))

;; Execute NL query with LLM enhancement and display
(define (execute-nl-query-enhanced nl-text)
  "Execute NL query with LLM enhancement and display results"
  (printf "Processing NL query (LLM-enhanced): ~a\n" nl-text)
  (printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  
  (if (llm-available?)
      (printf "  Using LLM enhancement\n")
      (printf "  LLM not available, using rule-based parsing\n"))
  
  (define-values (result success) (process-nl-query-enhanced nl-text))
  
  (if success
      (begin
        (printf "✓ Query processed successfully\n")
        ;; Try to generate LLM explanation if available
        (if (and (llm-available?) (pipeline-result? result))
            (let ([explanation (llm-generate-response "computeH1" result)])
              (if explanation
                  (printf "  Explanation: ~a\n" explanation))))
        (cond
          [(pipeline-result? result)
           (printf "  H¹ = ~a\n" (pipeline-result-h1 result))
           (printf "  Bindings: ~a\n" (pipeline-result-num-bindings result))
           (printf "  Simplices: 0:~a, 1:~a, 2:~a\n"
                   (pipeline-result-num-simplices0 result)
                   (pipeline-result-num-simplices1 result)
                   (pipeline-result-num-simplices2 result))]
          [(string? result)
           (printf "  Result: ~a\n" result)]
          [else
           (printf "  Result: ~a\n" result)]))
      (printf "✗ Error: ~a\n" result))
  
  (printf "\n")
  (values result success))

