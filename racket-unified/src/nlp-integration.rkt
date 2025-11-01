#lang racket/base

(require racket/match
         racket/file
         racket/string
         "nlp/nlp-main.rkt"
         "nlp/layer1-interface.rkt"
         "nlp/layer4-core.rkt"
         "algorithms/unified-pipeline.rkt"
         "algorithms/cfg-builder.rkt"
         "algorithms/cyclomatic.rkt"
         "m-expression.rkt")

(provide
 process-nl-query-to-computation
 execute-nl-query)

;; ============================================================
;; NLP INTEGRATION - Connect NL Queries to Unified Pipeline
;; ============================================================

;; Process NL query and execute computation
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

;; Execute NL query with result display
(define (execute-nl-query nl-text)
  "Execute NL query and display results"
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

