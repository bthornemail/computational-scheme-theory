#lang racket/base

(require json
         racket/match
         racket/string
         "../../racket-unified/src/algorithms/unified-pipeline.rkt"
         "../../racket-unified/src/bridge/racket-bridge.rkt"
         "../../racket-unified/src/m-expression.rkt"
         "../../racket-unified/src/nlp/layer1-interface.rkt")

(provide
 get-mcp-tools
 call-mcp-tool)

;; ============================================================
;; MCP Tools - Wrapping racket-unified API
;; ============================================================

;; Tool: compute_h1
(define (tool-compute-h1 params)
  "Compute H¹ cohomology from Scheme source code"
  (with-handlers ([exn? (lambda (e)
                         (hash 'success #f
                               'error (format "Error computing H¹: ~a" (exn-message e))))])
    (let* ([source (hash-ref params 'source_code "")]
           [result (compute-h1-from-source-detailed source)])
      (if (pipeline-result-success result)
          (hash 'success #t
                'h1 (pipeline-result-h1 result)
                'bindings (pipeline-result-num-bindings result)
                'simplices_0 (pipeline-result-num-simplices0 result)
                'simplices_1 (pipeline-result-num-simplices1 result)
                'simplices_2 (pipeline-result-num-simplices2 result))
          (hash 'success #f
                'error (or (pipeline-result-error result) "Unknown error"))))))

;; Tool: compute_vg (optional, requires service)
(define (tool-compute-vg params)
  "Compute V(G) cyclomatic complexity (requires Racket service)"
  (if (racket-service-available?)
      (let-values ([(vg error) (call-racket-vg (hash-ref params 'source_code ""))])
        (if error
            (hash 'success #f 'error error)
            (hash 'success #t 'v_g vg)))
      (hash 'success #f 'error "Racket V(G) service not available")))

;; Tool: validate_hypothesis
(define (tool-validate-hypothesis params)
  "Validate hypothesis H¹ = V(G) - k"
  (let* ([h1 (hash-ref params 'h1 #f)]
         [vg (hash-ref params 'v_g #f)]
         [k (hash-ref params 'k 0)]
         [tolerance (hash-ref params 'tolerance 0)])
    (if (and h1 vg)
        (let-values ([(valid? diff msg) (validate-hypothesis h1 vg k tolerance)])
          (hash 'valid valid?
                'difference diff
                'message msg))
        (hash 'success #f 'error "Missing h1 or v_g parameter"))))

;; Tool: process_natural_language
(define (tool-process-natural-language params)
  "Convert natural language query to M-expression"
  (let ([query (hash-ref params 'query "")])
    (with-handlers ([exn? (lambda (e)
                          (hash 'success #f
                                'error (exn-message e)))])
      (let ([m-expr (nl-to-m-expression query)])
        (hash 'success #t
              'm_expression (hash 'op (if (m-expr? m-expr) (m-expr-op m-expr) #f)
                                  'args (if (m-expr? m-expr) 
                                           (m-expr-args m-expr) 
                                           '()))
              'raw_query query)))))

;; MCP Tool Definitions with JSON Schemas
(define mcp-tool-definitions
  (list
   (hash 'name "compute_h1"
         'description "Compute H¹ cohomology from Scheme source code using the unified pipeline"
         'inputSchema (hash
                      'type "object"
                      'properties (hash
                                   'source_code (hash
                                                'type "string"
                                                'description "Scheme source code to analyze"))
                      'required (list "source_code"))
         'handler tool-compute-h1)
   
   (hash 'name "compute_vg"
         'description "Compute V(G) cyclomatic complexity from Scheme source (requires Racket service)"
         'inputSchema (hash
                      'type "object"
                      'properties (hash
                                   'source_code (hash
                                                'type "string"
                                                'description "Scheme source code to analyze"))
                      'required (list "source_code"))
         'handler tool-compute-vg)
   
   (hash 'name "validate_hypothesis"
         'description "Validate the hypothesis H¹ = V(G) - k"
         'inputSchema (hash
                      'type "object"
                      'properties (hash
                                   'h1 (hash 'type "number" 'description "H¹ cohomology value")
                                   'v_g (hash 'type "number" 'description "V(G) cyclomatic complexity")
                                   'k (hash 'type "number" 'description "Normalization constant (default: 0)")
                                   'tolerance (hash 'type "number" 'description "Allowed tolerance (default: 0)"))
                      'required (list "h1" "v_g"))
         'handler tool-validate-hypothesis)
   
   (hash 'name "process_natural_language"
         'description "Convert natural language query to M-expression using SGP-ASLN"
         'inputSchema (hash
                      'type "object"
                      'properties (hash
                                   'query (hash
                                          'type "string"
                                          'description "Natural language query (e.g., 'compute H1 for program test')"))
                      'required (list "query"))
         'handler tool-process-natural-language)))

;; Get list of MCP tools (without handlers, for protocol)
(define (get-mcp-tools)
  "Return list of tool definitions for MCP protocol"
  (map (lambda (tool)
         (hash 'name (hash-ref tool 'name)
               'description (hash-ref tool 'description)
               'inputSchema (hash-ref tool 'inputSchema)))
       mcp-tool-definitions))

;; Call a specific tool by name
(define (call-mcp-tool tool-name params)
  "Call a tool by name with given parameters"
  (with-handlers ([exn? (lambda (e)
                         (hash 'success #f 
                               'error (format "Error calling tool '~a': ~a" tool-name (exn-message e))))])
    (let ([tool (findf (lambda (t) (equal? (hash-ref t 'name) tool-name)) mcp-tool-definitions)])
      (if tool
          (let ([handler (hash-ref tool 'handler)])
            (handler params))
          (hash 'success #f 'error (format "Tool '~a' not found" tool-name))))))

