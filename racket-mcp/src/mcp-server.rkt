#lang racket/base

(require json
         racket/port
         racket/string
         "json-rpc.rkt"
         "mcp-tools.rkt"
         "mcp-resources.rkt")

(provide
 start-mcp-server)

;; ============================================================
;; MCP Server - Main Implementation
;; ============================================================

;; MCP Protocol Version
(define MCP_PROTOCOL_VERSION "2024-11-05")

;; Initialize handler
(define (handle-initialize params)
  "Handle MCP initialize request"
  (hash 'protocolVersion MCP_PROTOCOL_VERSION
        'capabilities (hash
                      'tools (hash)
                      'resources (hash))
        'serverInfo (hash
                    'name "computational-scheme-theory"
                    'version "0.1.0")))

;; Tools/list handler
(define (handle-tools-list params)
  "Handle tools/list request"
  (hash 'tools (get-mcp-tools)))

;; Tools/call handler
(define (handle-tools-call params)
  "Handle tools/call request"
  (let* ([name (hash-ref params 'name #f)]
         [arguments (hash-ref params 'arguments (hash))])
    (if name
        (call-mcp-tool name arguments)
        (hash 'success #f 'error "Missing tool name"))))

;; Resources/list handler
(define (handle-resources-list params)
  "Handle resources/list request"
  (hash 'resources (get-mcp-resources)))

;; Resources/read handler
(define (handle-resources-read params)
  "Handle resources/read request"
  (let ([uri (hash-ref params 'uri #f)])
    (if uri
        (read-mcp-resource uri)
        (hash 'success #f 'error "Missing resource URI"))))

;; Method router
(define (route-mcp-method method params)
  "Route MCP method to appropriate handler"
  (case method
    [("initialize") (handle-initialize params)]
    [("tools/list") (handle-tools-list params)]
    [("tools/call") (handle-tools-call params)]
    [("resources/list") (handle-resources-list params)]
    [("resources/read") (handle-resources-read params)]
    [else #f]))

;; Process a single JSON-RPC request
(define (process-request request-json)
  "Process a JSON-RPC request and return response"
  (cond
    ;; Validate request structure
    [(not (mcp-validate-request request-json))
     (mcp-create-error-response
      (get-request-id request-json)
      INVALID_REQUEST
      "Invalid JSON-RPC request")]
    
    ;; Route to method handler
    [else
     (let* ([method (get-request-method request-json)]
            [params (get-request-params request-json)]
            [id (get-request-id request-json)]
            [result (route-mcp-method method params)])
       (if result
           (mcp-create-response id result)
           (mcp-create-error-response
            id
            METHOD_NOT_FOUND
            (format "Method '~a' not found" method))))]))

;; Main server loop (stdio mode)
(define (start-mcp-server)
  "Start MCP server on stdio for AI assistant integration"
  ;; Log to stderr (so stdout is only for JSON-RPC)
  (fprintf (current-error-port) "Computational Scheme Theory MCP Server starting...\n")
  (flush-output (current-error-port))
  
  ;; Main request loop
  (let loop ([line-count 0])
    (let ([line (read-line)])
      (cond
        ;; EOF - exit gracefully
        [(eof-object? line)
         (fprintf (current-error-port) "MCP Server shutting down.\n")
         (flush-output (current-error-port))]
        
        ;; Empty line - skip
        [(string=? (string-trim line) "")
         (loop line-count)]
        
        ;; Process request
        [else
         (let* ([request-json (mcp-parse-request line)])
           (if request-json
               (let ([response (process-request request-json)])
                 ;; Write response to stdout (JSON-RPC protocol)
                 (displayln (jsexpr->string response))
                 (flush-output))
               (begin
                 ;; Parse error
                 (let ([response (mcp-create-error-response
                                  #f
                                  PARSE_ERROR
                                  "Parse error")])
                   (displayln (jsexpr->string response))
                   (flush-output))))
           (loop (+ line-count 1)))]))))

;; Main entry point
(module+ main
  (start-mcp-server))

