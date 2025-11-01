#lang racket/base

(require json
         racket/port)

(provide
 mcp-create-response
 mcp-create-error-response
 mcp-parse-request
 mcp-validate-request
 get-request-id
 get-request-method
 get-request-params
 PARSE_ERROR
 INVALID_REQUEST
 METHOD_NOT_FOUND
 INVALID_PARAMS
 INTERNAL_ERROR)

;; ============================================================
;; JSON-RPC 2.0 Protocol Handler
;; ============================================================

;; JSON-RPC 2.0 Response
(define (mcp-create-response id result)
  "Create a JSON-RPC 2.0 success response"
  (hash 'jsonrpc "2.0"
        'id id
        'result result))

;; JSON-RPC 2.0 Error Response
(define (mcp-create-error-response id code message [data #f])
  "Create a JSON-RPC 2.0 error response"
  (hash 'jsonrpc "2.0"
        'id id
        'error (hash 'code code
                     'message message
                     'data (if data data (hash)))))

;; Standard JSON-RPC 2.0 error codes
(define PARSE_ERROR -32700)
(define INVALID_REQUEST -32600)
(define METHOD_NOT_FOUND -32601)
(define INVALID_PARAMS -32602)
(define INTERNAL_ERROR -32603)

;; Parse JSON-RPC request from string
(define (mcp-parse-request json-string)
  "Parse JSON string into Racket hash, returns #f on error"
  (with-handlers ([exn? (lambda (e) #f)])
    (string->jsexpr json-string)))

;; Validate JSON-RPC request structure
(define (mcp-validate-request request)
  "Validate that request has required JSON-RPC 2.0 fields"
  (and (hash? request)
       (hash-has-key? request 'jsonrpc)
       (equal? (hash-ref request 'jsonrpc) "2.0")
       (hash-has-key? request 'method)
       (hash-has-key? request 'id)))

;; Helper to extract request components
(define (get-request-id request)
  "Extract ID from request"
  (hash-ref request 'id #f))

(define (get-request-method request)
  "Extract method from request"
  (hash-ref request 'method #f))

(define (get-request-params request)
  "Extract params from request (defaults to empty hash)"
  (hash-ref request 'params (hash)))

