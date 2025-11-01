#lang racket/base

(require json
         racket/string)

(provide
 get-mcp-resources
 read-mcp-resource)

;; ============================================================
;; MCP Resources - Data Access
;; ============================================================

;; Resource definitions
(define mcp-resource-definitions
  (list
   (hash 'uri "computational-scheme://bindings"
         'name "Binding Algebra"
         'description "Access to extracted binding algebra data from parsed programs"
         'mimeType "application/json")
   
   (hash 'uri "computational-scheme://topology"
         'name "Scope Topology"
         'description "Access to scope topology data from program analysis"
         'mimeType "application/json")
   
   (hash 'uri "computational-scheme://knowledge-graph"
         'name "Knowledge Graph"
         'description "Access to semantic knowledge graph from NLP processing"
         'mimeType "application/json")))

;; Get list of MCP resources (for protocol)
(define (get-mcp-resources)
  "Return list of resource definitions for MCP protocol"
  mcp-resource-definitions)

;; Read a resource by URI
(define (read-mcp-resource uri)
  "Read resource content by URI - returns hash with 'error or 'text/'mimeType"
  (cond
    [(string-prefix? uri "computational-scheme://bindings")
     (hash 'error "Binding data not yet accessible - requires program context")]
    [(string-prefix? uri "computational-scheme://topology")
     (hash 'error "Topology data not yet accessible - requires program context")]
    [(string-prefix? uri "computational-scheme://knowledge-graph")
     (hash 'error "Knowledge graph not yet accessible - requires query context")]
    [else
     (hash 'error (format "Unknown resource URI: ~a" uri))]))

