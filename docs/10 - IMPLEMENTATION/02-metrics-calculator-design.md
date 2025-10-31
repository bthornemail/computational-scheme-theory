# Traditional Metrics Calculator Design

**Status:** Planning Phase
**Language:** Scheme/Racket
**Purpose:** Compute cyclomatic complexity V(G) from control flow graphs for comparison with H¹

---

## 1. Technology Stack Selection

### Core Platform

**Racket** with R5RS compatibility mode
- Built-in R5RS support via `#lang r5rs` or `plt-r5rs`
- Rich metaprogramming capabilities
- Excellent AST manipulation tools
- Active community and documentation

### Key Libraries

1. **syntax/parse** - Racket's powerful syntax pattern matching
   - Process syntax objects structurally
   - Define syntax classes for common patterns
   - Error reporting with source locations

2. **graph** - Racket's graph library (if available, or build custom)
   - Graph data structures
   - DFS/BFS traversal
   - Connected components

3. **rackunit** - Unit testing framework
   - Test fixture support
   - Property-based testing via QuickCheck port

### Build and Integration

- **Racket Package Manager**: `raco pkg install`
- **gRPC**: Use `grpc` Racket package or shell out to Python bridge
- **Proto files**: Share `.proto` definitions with Haskell core

---

## 2. Module Structure

```
computational-scheme-theory/
├── racket-metrics/
│   ├── info.rkt                          -- Package metadata
│   ├── main.rkt                          -- Entry point
│   ├── r5rs-parser.rkt                   -- Parse R5RS to AST
│   ├── ast-types.rkt                     -- AST data structures
│   ├── cfg-builder.rkt                   -- Build control flow graph
│   ├── cfg-types.rkt                     -- CFG data structures
│   ├── cyclomatic.rkt                    -- V(G) calculator
│   ├── graph-utils.rkt                   -- Graph algorithms
│   ├── metrics-api.rkt                   -- gRPC service interface
│   ├── pretty-print.rkt                  -- CFG visualization
│   ├── test/
│   │   ├── parser-test.rkt
│   │   ├── cfg-builder-test.rkt
│   │   ├── cyclomatic-test.rkt
│   │   └── integration-test.rkt
│   ├── examples/
│   │   ├── simple-if.scm                 -- V(G) = 2
│   │   ├── factorial.scm                 -- V(G) = 2
│   │   ├── nested-loops.scm              -- V(G) = 4
│   │   └── call-cc.scm                   -- V(G) = ?
│   └── proto/
│       └── metrics_service.proto         -- gRPC interface
```

---

## 3. Core Data Structures

### 3.1 AST Representation

```racket
;; ast-types.rkt
#lang racket

(provide (all-defined-out))

;; Core AST node types
(struct ast-node (source-loc) #:transparent)

;; Literals
(struct ast-const ast-node (value) #:transparent)
(struct ast-var ast-node (name) #:transparent)

;; Binding forms
(struct ast-lambda ast-node (params body) #:transparent)
(struct ast-define ast-node (name value) #:transparent)
(struct ast-let ast-node (bindings body) #:transparent)

;; Control flow
(struct ast-if ast-node (test then else) #:transparent)
(struct ast-cond ast-node (clauses) #:transparent)
(struct ast-case ast-node (key clauses) #:transparent)

;; Application
(struct ast-app ast-node (operator operands) #:transparent)

;; Sequencing
(struct ast-begin ast-node (exprs) #:transparent)

;; First-class continuations
(struct ast-call/cc ast-node (proc) #:transparent)

;; Primitives
(struct ast-primitive ast-node (name) #:transparent)

;; Source location
(struct source-loc (file line col) #:transparent)
```

### 3.2 Control Flow Graph

```racket
;; cfg-types.rkt
#lang racket

(provide (all-defined-out))

;; CFG node represents a basic block
(struct cfg-node
  (id            ; Unique node identifier
   type          ; 'entry | 'exit | 'basic | 'branch | 'join
   statements    ; List of non-branching statements
   source-loc)   ; Source location for debugging
  #:transparent)

;; CFG edge represents control flow
(struct cfg-edge
  (from          ; Source node ID
   to            ; Target node ID
   condition     ; #f for unconditional, expression for conditional
   type)         ; 'normal | 'true-branch | 'false-branch | 'return
  #:transparent)

;; Complete control flow graph
(struct cfg
  (entry         ; Entry node ID
   exit          ; Exit node ID (may be multiple for continuations)
   nodes         ; Hash: node-id -> cfg-node
   edges         ; List of cfg-edge
   metadata)     ; Additional info (function name, etc.)
  #:transparent)

;; Cyclomatic complexity result
(struct complexity-metrics
  (v-g           ; Cyclomatic complexity V(G)
   nodes         ; Number of nodes N
   edges         ; Number of edges E
   components    ; Connected components P
   formula)      ; Which formula used: E - N + 2P
  #:transparent)
```

---

## 4. Algorithm Designs

### Algorithm: Parse R5RS to AST

**Input**: R5RS Scheme source code (string or port)
**Output**: AST (abstract syntax tree)

```racket
;; r5rs-parser.rkt
#lang racket

(require syntax/parse)

(provide parse-r5rs)

;; Main entry point
(define (parse-r5rs source)
  (cond
    [(string? source) (parse-r5rs-string source)]
    [(port? source) (parse-r5rs-port source)]
    [else (error "Invalid source type")]))

(define (parse-r5rs-string str)
  (with-input-from-string str
    (λ () (parse-r5rs-port (current-input-port)))))

(define (parse-r5rs-port port)
  ;; Read all expressions from port
  (let loop ([exprs '()])
    (let ([expr (read port)])
      (if (eof-object? expr)
          (reverse exprs)
          (loop (cons (sexpr->ast expr) exprs))))))

;; Convert S-expression to AST
(define (sexpr->ast sexpr)
  (match sexpr
    ;; Self-evaluating literals
    [(? number? n) (ast-const (source-loc/current) n)]
    [(? string? s) (ast-const (source-loc/current) s)]
    [(? boolean? b) (ast-const (source-loc/current) b)]

    ;; Variables
    [(? symbol? sym) (ast-var (source-loc/current) sym)]

    ;; Lambda
    [`(lambda ,params ,body ...)
     (ast-lambda (source-loc/current)
                 params
                 (map sexpr->ast body))]

    ;; Define
    [`(define ,name ,value)
     (ast-define (source-loc/current)
                 name
                 (sexpr->ast value))]

    ;; If
    [`(if ,test ,then)
     (ast-if (source-loc/current)
             (sexpr->ast test)
             (sexpr->ast then)
             (ast-const (source-loc/current) #f))] ; implicit else

    [`(if ,test ,then ,else)
     (ast-if (source-loc/current)
             (sexpr->ast test)
             (sexpr->ast then)
             (sexpr->ast else))]

    ;; Cond
    [`(cond ,clauses ...)
     (ast-cond (source-loc/current)
               (map (lambda (c)
                      (match c
                        [`(,test ,body ...)
                         (cons (sexpr->ast test) (map sexpr->ast body))]))
                    clauses))]

    ;; Let
    [`(let ,bindings ,body ...)
     (ast-let (source-loc/current)
              (map (lambda (b)
                     (match b
                       [`(,name ,value)
                        (cons name (sexpr->ast value))]))
                   bindings)
              (map sexpr->ast body))]

    ;; Begin
    [`(begin ,exprs ...)
     (ast-begin (source-loc/current)
                (map sexpr->ast exprs))]

    ;; Call/cc
    [`(call-with-current-continuation ,proc)
     (ast-call/cc (source-loc/current)
                  (sexpr->ast proc))]

    [`(call/cc ,proc)
     (ast-call/cc (source-loc/current)
                  (sexpr->ast proc))]

    ;; Application (must come last)
    [`(,operator ,operands ...)
     (ast-app (source-loc/current)
              (sexpr->ast operator)
              (map sexpr->ast operands))]

    [_ (error "Unrecognized form:" sexpr)]))
```

### Algorithm: Build Control Flow Graph

**Input**: AST (abstract syntax tree)
**Output**: CFG (control flow graph)

**Key Insights**:
1. Each **branching construct** (if, cond, case) creates a **decision node**
2. **Function calls** create edges but not cycles (unless recursive)
3. **Recursion** creates back-edges (cycles)
4. **call/cc** creates complex control flow (may need special handling)

```racket
;; cfg-builder.rkt
#lang racket

(require "ast-types.rkt"
         "cfg-types.rkt")

(provide build-cfg)

;; Global state for node ID generation
(define next-node-id 0)

(define (fresh-node-id!)
  (let ([id next-node-id])
    (set! next-node-id (+ id 1))
    id))

;; Main entry point
(define (build-cfg ast)
  ;; Reset node counter
  (set! next-node-id 0)

  ;; Create entry and exit nodes
  (define entry-id (fresh-node-id!))
  (define exit-id (fresh-node-id!))

  (define entry-node (cfg-node entry-id 'entry '() #f))
  (define exit-node (cfg-node exit-id 'exit '() #f))

  ;; Build CFG for AST body
  (define-values (body-cfg body-edges)
    (ast->cfg ast entry-id exit-id))

  ;; Combine everything
  (cfg entry-id
       exit-id
       (hash-set* (make-hash)
                  entry-id entry-node
                  exit-id exit-node
                  body-cfg)
       (append body-edges
               (list (cfg-edge entry-id (hash-ref body-cfg 'start) #f 'normal)))
       (hash)))

;; Convert AST to CFG nodes and edges
;; Returns: (values nodes edges)
(define (ast->cfg node entry-id exit-id)
  (match node
    ;; Constant: straight-line code
    [(ast-const loc val)
     (define id (fresh-node-id!))
     (values (hash id (cfg-node id 'basic (list node) loc))
             (list))]

    ;; Variable: straight-line code
    [(ast-var loc name)
     (define id (fresh-node-id!))
     (values (hash id (cfg-node id 'basic (list node) loc))
             (list))]

    ;; If: creates branch
    [(ast-if loc test then else)
     (define branch-id (fresh-node-id!))
     (define join-id (fresh-node-id!))

     ;; Build CFG for test, then, else
     (define-values (test-nodes test-edges)
       (ast->cfg test entry-id branch-id))

     (define-values (then-nodes then-edges)
       (ast->cfg then branch-id join-id))

     (define-values (else-nodes else-edges)
       (ast->cfg else branch-id join-id))

     ;; Create branch and join nodes
     (define branch-node (cfg-node branch-id 'branch (list node) loc))
     (define join-node (cfg-node join-id 'join '() loc))

     ;; Combine
     (values (hash-set* (make-hash)
                        branch-id branch-node
                        join-id join-node
                        test-nodes
                        then-nodes
                        else-nodes)
             (append test-edges
                     then-edges
                     else-edges
                     (list (cfg-edge branch-id (hash-ref then-nodes 'start) test 'true-branch)
                           (cfg-edge branch-id (hash-ref else-nodes 'start) test 'false-branch))))]

    ;; Lambda: creates nested CFG (could be analyzed separately)
    [(ast-lambda loc params body)
     ;; For simplicity, treat lambda as atomic (no internal CFG)
     ;; Advanced: recursively build CFG for body
     (define id (fresh-node-id!))
     (values (hash id (cfg-node id 'basic (list node) loc))
             (list))]

    ;; Application: may create cycle if recursive
    [(ast-app loc operator operands)
     (define id (fresh-node-id!))
     ;; Check if operator is a variable referencing a defined function
     ;; If so, and it matches current function, add back-edge
     (define node-obj (cfg-node id 'basic (list node) loc))

     ;; TODO: detect recursion by checking if operator == current function
     (values (hash id node-obj)
             (list))]

    ;; Begin: sequence of statements
    [(ast-begin loc exprs)
     ;; Thread CFG through each expression
     (define-values (nodes edges)
       (for/fold ([all-nodes (make-hash)]
                  [all-edges '()])
                 ([expr exprs])
         (define-values (expr-nodes expr-edges)
           (ast->cfg expr entry-id exit-id))
         (values (hash-union all-nodes expr-nodes)
                 (append all-edges expr-edges))))
     (values nodes edges)]

    ;; Call/cc: complex control flow
    [(ast-call/cc loc proc)
     ;; For now, treat as normal application
     ;; TODO: model first-class continuation properly
     (define id (fresh-node-id!))
     (values (hash id (cfg-node id 'basic (list node) loc))
             (list))]

    [_ (error "Unhandled AST node in CFG builder:" node)]))
```

### Algorithm: Compute Cyclomatic Complexity

**Input**: CFG (control flow graph)
**Output**: V(G) (cyclomatic complexity)

**Formula**: `V(G) = E - N + 2P`
- E = number of edges
- N = number of nodes
- P = number of connected components (usually 1)

```racket
;; cyclomatic.rkt
#lang racket

(require "cfg-types.rkt"
         "graph-utils.rkt")

(provide compute-cyclomatic-complexity)

;; Main entry point
(define (compute-cyclomatic-complexity cfg)
  (define nodes (hash-count (cfg-nodes cfg)))
  (define edges (length (cfg-edges cfg)))

  ;; Compute connected components using DFS
  (define components (count-connected-components cfg))

  ;; V(G) = E - N + 2P
  (define v-g (+ (- edges nodes) (* 2 components)))

  (complexity-metrics v-g nodes edges components "E - N + 2P"))

;; Count connected components via DFS
(define (count-connected-components cfg)
  (define visited (mutable-set))
  (define component-count 0)

  (for ([node-id (hash-keys (cfg-nodes cfg))])
    (unless (set-member? visited node-id)
      (dfs cfg node-id visited)
      (set! component-count (+ component-count 1))))

  component-count)

;; Depth-first search
(define (dfs cfg node-id visited)
  (set-add! visited node-id)

  ;; Visit all neighbors
  (for ([edge (cfg-edges cfg)])
    (when (= (cfg-edge-from edge) node-id)
      (define to (cfg-edge-to edge))
      (unless (set-member? visited to)
        (dfs cfg to visited)))))
```

---

## 5. gRPC Service Interface

```racket
;; metrics-api.rkt
#lang racket

(require "r5rs-parser.rkt"
         "cfg-builder.rkt"
         "cyclomatic.rkt")

(provide start-metrics-service)

;; Service implementation
(define (compute-metrics-handler request)
  (define source-code (hash-ref request 'source_code))
  (define program-id (hash-ref request 'program_id))

  (with-handlers ([exn:fail? (lambda (e)
                               (hash 'error (exn-message e)
                                     'program_id program-id))])
    ;; Parse → CFG → V(G)
    (define ast (parse-r5rs source-code))
    (define cfg (build-cfg ast))
    (define metrics (compute-cyclomatic-complexity cfg))

    ;; Return result
    (hash 'program_id program-id
          'v_g (complexity-metrics-v-g metrics)
          'nodes (complexity-metrics-nodes metrics)
          'edges (complexity-metrics-edges metrics)
          'components (complexity-metrics-components metrics)
          'error "")))

;; Start gRPC server
(define (start-metrics-service port)
  ;; TODO: Integrate with Racket gRPC library
  ;; For now, use simple HTTP JSON API or shell-based IPC
  (printf "Starting metrics service on port ~a\n" port)

  ;; Placeholder: read from stdin, write to stdout
  (let loop ()
    (define request (read-json))
    (unless (eof-object? request)
      (define response (compute-metrics-handler request))
      (write-json response)
      (flush-output)
      (loop))))
```

---

## 6. Testing Strategy

### 6.1 Known Examples

```racket
;; test/cyclomatic-test.rkt
#lang racket

(require rackunit
         "../cyclomatic.rkt"
         "../cfg-builder.rkt"
         "../r5rs-parser.rkt")

(define (test-program source expected-vg)
  (define ast (parse-r5rs source))
  (define cfg (build-cfg ast))
  (define metrics (compute-cyclomatic-complexity cfg))
  (check-equal? (complexity-metrics-v-g metrics) expected-vg))

;; Test cases from literature
(test-program "(define x 42)" 1)          ; Linear code: V(G) = 1

(test-program "(if (> x 0) 1 -1)" 2)     ; One branch: V(G) = 2

(test-program "(define (fact n)           ; Factorial: V(G) = 2
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))" 2)

(test-program "(cond                      ; Multiple branches: V(G) = 4
                 [(< x 0) 'negative]
                 [(= x 0) 'zero]
                 [(> x 0) 'positive])" 4)
```

### 6.2 Property Tests

```racket
;; Property: V(G) >= 1 for any program
(define (prop-vg-positive)
  (check-pred positive? (complexity-metrics-v-g metrics)))

;; Property: Linear code has V(G) = 1
(define (prop-linear-code-simple)
  (define ast (parse-r5rs "(define x 1)"))
  (define cfg (build-cfg ast))
  (define metrics (compute-cyclomatic-complexity cfg))
  (check-equal? (complexity-metrics-v-g metrics) 1))
```

---

## 7. Integration with Haskell Core

### Communication Options

**Option 1: gRPC (Preferred)**
- Use Racket gRPC library (if mature enough)
- Share `.proto` definitions
- Strongly typed, contract-first

**Option 2: JSON over HTTP**
- Simple REST API
- Easy debugging with curl
- Less type safety

**Option 3: Unix Pipes (Simplest)**
- Racket reads from stdin, writes to stdout
- Python bridge orchestrates communication
- Good for prototyping

### Shared Protocol Buffer

```protobuf
// proto/metrics_service.proto
syntax = "proto3";

package metrics_service;

service MetricsCalculator {
  rpc ComputeVG(SchemeProgram) returns (MetricsResult);
  rpc HealthCheck(Empty) returns (HealthStatus);
}

message SchemeProgram {
  string source_code = 1;
  string program_id = 2;
}

message MetricsResult {
  string program_id = 1;
  int32 v_g = 2;            // Cyclomatic complexity
  int32 nodes = 3;          // CFG nodes
  int32 edges = 4;          // CFG edges
  int32 components = 5;     // Connected components
  string formula = 6;       // "E - N + 2P"
  string error = 7;         // Error message if failed
}
```

---

## 8. Visualization (Optional)

```racket
;; pretty-print.rkt
#lang racket

(require "cfg-types.rkt")

(provide cfg->dot)

;; Generate Graphviz DOT format for visualization
(define (cfg->dot cfg)
  (define nodes-str
    (for/list ([(id node) (in-hash (cfg-nodes cfg))])
      (format "  ~a [label=\"~a\", shape=~a];"
              id
              (cfg-node-type node)
              (match (cfg-node-type node)
                ['entry "ellipse"]
                ['exit "ellipse"]
                ['branch "diamond"]
                ['join "invtriangle"]
                [_ "box"]))))

  (define edges-str
    (for/list ([edge (cfg-edges cfg)])
      (format "  ~a -> ~a [label=\"~a\"];"
              (cfg-edge-from edge)
              (cfg-edge-to edge)
              (or (cfg-edge-condition edge) ""))))

  (string-join
   (append (list "digraph CFG {")
           nodes-str
           edges-str
           (list "}"))
   "\n"))
```

---

## 9. Performance Considerations

### Expected Complexity

- **Parsing**: O(n) where n = source size
- **CFG construction**: O(n) where n = AST nodes
- **V(G) calculation**: O(V + E) where V = vertices, E = edges
- **Total**: Linear in program size

### Optimization Strategies

1. **Lazy evaluation**: Only build CFG when needed
2. **Caching**: Memoize CFG for unchanged programs
3. **Parallel testing**: Test corpus programs independently

### Scalability Target

- Programs with **< 1000 LOC**: milliseconds
- Programs with **1000-10000 LOC**: < 1 second
- Programs with **> 10000 LOC**: < 10 seconds

---

## 10. Open Questions

1. **call/cc handling**: How to properly model first-class continuations in CFG?
   - May need multiple exit nodes
   - Non-local control flow creates complex edges

2. **Macro expansion**: Should we expand macros before CFG construction?
   - R5RS has `syntax-rules` macros
   - May need Racket's expander

3. **Tail call optimization**: Should tail-recursive calls create cycles?
   - Semantically they're jumps, not calls
   - Affects V(G) calculation

4. **Higher-order functions**: How to handle `map`, `fold`, etc.?
   - Do we inline them?
   - Or treat as single CFG node?

These questions will be resolved through empirical testing against the Haskell H¹ calculator.

---

## 11. Next Steps

1. **Set up Racket project** (`info.rkt`, package structure)
2. **Implement R5RS parser** (S-expression → AST)
3. **Implement CFG builder** (AST → CFG)
4. **Implement V(G) calculator** (CFG → metrics)
5. **Write comprehensive tests** (known examples + properties)
6. **Add gRPC service** (or JSON API for prototyping)
7. **Visualize CFGs** (Graphviz integration)
8. **Benchmark performance**
