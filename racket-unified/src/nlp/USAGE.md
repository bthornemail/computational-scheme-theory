# SGP-ASLN Usage Guide

## Quick Start

### Basic NL Query Processing

```racket
(require "nlp/nlp-main.rkt")

;; Process a natural language query
(define-values (m-expr events knowledge-graph)
  (process-nl-query "compute H1 for program test"))

;; Access results
(m-expr-op m-expr)     ; 'computeH1
(m-expr-args m-expr)   ; '(test)
(length events)        ; Number of parse events generated
```

### Layer 1: NL to M-Expression Conversion

```racket
(require "nlp/layer1-interface.rkt")

;; Convert NL directly to M-expression
(define m-expr (nl-to-m-expression "compute H1"))
;; Returns: (m-expr 'computeH1 '())
```

### Layer 4: Full Pipeline

```racket
(require "nlp/layer4-core.rkt")

;; Validate and process NL query
(define-values (valid? event)
  (validate-nl-query "compute H1" #f))

;; Full processing pipeline
(define-values (m-expr events kg)
  (process-nl-query "compute H1 for program example"))
```

## Supported Query Patterns

### Compute Operations
- "compute H1"
- "compute H1 for program X"
- "compute V(G) for program Y"
- "compute H1 with k=1"

### Validation Operations
- "validate hypothesis"
- "validate H1 against V(G)"

### Analysis Operations
- "analyze patterns in program Z"
- "compare H1 and V(G) for corpus"

## Event Sourcing

All parse activities generate immutable S-expressions:

```racket
(require "nlp/parse-events.rkt")

;; Events are automatically appended to event store
;; Replay events to reconstruct knowledge graph
(define kg (replay-parse-events (event-store)))
```

## Knowledge Graph Queries

```racket
(require "nlp/layer2-query.rkt")

;; Query lattice (read-only)
(define results (query-lattice 'get-concept '((id "H1"))))

;; Get concept hierarchy
(define hierarchy (get-concept-hierarchy))
```

## Integration with Unified Pipeline

To integrate with the existing unified pipeline:

```racket
(require "nlp/layer1-interface.rkt"
         "algorithms/unified-pipeline.rkt")

;; Process NL query
(define m-expr (nl-to-m-expression "compute H1 for program test"))

;; Execute via unified pipeline
(define result (compute-h1-from-source-detailed 
                (m-expr-args m-expr)))
```

## Error Handling

Invalid queries will generate parse-failed events:

```racket
(define-values (state events)
  (parse-query-fsm "invalid query"))

;; Check for errors
(if (eq? (parse-state-current-state state) 'ParseError)
    (displayln "Parse failed")
    (displayln "Parse successful"))
```

## Advanced: Custom Grammar Rules

Extend the grammar by modifying `grammar-parser.rkt`:

```racket
;; Add new action verb
(define ACTION-VERBS '("compute" "validate" "analyze" "explain" ...))

;; Add new object type
(define OBJECTS '("H1" "V(G)" "cohomology" ...))
```

## Testing

Run the test suite:

```bash
cd racket-unified
racket test/test-nlp/test-grammar.rkt
racket test/test-nlp/test-fsm.rkt
racket test/test-nlp/test-lattice.rkt
racket test/test-nlp/test-intent-mapper.rkt
racket test/test-nlp/test-nlp-integration.rkt
racket test/test-nlp/test-nlp-corpus.rkt
```

