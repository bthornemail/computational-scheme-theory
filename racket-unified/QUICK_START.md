# SGP-ASLN Quick Start Guide

**Date**: 2025-01-31  
**Version**: 1.0

---

## Overview

The SGP-ASLN (Symbolic Grammar Parsing Automaton Semantic Lattice Network) system enables natural language queries to be processed and mapped to mathematical computations.

## Quick Usage

### Basic NL Query Processing

```racket
(require "src/nlp-integration.rkt")

;; Process a natural language query
(define-values (result success) 
  (process-nl-query-to-computation "compute H1"))

(if success
    (printf "H¹ = ~a\n" (pipeline-result-h1 result))
    (printf "Error: ~a\n" result))
```

### Parse Only (No Execution)

```racket
(require "src/nlp/layer4-core.rkt")

;; Parse NL query to M-expression
(define-values (m-expr events kg) 
  (process-nl-query "compute H1"))

(if m-expr
    (printf "M-expression: ~a\n" (m-expr->string m-expr))
    (printf "Parse failed\n"))
```

### Run Complete Demo

```bash
cd racket-unified
racket src/main.rkt
```

The demo includes:
1. M/S-expression pipeline
2. H¹ computation pipeline
3. **Natural Language Query Processing** ← NEW

## Supported Queries

### Current Operations

- **`"compute H1"`** - Computes first cohomology dimension
  - Maps to: `computeH1[]`
  - Executes: Algorithm 4 (H¹ computation)

- **`"compute H1 for program X"`** - Computes H¹ for specific program
  - Uses default source if program not found
  - Future: Full program resolution from corpus

### Query Structure

```
<ActionVerb> <Object> [<Modifier> <Entity>]*
```

**Examples**:
- `"compute H1"`
- `"validate hypothesis"`
- `"analyze patterns"`
- `"compare metrics"`

## Architecture

The system follows a four-layer architecture:

```
┌─────────────────────────────────────┐
│   Layer 1: UI (NL Interface)      │
│   - Accepts NL queries              │
│   - Converts to M-expressions       │
└──────────────┬──────────────────────┘
               │
┌─────────────────────────────────────┐
│   Layer 2: Query (Read-Only)       │
│   - Knowledge graph views          │
│   - Lattice queries                 │
└──────────────┬──────────────────────┘
               │
┌─────────────────────────────────────┐
│   Layer 3: Coordination (Pub/Sub)  │
│   - Event broadcasting              │
│   - Parse event distribution        │
└──────────────┬──────────────────────┘
               │
┌─────────────────────────────────────┐
│   Layer 4: Core (Mathematical FSM) │
│   - Grammar validation             │
│   - Parse event generation         │
│   - M-expression mapping           │
└──────────────┬──────────────────────┘
               │
┌─────────────────────────────────────┐
│   Unified Pipeline                 │
│   - Algorithm execution             │
│   - H¹ computation                  │
└─────────────────────────────────────┘
```

## Module Overview

### Core NLP Modules

- **`grammar-parser.rkt`** - EBNF grammar parser
- **`parsing-fsm.rkt`** - Finite state transducer
- **`intent-mapper.rkt`** - Maps frames to M-expressions
- **`semantic-lattice.rkt`** - Lattice data structure
- **`knowledge-graph.rkt`** - Knowledge graph persistence

### Integration

- **`layer4-core.rkt`** - Core NL processing
- **`nlp-integration.rkt`** - Integration with unified pipeline

## Event Sourcing

All parsing activities generate immutable S-expressions:

```racket
;; Event types:
'query-parsed
'verb-parsed
'entity-resolved
'parse-step
'parse-failed
```

Events can be replayed to reconstruct the knowledge graph:

```racket
(require "src/nlp/knowledge-graph.rkt")

;; Reconstruct KG from events
(define kg (foldl update-graph-from-event 
                  (empty-knowledge-graph) 
                  events))
```

## Extending the System

### Adding New Operations

1. Add object to `OBJECTS` list in `grammar-parser.rkt`
2. Add classification in `classify-operation` in `intent-mapper.rkt`
3. Add mapping function `map-<operation>` in `intent-mapper.rkt`
4. Add handler `handle-<operation>` in `nlp-integration.rkt`

### Adding Grammar Rules

Extend the EBNF grammar in `grammar-parser.rkt`:

```racket
(define (parse-<new-rule> tokens frame)
  "Parse new production rule"
  ;; Implementation
  )
```

## Troubleshooting

### Parse Failures

- Check tokenization: `(tokenize "your query")`
- Verify object recognition (case-sensitive matching)
- Check FSM state transitions

### Integration Issues

- Ensure M-expression structure matches expected format
- Verify operation classification returns correct symbol
- Check handler registration in `nlp-integration.rkt`

## Documentation

- `SGP-ASLN_FINAL_REPORT.md` - Complete implementation report
- `SGP-ASLN_COMPLETE.md` - Completion summary
- `INTEGRATION_COMPLETE.md` - Integration details
- `INTEGRATION_VERIFICATION.md` - Verification results

## Status

✅ **PRODUCTION READY**

All core components implemented, integrated, and verified.

---

**Quick Start Guide Version**: 1.0  
**Last Updated**: 2025-01-31
