# SGP-ASLN Implementation

**Symbolic Grammar Parsing Automaton Semantic Lattice Network**

This module implements natural language processing for the Computational Scheme Theory framework, enabling queries like "compute H1 for program X" to be parsed and mapped to M-expressions for mathematical execution.

## Architecture

The implementation follows the four-layer FSM architecture:

- **Layer 1 (UI)**: `layer1-interface.rkt` - Accepts NL inputs, converts to M-expressions
- **Layer 2 (Query)**: `layer2-query.rkt` - Read-only views of semantic lattice/knowledge graph
- **Layer 3 (Coordination)**: `layer3-coordination.rkt` - Broadcasts parse events via pub/sub
- **Layer 4 (Mathematical Core)**: `layer4-core.rkt` - Validates NL, generates events, updates KG

## Core Components

### Phase 1: Parsing Foundation
- `grammar-parser.rkt` - EBNF grammar parser with production rules
- `parsing-fsm.rkt` - Deterministic finite state transducer
- `parse-events.rkt` - Event sourcing for parse activities

### Phase 2: Semantic Lattice Network
- `semantic-lattice.rkt` - Complete lattice (L, ≤) with meet/join operations
- `knowledge-graph.rkt` - Persistent knowledge graph (V, E, L)
- `lattice-ops.rkt` - Lattice inference operations

### Phase 3: Intent Mapping
- `semantic-frame.rkt` - Parsed query structure with enrichment
- `intent-mapper.rkt` - Functor mapping frames to M-expressions
- `domain-mappings.rkt` - Domain-specific mappings for Computational Scheme Theory

### Phase 4: Learning
- `learning-engine.rkt` - Continuous learning from interactions
- `context-manager.rkt` - Conversation context management

## Usage Example

```racket
(require "nlp/nlp-main.rkt")

;; Process natural language query
(define-values (m-expr events kg)
  (process-nl-query "compute H1 for program test"))

;; Convert NL to M-expression (Layer 1)
(define m-expr (nl-to-m-expression "compute H1"))
```

## Design Principles

1. **Pure Racket**: No external NLP libraries, pure symbolic parsing
2. **Deterministic**: Rule-based parsing, no probabilistic components
3. **Event-Sourced**: All parsing activities generate immutable S-expressions
4. **Four-Layer Compliance**: Strict adherence to FSM architecture

