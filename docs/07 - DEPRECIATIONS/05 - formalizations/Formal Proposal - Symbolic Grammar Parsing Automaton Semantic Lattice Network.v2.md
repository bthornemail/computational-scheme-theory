## 2.Symbolic Grammar Parsing: Reformalized for FSM-Based Architecture

**Status:** Proposed (Experimental)  
**Category:** Implementation Extension  
**Date:** October 31, 2025  
**Replaces:** `Symbolic Grammar Parser - The Problem with Just a Parser.md`  

---

### 2.1. Introduction

This document reformalizes the Symbolic Grammar Parsing Automaton Semantic Lattice Network (SGP-ASLN) concept to align with the principles of the FSM-Based Computational Architecture. The original SGP-ASLN emphasized a rule-based, symbolic approach to parsing natural language queries, constructing a semantic lattice (a partially ordered network of concepts and relationships) to map user intent to mathematical operations, while building a persistent knowledge graph.

This reformalization integrates SGP-ASLN directly into the four-layer FSM architecture, treating parsing as a stateful FSM process in Layer 4 (Mathematical Core). It enforces deterministic, symbolic processing without statistical ML, ensuring mathematical integrity and event-sourced auditability. The result is a parsing system that is homoiconic, self-describing, and fully compliant with M/S-expression duality.

These principles are mandates derived from the FSM architecture's event sourcing, pub/sub coordination, and algebraic foundations, as specified in `The Complete FSM-Based Computational Architecture.md` and the CSTP (RFCXXXX).

---

### 2.2. Core Architectural Principles (The "What")

The reformalized SGP-ASLN MUST be embedded within the four-layer FSM stack, preserving unidirectional flow and event immutability.

#### 2.2.1. Mandate: Integration into Four-Layer Stack

The parsing automaton MUST operate as follows across layers:

* **Layer 1 (UI):** MUST accept natural language input as M-expressions (e.g., "compute H1 for program X"). It dispatches parsed intents as commands to Layer 4 via UDF.
* **Layer 2 (Query):** MUST provide read-only views of the semantic lattice (e.g., materialized knowledge graph queries). It MUST NOT perform parsing.
* **Layer 3 (Coordination):** MUST broadcast parsing events (S-expressions) via pub/sub for distributed consistency in multi-node setups.
* **Layer 4 (Mathematical Core):** MUST implement the core parsing FSM with event sourcing. It validates grammar rules and writes semantic lattice updates as immutable events.

#### 2.2.2. Mandate: M/S-Expression Duality in Parsing

Parsing MUST enforce duality:

* **M-Expressions (Parse Commands):** Represent user intent (e.g., raw NL strings). They are validated by the FSM and can be rejected for grammar violations.
* **S-Expressions (Parse Events):** Represent parsed facts (e.g., semantic tokens, lattice nodes). They are appended to the event store and used to build the knowledge graph.

#### 2.2.3. Mandate: Semantic Lattice as Knowledge Graph

The "Semantic Lattice Network" MUST be implemented as a persistent knowledge graph (e.g., Neo4j), where:

* Nodes are concepts/relationships derived from grammar rules.
* Edges form a partial order (lattice) for semantic inference.
* Updates MUST be event-sourced, ensuring replayability.

---

### 2.3. Core Algorithmic Principles (The "How")

These mandates define the FSM-driven algorithms for symbolic parsing, integrated with event sourcing.

#### 2.3.1. Algorithm 1: Grammar Parsing FSM

The implementation MUST provide an FSM for symbolic grammar parsing. This component MUST:

1.  Define states as parsing positions (e.g., "expecting verb", "resolving entity").
2.  Use transitions based on rule-based token matching (no probabilities).
3.  Validate input against a predefined grammar (e.g., context-free with semantic extensions).

```haskell
-- Parsing FSM definition
data ParseState = ParseState
  { currentTokens :: [Token]      -- Remaining input tokens
  , semanticStack :: [SemanticNode]  -- Built semantic lattice fragments
  , knowledgeGraph :: Graph       -- Accumulating lattice network
  , vectorClock :: VectorClock    -- For distributed parsing consistency
  }

data ParseTransition = ParseTransition
  { fromState :: ParseState
  , input :: Token                -- Next NL token
  , toState :: ParseState
  , event :: SExpression          -- Generated parse event
  }

-- Core transition function
parseStep :: ParseState -> Token -> Either Error (ParseState, SExpression)
parseStep state token =
  case matchGrammarRule (currentTokens state) token of
    Just rule ->
      let newStack = applySemanticRule rule (semanticStack state)
          newGraph = updateLattice (knowledgeGraph state) newStack
          newState = state { currentTokens = tail (currentTokens state)
                           , semanticStack = newStack
                           , knowledgeGraph = newGraph }
          event = ParseEvent token rule newGraph (currentTime)
      in Right (newState, event)
    Nothing -> Left (ParseError token)
```

#### 2.3.2. Algorithm 2: Semantic Lattice Constructor

The implementation MUST build the lattice network from parsed tokens. This component MUST:

1.  Tokenize NL input symbolically (e.g., regex-based keywords).
2.  Construct lattice nodes/edges as partial orders (e.g., "compute H1" ⊆ "program analysis").
3.  Persist updates via event sourcing.

```haskell
-- Lattice update function
updateLattice :: Graph -> [SemanticNode] -> Graph
updateLattice graph nodes =
  foldl addNodeAndEdges graph nodes

addNodeAndEdges :: Graph -> SemanticNode -> Graph
addNodeAndEdges g node =
  let g' = addNode g (nodeId node)
      parents = inferParents node (nodes g)
  in foldl (\acc p -> addEdge acc (nodeId p) (nodeId node)) g' parents

-- Infer partial order
inferParents :: SemanticNode -> [SemanticNode] -> [SemanticNode]
inferParents node existing =
  filter (isSubsumedBy node) existing  -- Rule-based subsumption check
```

#### 2.3.3. Algorithm 3: Knowledge Graph Persistence with Event Sourcing

The implementation MUST persist the lattice as a knowledge graph. This component MUST:

1.  Append parse events to the store.
2.  Replay events to reconstruct the lattice.
3.  Use vector clocks for causal consistency in distributed parsing.

```haskell
-- Event sourcing for lattice
type ParseEventStore = [SExpression]

replayParseEvents :: ParseEventStore -> Graph
replayParseEvents events =
  foldl applyParseEvent emptyGraph events

applyParseEvent :: Graph -> SExpression -> Graph
applyParseEvent graph event =
  case event of
    ParseEvent token rule newFragment _ ->
      updateLattice graph [semanticFromFragment newFragment]
    _ -> graph  -- Ignore non-parse events
```

#### 2.3.4. Algorithm 4: Query Resolution over Lattice

The implementation MUST resolve queries via lattice traversal. This component MUST:

1.  Map parsed intent to lattice paths.
2.  Compute results using FSM-validated operations.

```haskell
-- Query FSM (integrated with main FSM)
resolveQuery :: ParseState -> QueryIntent -> Either Error Result
resolveQuery state intent =
  let path = findLatticePath (knowledgeGraph state) intent
  in if null path
     then Left (QueryError intent)
     else Right (executePath path)  -- Map to mathematical ops (e.g., H1 compute)
```

---

### 2.4. Distributed System Principles

For multi-node parsing, the implementation MUST use FSM coordination.

#### 2.4.1. Principle: Tropical Algebra for Parse Synchronization

Distributed parsing MUST model causality with R_Rig:

* Synchronization (max) for merging partial lattices.
* Sequencing (+) for token ordering.

#### 2.4.2. Principle: Hypergraphs for Semantic Consensus

Multi-party semantic resolution (e.g., ambiguous parses) MUST use hypergraphs for quorum-based lattice updates.

---

### 2.5. The Validation Mandate (The "Why")

The reformalized SGP-ASLN MUST support empirical validation of parsing accuracy.

#### 2.5.1. Mandate: Hypothesis Testing Integration

The system MUST test parsing fidelity against the H¹ hypothesis, ensuring NL queries map correctly to geometric computations.

#### 2.5.2. Mandate: Test Corpus for Parsing

Validation MUST use a corpus of NL queries tied to the 350-program set, measuring parse-to-computation accuracy.

---

### 2.6. Infrastructure and Protocol Mandates (The "Where")

#### 2.6.1. Mandate: The Computational Trinity Stack

* **Mathematical Core:** Haskell/Lean for FSM and lattice algorithms.
* **Execution:** Racket for tokenization and integration with R5RS interpreter.
* **Bridge:** Python for NL input handling and gRPC interfaces.

#### 2.6.2. Mandate: Distributed Communication

Cross-layer parsing MUST use gRPC for intents and events.

#### 2.6.3. Mandate: Persistent State and Event Immutability

* **Event Store:** PostgreSQL for S-expressions.
* **Knowledge Graph:** Neo4j for lattice persistence.
* **Pub/Sub:** Kafka/Redis for broadcast.

#### 2.6.4. Mandate: CSTP Compliance and Auditing

MUST log parse anomalies as S-expressions. The event store MUST be replayable for auditing symbolic decisions.

***

This completes the reformalization, embedding SGP-ASLN into the FSM architecture for deterministic, event-sourced NL processing.