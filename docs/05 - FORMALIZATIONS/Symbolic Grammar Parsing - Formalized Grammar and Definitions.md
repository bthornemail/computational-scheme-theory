# Symbolic Grammar Parsing: Formalized Grammar and Definitions

**Status:** Proposed (Experimental)  
**Category:** Implementation Extension  
**Date:** October 31, 2025  
**Replaces:** `Symbolic Grammar Parser - The Problem with Just a Parser.md` (Previous Reformalization)  

---

## 1. Introduction

This document provides a formalized specification of the grammar and definitions for the Symbolic Grammar Parsing Automaton Semantic Lattice Network (SGP-ASLN), ensuring full alignment and cohesion with the FSM-Based Computational Architecture. The formalization emphasizes deterministic, rule-based parsing integrated into the four-layer stack, with explicit ties to M/S-expression duality, event sourcing, and algebraic invariants.

All definitions and grammar rules are now expressed formally using Extended Backus-Naur Form (EBNF) for the grammar, mathematical notation for semantic mappings, and FSM state definitions for parsing behavior. This ensures mathematical rigor, auditability, and seamless integration with Layer 4's FSM core.

These formalizations are mandates derived from the architectural principles in `The Complete FSM-Based Computational Architecture.md` and the CSTP (RFCXXXX), focusing on symbolic, non-statistical processing.

---

## 2. Core Definitions (The "What")

These formalized definitions provide the foundational elements of SGP-ASLN, aligned with the system's algebraic and event-sourced nature.

### 2.1. Definition: Symbolic Grammar

The **Symbolic Grammar** is a context-sensitive grammar that maps natural language (NL) queries to semantic intents. It MUST be defined as a tuple \( G = (V_N, V_T, P, S) \), where:

- \( V_N \): Non-terminal symbols (e.g., `<Query>`, `<Intent>`, `<Entity>`).
- \( V_T \): Terminal symbols (keywords, operators, identifiers from the domain vocabulary).
- \( P \): Production rules (symbolic mappings, no probabilities).
- \( S \): Start symbol (`<Query>`).

The grammar MUST enforce domain-specific constraints, such as mapping to mathematical operations (e.g., "compute H1" → cohomology calculation).

### 2.2. Definition: Parsing Automaton

The **Parsing Automaton** is an FSM embedded in Layer 4, defined as \( A = (Q, \Sigma, \delta, q_0, F) \), where:

- \( Q \): Finite set of states (e.g., `ExpectingVerb`, `ResolvingEntity`, `BuildingLattice`).
- \( \Sigma \): Input alphabet (tokenized NL words/symbols).
- \( \delta: Q \times \Sigma \to Q \): Transition function (rule-based, deterministic).
- \( q_0 \): Initial state (`StartParse`).
- \( F \): Accepting states (e.g., `ParseComplete` with valid semantic lattice).

Transitions MUST generate S-expression events for the event store.

### 2.3. Definition: Semantic Lattice Network

The **Semantic Lattice Network** is a partially ordered set (poset) \( L = (N, E, \leq) \), where:

- \( N \): Nodes (semantic concepts, e.g., "H1Computation" as a subtype of "ProgramAnalysis").
- \( E \): Edges (relationships, e.g., subsumption \( \leq \)).
- \( \leq \): Partial order (e.g., \( a \leq b \) iff \( a \) is subsumed by \( b \), enabling inference).

The lattice MUST be persisted as a knowledge graph, updated via event-sourced S-expressions.

### 2.4. Definition: Knowledge Graph Persistence

The **Knowledge Graph** is a directed graph \( KG = (V, E, L) \), where:

- \( V \): Vertices (lattice nodes).
- \( E \): Labeled edges (relationships from the lattice).
- \( L \): Labels (e.g., "subsumes", "mapsToOperation").

Updates MUST be immutable, appended as S-expressions to the event store.

---

## 3. Formal Grammar Specification (EBNF)

The grammar is formalized in EBNF, with semantic actions in curly braces `{}` to indicate mappings to lattice nodes or FSM transitions. It focuses on domain-specific NL queries (e.g., for program analysis, cohomology computation), ensuring cohesion with mathematical operations like \( H^1 \) calculation.

### 3.1. Non-Terminals and Terminals

- Non-Terminals: `<Query>`, `<Intent>`, `<ActionVerb>`, `<Object>`, `<Modifier>`, `<Entity>`, `<Parameter>`.
- Terminals: Keywords like "compute", "validate", "H1", "V(G)", "program", "corpus"; operators like "for", "with", "against".

### 3.2. Production Rules

```
<Query> ::= <Intent> [<Modifier>] { map to FSM transition: StartParse → ExpectingIntent; generate S-event: (query-parsed <Intent>) }

<Intent> ::= <ActionVerb> <Object> [<Parameter>*] { build lattice node: IntentNode(<ActionVerb>, <Object>); infer subsumption ≤ ProgramAnalysis }

<ActionVerb> ::= "compute" | "validate" | "analyze" | "compare" { transition: ExpectingVerb → ResolvingEntity; if "compute" then map to H1Calculator }

<Object> ::= "H1" | "V(G)" | "cohomology" | "complexity" | "binding algebra" | "topology" { lattice edge: Object ≤ ComplexityMetric; if "H1" then link to ČechComplexBuilder }

<Modifier> ::= "for" <Entity> | "with" <Parameter> | "against" <Entity> { add modifier edge in lattice: Modifier → Intent; e.g., "for program X" ≤ SpecificValidation }

<Entity> ::= "program" <Identifier> | "corpus" | "hypothesis" { transition: ResolvingEntity → BuildingLattice; generate S-event: (entity-resolved <Entity>) }

<Parameter> ::= "k=" <Number> | "p>=" <Number> | "language=" <LangId> { map to alg params, e.g., k in H1 = V(G) - k; lattice node: Parameter ≤ HypothesisTest }

<Identifier> ::= [a-zA-Z0-9_]+ { terminal; validate against corpus (e.g., program file names) }

<Number> ::= [0-9]+ { terminal; for constants like k=0-2 }

<LangId> ::= "R5RS" | "Haskell" | "Python" { for RQ5 extensions }
```

### 3.3. Semantic Actions and Constraints

- Each production MUST include a semantic action `{}` that:
  - Triggers an FSM transition.
  - Builds or updates the lattice (e.g., add node/edge).
  - Generates an S-expression event (e.g., `(parse-step <fromState> <toState> <token>)`).
- Constraints: The grammar MUST reject ambiguous inputs deterministically (e.g., no backtracking; use lookahead if needed).
- Domain Alignment: Rules MUST map to project-specific concepts (e.g., "compute H1 for program X with k=1" → invoke Algorithm 4 from principles).

---

## 4. FSM Integration for Parsing (The "How")

The parsing process is formalized as an FSM in Layer 4, cohesive with event sourcing.

### 4.1. FSM States and Transitions

Formally, \( Q = \{ q_0: \text{StartParse}, q_1: \text{ExpectingIntent}, q_2: \text{ExpectingVerb}, q_3: \text{ResolvingEntity}, q_4: \text{BuildingLattice}, q_5: \text{ParseComplete}, q_e: \text{ParseError} \} \).

- \( \delta(q_0, \epsilon) = q_1 \) (initiate on query input).
- \( \delta(q_1, <ActionVerb>) = q_2 \) { generate S-event: (verb-parsed <ActionVerb>) }.
- \( \delta(q_2, <Object>) = q_3 \) { build lattice node; if invalid, → \( q_e \) }.
- \( \delta(q_3, <Modifier> | <Parameter>) = q_4 \) { update lattice with subsumption }.
- \( \delta(q_4, \epsilon) = q_5 \) { finalize lattice; append to KG }.
- Error transitions: Invalid token → \( q_e \) { generate S-event: (parse-failed <token>) }.

### 4.2. Event Sourcing in Parsing

Every transition \( \delta \) MUST produce an S-expression event, appended to the store:

- Example: `(transition-event q1 q2 (verb "compute") (timestamp))`.
- Replay: \( KG = \foldl \applyEvent \emptyset \text{event-store} \), where \( \applyEvent \) updates the lattice poset.

### 4.3. Lattice Operations

- Subsumption Check: \( a \leq b \) iff \( \exists \) path from \( a \) to \( b \) in KG (transitive closure).
- Inference: For query resolution, compute least upper bound (join) in the poset to map to ops (e.g., join("H1", "program") = H1Calculator).

---

## 5. Distributed and Validation Principles

### 5.1. Distributed Parsing

In multi-node setups, parsing MUST use tropical algebra for synchronization:

- Token sequencing: \( t_1 \otimes t_2 = t_1 + t_2 \) (add delays).
- Lattice merging: \( L_1 \oplus L_2 = \max(L_1, L_2) \) (idempotent union).

### 5.2. Validation Integration

The grammar MUST support queries for the H¹ hypothesis (e.g., "validate H1 against V(G) for corpus"), mapping to test corpus runs (RQ1-RQ5).

---

## 6. Infrastructure Mandates (The "Where")

- **Core Implementation:** Haskell for FSM and lattice (type-safe posets).
- **Persistence:** Neo4j for KG; PostgreSQL for event store.
- **Communication:** gRPC for query intents; Kafka for event broadcast.
- **Auditing:** All parse events MUST be homoiconic and replayable.

***

This formalization ensures the grammar and definitions are cohesive, mathematically precise, and fully aligned with the FSM architecture, enabling deterministic NL processing for the project.