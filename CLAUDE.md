# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a theoretical research repository exploring **Computational Scheme Theory** - a mathematical framework that unifies program semantics with algebraic geometry, distributed systems coordination, and natural language interfaces. The work is purely theoretical documentation with no implementation code yet.

**Central Hypothesis**: The static binding structure of R5RS Scheme programs forms a commutative algebraic object (rig) whose geometric spectrum captures program complexity through the equation: `H¹(X_Comp, O_Comp) = V(G) - k`

Where:
- `H¹` is a topological invariant computed from static scope analysis (Čech cohomology)
- `V(G)` is cyclomatic complexity from control flow graphs
- `k` is a normalization constant

## Document Structure

The `docs/` directory is organized by document type:

- **01 - RESEARCH**: Initial investigations and analysis strategies (Rumsfeldian Analysis)
- **02 - ANALYSIS**: Deep mathematical analyses (Grothendieck schemes, binding algebra)
- **03 - ASSESMENTS**: Evaluations of specific problems (M/S-expression duality, FSMs)
- **04 - REFLECTIONS**: High-level philosophical connections
- **05 - FORMALIZATIONS**: Formal mathematical definitions and proofs
- **05 - SPECIFICATIONS**: RFC-style protocol specifications (CSTP)
- **06 - PROPOSALS**: Project and validation proposals
- **07 - DEPRECIATIONS**: Deprecated documents
- **08 - EXPLANATIONS**: Human-readable guides for practitioners
- **09 - GUIDANCE**: **Implementation mandates for AI agents (READ THIS FIRST)**

## Key Documents for Understanding the Theory

1. **Start here**: `docs/09 - GUIDANCE/Agent Guidance: Implementation Principles.md` - Concrete implementation mandates
2. **Human-readable overview**: `docs/08 - EXPLANATIONS/Computational Scheme Theory - A Human-Readable Guide.md`
3. **Validation strategy**: `docs/06 - PROPOSALS/Project Proposal - Empirical Validation of Computational Scheme Theory.md`
4. **Protocol spec**: `docs/05 - SPECIFICATIONS/RFCXXXX - Computational Scheme Theory Protocol Specification.v1.md`
5. **Architecture**: `docs/05 - FORMALIZATIONS/The Complete FSM-Based Computational Architecture.md`

## Core Architecture Principles

### The Four-Layer Architecture (Mandatory)

All implementations MUST follow this structure:

1. **Layer 1 (UI)**: Unidirectional Data Flow (UDF) - accepts M-expressions (commands representing intent)
2. **Layer 2 (Query)**: CQRS Read-Only - materialized views from event stream, NO state modification
3. **Layer 3 (Coordination)**: Pub/Sub (Kafka/Redis) - broadcasts S-expression events (immutable facts)
4. **Layer 4 (Mathematical Core)**: FSM + Event Sourcing - validates commands, writes events

### M-Expression / S-Expression Duality (Critical)

- **M-Expressions**: Meta-language commands representing user *intent* (can be rejected)
- **S-Expressions**: Object-language events representing immutable *facts* (append-only log)

This duality is the foundation of the event sourcing pattern in this system.

## Required Algorithms (Mathematical Core)

Implementation MUST provide these four algorithms:

1. **Binding Algebra Extractor**: Parse R5RS code → apply α-conversion → construct commutative rig R_Scheme
2. **Scope Topology Constructor**: Compute Zariski topology from visibility regions D(f)
3. **Čech Complex Builder**: Construct nerve N(U) from open cover to identify simplices
4. **Cohomology Calculator**: Compute β₁ (first Betti number) using incidence matrices

These algorithms validate the central hypothesis: H¹ = V(G) - k

## Technology Stack Mandates

- **Mathematical Core**: Haskell or Lean (formal mathematics)
- **Metrics/Execution**: Scheme/Racket (R5RS interpreter, CFG, V(G) calculator)
- **Bridge/NLI**: Python (Natural Language Interface, service connectors)
- **Communication**: gRPC (strongly-typed, contract-first)
- **Event Store**: PostgreSQL (immutable S-expression log)
- **Knowledge Graph**: Neo4j (NLI symbolic relationships)
- **Pub/Sub**: Kafka or Redis (Layer 3 coordination)

## Natural Language Interface Requirements

The NLI MUST:
- Use **rule-based symbolic grammar parsing** (NOT statistical ML models)
- Build and persist a **Knowledge Graph** for concept learning
- Map user intent deterministically to mathematical operations

## Distributed Systems: Tropical Algebra

Vector clocks and causality MUST use the idempotent semiring (tropical rig):
- `R_Rig = (ℝ ∪ {-∞}, max, +)`
- Synchronization: `max` operation
- Sequencing: `+` operation
- Multi-party consensus: Hypergraphs with tropical eigenvalues

## Validation Requirements

The system MUST:
- Test the H¹ = V(G) hypothesis on a **350-program test corpus**
- Log **Protocol Anomaly** events when |H¹ - V(G)| > k
- Maintain **homoiconic auditing** through the S-expression event log
- Support replay of any computation from the event store

## Working with This Repository

### Current State
- Pure documentation and theoretical specifications
- No implementation code exists yet
- All content is markdown in `docs/`

### When Adding Content
- Follow RFC 2119 language (MUST, SHOULD, MAY) in specifications
- Place documents in appropriate numbered directories
- Reference the Implementation Principles document for mandates
- Maintain the M/S-expression duality in all system descriptions

### When Implementing
- Implementation Principles document (`docs/09 - GUIDANCE/`) is non-negotiable
- Architecture mandates override any architectural preferences
- All components must support the four required algorithms
- Event sourcing is mandatory (immutable S-expression log)

## Mathematical Foundations

- **Algebraic Geometry**: Grothendieck schemes, Zariski topology, structure sheaves
- **Homological Algebra**: Čech cohomology, simplicial complexes, Betti numbers
- **Tropical Geometry**: Max-plus algebra, idempotent semirings, tropical eigenvalues
- **Type Theory**: Binding algebras, continuation semantics, hygienic macros
- **Category Theory**: Functors between PL semantics and algebraic structures

## Research Context

This framework aims to:
1. Measure program complexity through topological invariants (H¹)
2. Model distributed coordination using rigorous algebra (tropical rig)
3. Provide deterministic natural language interfaces (symbolic parsing)
4. Unify these three domains through scheme-theoretic foundations

The empirical validation will determine if program binding structure (static) truly corresponds to control flow complexity (dynamic) through algebraic geometry.
