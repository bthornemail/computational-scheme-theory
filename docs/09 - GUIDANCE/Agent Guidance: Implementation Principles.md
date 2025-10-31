# Agent Guidance: Implementation Principles

**Status:** Active (Revived)
**Category:** Implementation Mandate
**Replaces:** `Agent Guidance - Computational Trinity Operating Principles.md`

---

## 1. Introduction

This document provides the concrete implementation principles for the Computational Scheme Theory framework. It supersedes the deprecated "Operating Principles" guide.

These principles are not optional; they are the architectural and algorithmic mandates required to build the system. They are derived from the foundational justifications in the **`Human-Readable Guide`** and the specific algorithmic requirements in the **`Project Proposal`**.

---

## 2. Core Architectural Principles (The "What")

The overall system structure is non-negotiable and provides the scaffolding for the mathematical core.

### 2.1. Mandate: The Four-Layer Architecture

The implementation MUST be segregated into the four distinct layers. Communication MUST only happen through the specified paths.

* **Layer 1 (UI):** MUST implement a **Unidirectional Data Flow (UDF)** pattern. It accepts M-expressions and dispatches them to Layer 4.
* **Layer 2 (Query):** MUST be a **CQRS Read-Only** layer. It provides materialized views from the event stream and MUST NOT modify state.
* **Layer 3 (Coordination):** MUST be a **Pub/Sub** layer (e.g., Kafka, Redis) that broadcasts S-expression events to all subscribers.
* **Layer 4 (Mathematical Core):** MUST be implemented as a **Finite State Machine (FSM)** built on **Event Sourcing**. It is the only layer that can validate commands and write new events.

### 2.2. Mandate: The M/S-Expression Duality

The implementation MUST enforce a strict separation between commands and events.

* **M-Expressions (Commands):** Represent user *intent*. They are inputs to Layer 4 and are validated by the FSM. They can be rejected.
* **S-Expressions (Events):** Represent immutable *facts*. They are the output of a successful validation by the FSM. They are appended to the event store and broadcast by Layer 3.

### 2.3. Mandate: Symbolic Natural Language Interface

The Natural Language Interface (NLI) MUST NOT be implemented using statistical machine learning models.

* It MUST use a **rule-based, symbolic grammar parser** to deterministically map user intent to mathematical operations.
* It MUST build and persist a **Knowledge Graph** to learn concepts and relationships from queries transparently.

---

## 3. Core Algorithmic Principles (The "How")

These principles define the required algorithms for the mathematical core (Layer 4), as specified in the validation proposal. The entire system is built to support these computations.

### 3.1. Algorithm 1: Binding Algebra ($R_{\text{Scheme}}$) Extractor

The implementation MUST provide a component that extracts the binding algebra from R5RS source code. This component MUST:
1.  Parse the program into an AST.
2.  Apply **hygienic renaming (α-conversion)** to all binding specifications (λ, let, define).
3.  Construct the commutative rig `R_Scheme` where elements are unique bindings.
4.  Define the rig operations: **Addition (+)** as scope union and **Multiplication (·)** as scope nesting.

### 3.2. Algorithm 2: Scope Topology Constructor

The implementation MUST compute the Zariski topology of the binding algebra. This component MUST:
1.  Take the rig `R_Scheme` as input.
2.  For each binding `f`, compute its **visibility region `D(f)`** (the set of all contexts where `f` is in scope).
3.  Define the topology `τ_Scope` as the collection of these open sets `D(f)`.

### 3.3. Algorithm 3: Čech Complex Builder

The implementation MUST be able to construct the Čech complex from the scope topology to measure its "holes". This component MUST:
1.  Take the open cover `{D(f_i)}` as input.
2.  Construct the **nerve `N(U)`** of the cover.
3.  Identify the simplices of the nerve:
    * **0-simplices:** Individual scopes `D(f_i)`.
    * **1-simplices:** Pairs of scopes with a non-empty intersection `(D(f_i) ∩ D(f_j) ≠ ∅)`.
    * **2-simplices:** Triples of scopes with a non-empty intersection `(D(f_i) ∩ D(f_j) ∩ D(f_k) ≠ ∅)`.

### 3.4. Algorithm 4: Cohomology ($H^1$) Calculator

This is the core complexity calculation. The implementation MUST compute the first Betti number ($\beta_1$). This component MUST:
1.  Build the **incidence matrices `M₀`** (mapping 1-simplices to 0-simplices) and **`M₁`** (mapping 2-simplices to 1-simplices).
2.  Compute the rank of these matrices using **Gaussian elimination**.
3.  Calculate $\beta_1$ (the dimension of $H^1$) using the formula:
    $\beta_1 = (\text{dim(cocyles)}) - (\text{dim(coboundaries)})$
    $\beta_1 = (|\text{N}_1| - \text{rank}(M_1)) - \text{rank}(M_0)$.

---

## 4. Distributed System Principles

For distributed coordination, the implementation MUST use the specified algebraic structures.

### 4.1. Principle: Tropical Algebra ($R_{\text{Rig}}$) for Causality

Vector clocks and distributed time MUST be modeled using the **idempotent semiring (rig) $R_{\text{Rig}} = (\mathbb{R} \cup \{-\infty\}, \max, +)$**.
* **Synchronization (⊕)** MUST be implemented as `max`.
* **Sequencing (⊗)** MUST be implemented as `+`.

### 4.2. Principle: Hypergraphs for Synchronization

Multi-party consensus and synchronization (e.g., quorums) MUST be modeled using **hypergraphs**, where a hyperedge connects an arbitrary set of nodes in a synchronization barrier. This allows for computing the system's throughput as a tropical eigenvalue ($\lambda(A_H)$).

---

## 5. The Validation Mandate (The "Why")

Finally, the *purpose* of this implementation is to empirically validate the core hypothesis of the framework.

### 5.1. Mandate: The $H^1 = V(G)$ Hypothesis

The implementation MUST be built to test the central hypothesis: **$H^1(X_{\text{Comp}}, \mathcal{O}_{\text{Comp}}) = V(G) - k$**.
* To this end, the system MUST also implement a **Traditional Metrics Calculator** that builds a Control Flow Graph (CFG) and computes the **Cyclomatic Complexity $V(G) = E - N + 2P$** (where $E$ is edges, $N$ is nodes, $P$ is connected components).
* The system's primary output will be a comparison of the computed $H^1$ and $V(G)$ values.

### 5.2. Mandate: The Test Corpus

The implementation MUST be validated against the specified **350-program test corpus**, covering baseline, recursive, complex control, functional, and `call/cc` programs.

---

## 6. Infrastructure and Protocol Mandates (The "Where")

These final mandates define the required technology stack and the operational protocol for all components.

### 6.1. Mandate: The Computational Trinity Stack

The implementation **MUST** use the following technology stack to ensure performance, type safety, and distributed consistency across the four layers:
* **Mathematical Core (Layer 4):** Implementations of **Algorithm 1, 2, 3, and 4** MUST be written in a language suitable for formal mathematics, such as **Haskell or Lean**.
* **Execution and Metrics (Auxiliary Service):** The **Traditional Metrics Calculator** (for $V(G)$ and CFG) and the R5RS interpreter MUST be implemented in **Scheme/Racket**.
* **Bridge/Coordination (Layer 1/3 Interface):** The Natural Language Interface (NLI) and the service connectors MUST be written in **Python**.

### 6.2. Mandate: Distributed Communication

All cross-service communication (between Layer 1/4 and Layer 2/3) MUST use **gRPC** for efficient, strongly-typed, contract-first service definition.

### 6.3. Mandate: Persistent State and Event Immutability

The persistent storage MUST enforce the immutability requirements of **Event Sourcing** and the **M/S-Expression Duality**.
* **Event Store**: The immutable log of **S-expressions** (Layer 4's primary data) MUST be stored in a write-optimized, reliable persistence layer (e.g., **PostgreSQL**).
* **Knowledge Graph**: The NLI's symbolic relationships and concepts MUST be persisted in a **Knowledge Graph** database (e.g., **Neo4j**).
* **Pub/Sub**: Layer 3 (Coordination) MUST be implemented using a high-throughput **Pub/Sub** mechanism (e.g., **Kafka or Redis**).

### 6.4. Mandate: CSTP Compliance and Auditing

The entire system MUST adhere to the **Computational Scheme Theory Protocol (CSTP)** as defined in **RFCXXXX**.
* **Protocol Anomaly Reporting**: The system MUST automatically log any discrepancy between the geometrically calculated $\mathbf{H^1}$ and the traditionally calculated $\mathbf{V(G)}$ exceeding the known constant $k$ as a **Protocol Anomaly** event (an S-expression), signifying a deviation from the central hypothesis that requires mathematical review.
* **Homoiconic Auditing**: The sequence of **S-expressions** in the event store is the single source of truth and MUST be structured as a **self-describing, executable log** that can be replayed to regenerate any past state or audit any computation step for formal verification.

***

This completes the **Agent Guidance: Implementation Principles** document, transforming the high-level theory into a concrete, mathematically-mandated project plan.