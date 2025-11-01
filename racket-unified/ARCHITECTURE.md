# Architecture Overview

## Design Philosophy

**Everything in Pure Lisp**: The unified substrate demonstrates that all components can be elegantly implemented in native Lisp (Racket), leveraging:

- **Homoiconicity**: Code = Data = Code
- **First-class functions**: Combinators are just values
- **Macros**: Can define custom syntax
- **Dynamic typing**: Natural for Prolog-style unification
- **Tail recursion**: Efficient fixed-point iteration

## System Layers

```
┌─────────────────────────────────────────┐
│  User Interface / API                  │
│  (M-expressions, HTTP, CLI)            │
└─────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│  Logic Layer (Prolog/Datalog)          │
│  - Prolog: Top-down validation (Y)      │
│  - Datalog: Bottom-up inference (Z)     │
└─────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│  Execution Layer (S-expressions)        │
│  - Event store                          │
│  - FSM transitions                     │
│  - Homoiconic execution                 │
└─────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│  Algorithm Layer                        │
│  - Algorithm 1: Binding extraction       │
│  - Algorithm 2: Scope topology          │
│  - Algorithm 3: Čech complex            │
│  - Algorithm 4: Cohomology (H¹)         │
└─────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────┐
│  Service Bridges (Optional)              │
│  - Haskell H¹ service                   │
│  - Racket V(G) service                  │
└─────────────────────────────────────────┘
```

## Component Interactions

### M→S Pipeline

```
M-expression (command)
    ↓ [Prolog validation]
S-expression (event)
    ↓ [FSM execution]
State update
    ↓ [Datalog inference]
Derived facts
```

### H¹ Computation Pipeline

```
Scheme source
    ↓ [Parse]
AST
    ↓ [Alpha convert]
Hygienic AST
    ↓ [Algorithm 1: Extract bindings]
R_Scheme rig
    ↓ [Algorithm 2: Analyze scopes]
Topology
    ↓ [Algorithm 3: Build complex]
Čech complex
    ↓ [Algorithm 4: Compute H¹]
H¹ value
```

## Data Flow

### Binding Flow

1. **Source** → Parse → AST
2. **AST** → Alpha convert → Hygienic AST
3. **Hygienic AST** → Extract → R_Scheme rig (set of bindings)

### Scope Flow

1. **AST** → Analyze → Scope tree
2. **Scope tree** → Visibility regions → Topology
3. **Topology** → Open cover → Čech nerve

### Cohomology Flow

1. **Čech complex** → Incidence matrices → Ranks
2. **Ranks** → Formula → H¹ = (|N₁| - rank(M₁)) - rank(M₀)

## Key Abstractions

### M-Expressions (Meta-Language)
- Commands/intentions
- User-facing syntax
- Validated by Prolog

### S-Expressions (Object-Language)
- Events/facts/data
- Native Lisp (homoiconic)
- Executed directly

### Combinators
- **Y**: Lazy fixed point (Prolog infinite search)
- **Z**: Eager fixed point (Datalog termination)

### Logic Engines
- **Prolog**: Top-down, backward chaining
- **Datalog**: Bottom-up, forward chaining

## Extensibility

The architecture supports:

1. **miniKanren Upgrade**: Replace custom Prolog with miniKanren package
2. **Service Integration**: Call existing services or use pure Lisp
3. **Algorithm Refinement**: Improve implementations incrementally
4. **Feature Addition**: Add new algorithms or features

## Performance Considerations

- **Lazy evaluation**: Y-combinator enables coinductive reasoning
- **Eager fixpoint**: Z-combinator guarantees termination
- **Native execution**: No FFI overhead
- **REPL development**: Immediate feedback

This architecture enables the complete vision: **Everything is Lisp!** 🎯

