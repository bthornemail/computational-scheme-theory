# Unified Lisp Substrate

**Pure Lisp implementation of Computational Scheme Theory**

This is a complete unified implementation in pure Racket (Lisp) that brings together:
- M/S-expression duality (native)
- Prolog/Datalog logic engines (embedded)
- Y/Z combinators (native)
- All four algorithms (pure Lisp)
- Service bridges for hybrid operation

## Quick Start

```bash
cd racket-unified
racket src/main.rkt
```

Or load modules directly:

```racket
(require "src/algorithms/unified-pipeline.rkt")
(compute-h1-from-source-detailed "(lambda (x) x)")
```

## Architecture

### Core Components

- **M/S-Expressions**: Native Lisp representation
- **Combinators**: Y (lazy) and Z (eager) fixed-point recursion
- **Logic Engines**: Prolog (custom) and Datalog (with Z-combinator)
- **Algorithms**: Complete pipeline from source to H¹

### Service Bridges

- **Haskell Bridge**: Call existing H¹ service for comparison
- **Racket Bridge**: Call existing V(G) service for validation

## Status

✅ **Phase 1-2**: Foundation Complete  
✅ **Phase 4**: All Algorithms Complete  
✅ **Phase 5**: Service Bridges Complete  
⏳ **Phase 6**: Testing (in progress)

**Overall**: ~90% Complete

## File Structure

```
src/
├── combinators.rkt              ✅ Y/Z combinators
├── m-expression.rkt             ✅ M-expression parser
├── s-expression.rkt             ✅ S-expression events
├── datalog-engine.rkt           ✅ Custom Datalog
├── prolog-engine.rkt            ✅ Custom Prolog
├── m-s-compiler.rkt             ✅ M↔S compiler
├── algorithms/
│   ├── algorithm1.rkt          ✅ Binding extraction
│   ├── algorithm2.rkt          ✅ Scope topology
│   ├── algorithm3.rkt           ✅ Čech complex
│   ├── algorithm4.rkt           ✅ Cohomology (H¹)
│   └── unified-pipeline.rkt     ✅ Complete pipeline
├── bridge/
│   ├── haskell-bridge.rkt       ✅ Haskell service client
│   └── racket-bridge.rkt        ✅ Racket service client
└── main.rkt                     ✅ Entry point
```

## Usage Examples

### Compute H¹

```racket
(require "src/algorithms/unified-pipeline.rkt")

(let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
  (printf "H¹ = ~a\n" (pipeline-result-h1 result)))
```

### Compare with Services

```racket
(require "src/bridge/haskell-bridge.rkt")
(require "src/bridge/racket-bridge.rkt")

(when (haskell-service-available?)
  (let-values ([(haskell-h1 error) (call-haskell-h1 source)])
    (printf "Haskell H¹ = ~a\n" haskell-h1)))
```

### Validate Hypothesis

```racket
(validate-hypothesis h1 vg 0 0)
;; Returns: (values match? diff message)
```

## Next Steps

See `IMPLEMENTATION_STATUS.md` for detailed progress.
