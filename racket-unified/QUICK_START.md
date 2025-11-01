# Quick Start Guide

## Installation

No installation needed! Just run with Racket:

```bash
cd racket-unified
racket src/main.rkt
```

## Basic Usage

### Compute H¹ from Scheme Source

```racket
(require "src/algorithms/unified-pipeline.rkt")

(compute-h1-from-source-detailed "(lambda (x) x)")
```

### Run Complete Demo

```bash
racket src/main.rkt
```

Shows:
- M/S-expression pipeline
- H¹ computation for test cases
- Service bridge integration (if services available)

### Run Validation Suite

```bash
racket test/validation-suite.rkt
```

Tests hypothesis: H¹ = V(G) - k

## File Overview

- `src/main.rkt` - Complete integrated demo
- `src/validation-demo.rkt` - Service comparison demo
- `src/algorithms/unified-pipeline.rkt` - H¹ computation pipeline
- `src/bridge/*.rkt` - Service bridges (Racket V(G) service)

## Configuration

Services are optional. The system works in pure Lisp mode if services are unavailable.

To use the optional Racket V(G) service for hypothesis validation, ensure:
- Racket V(G) service running on `localhost:8081`

## Example Output

```
━━ Test: Simple lambda ━━
Source: (lambda (x) x)
  ✓ Success: H¹ = 0
  ✓ Bindings: 1
  ✓ Simplices: 0:1, 1:0, 2:0
```

## Next Steps

1. Run the demo: `racket src/main.rkt`
2. Check results against your expectations
3. Run validation: `racket test/validation-suite.rkt`
4. Extend test cases as needed

The system is ready to use! 🚀

