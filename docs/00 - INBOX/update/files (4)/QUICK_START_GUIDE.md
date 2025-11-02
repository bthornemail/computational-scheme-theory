# Quick Start Guide: Using the Dimensional Framework

**Date**: 2025-01-31  
**Status**: ✅ **Ready to Use**

---

## Overview

This guide shows you how to use the complete dimensional framework for computing H¹ cohomology from Scheme programs.

---

## Python Pipeline

### Basic Usage

```python
from scheme_h1_pipeline import compute_h1_from_scheme

# Simple program
scheme = """
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
"""

# Compute H¹
h1 = compute_h1_from_scheme(scheme, verbose=True)
print(f"H¹ = {h1}")
```

### What Happens

1. **Parsing**: Scheme → M-expression AST
2. **Access Counting**: Counts variable references (Church numerals)
3. **Datalog Generation**: Creates logic facts
4. **Incidence Structure**: Builds bipartite graph (points ↔ hyperplanes)
5. **H¹ Computation**: Computes first cohomology with dimensional weighting

### Accessing Dimensional Information

```python
from scheme_h1_pipeline import parse_scheme, DatalogGenerator, build_incidence_from_datalog

# Parse and generate
ast = parse_scheme("(define x 10)\n(define y (+ x x))")
generator = DatalogGenerator()
generator.generate(ast)

# Access counting
print(f"x accessed {generator.access_map.get('x', 0)} times")

# Build incidence structure with dimensions
structure = build_incidence_from_datalog(generator)

# Check point dimensions
for point in structure.points:
    print(f"{point.name}: dimension={point.dimension}, access_count={point.access_count}")
```

---

## Racket Implementation

### Using the Incidence Structure

```racket
(require "algorithms/incidence-structure.rkt")

;; Build incidence structure from AST
(define incidence-struct
  (build-incidence-structure ast bindings scope-map scope-tree))

;; Compute H¹
(define h1 (compute-h1-incidence incidence-struct))
(printf "H¹ = ~a\n" h1)

;; Export to polynomial representation
(define poly-ring (incidence-structure->polynomial-ring incidence-struct))
;; Returns: list of (binding-id . degree) pairs
;; Example: (('x . 2) ('y . 1)) means x²·y
```

### Pattern Dimension Detection

The system automatically detects pattern dimensions:
- `()` → 0D
- `(P)` → 1D
- `(P ...)` → nD (where n = length)
- `#(P ...)` → 2nD (vectors are multivariate)

### Polynomial Export

```racket
;; Convert single binding to polynomial
(define point (hash-ref points 'factorial))
(define poly (binding->polynomial point))
;; Returns: ('factorial . dimension)

;; Export entire structure
(define all-polys (incidence-structure->polynomial-ring incidence-struct))
;; Use for: factorization, zero locus computation, etc.
```

---

## Understanding the Results

### H¹ = 0
- No cycles in the program
- Linear control flow
- Tree-like binding structure

### H¹ > 0
- Cycles detected (recursion, loops)
- Complex binding dependencies
- Projective points creating cycles through infinity

### Dimensional Information

- **Dimension** = max(access count, pattern dimension)
- **Access count** = Church numeral = number of times accessed
- **Pattern dimension** = structural depth (ellipsis patterns)

---

## Examples

### Example 1: Simple Binding (H¹ = 0)

```scheme
(define x 10)
```

**Result**: H¹ = 0 (no cycles)

### Example 2: Recursive Function (H¹ > 0)

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

**Result**: H¹ > 0 (recursive cycle detected)

### Example 3: Projective Point

```scheme
(define (safe-divide x y)
  (if (= y 0)
      'undefined      ; ← Projective point at infinity
      (/ x y)))
```

**Result**: H¹ >= 0 (cycle through projective closure)

---

## Integration Tests

Run the test suite:

```bash
cd "docs/00 - INBOX/update/files (4)"
pip install numpy  # If not already installed
python test_scheme_h1_integration.py
```

---

## Dependencies

**Python**:
- `numpy` (for matrix operations in H¹ computation)

**Racket**:
- All dependencies included in `racket-unified/src/`

---

## Next Steps

1. **Read**: `IMPLEMENTATION_STATUS_REPORT.md` for detailed findings
2. **Review**: `CODE_RECOMMENDATIONS_SUMMARY.md` for enhancement suggestions
3. **Explore**: Theoretical docs for advanced concepts (Lie algebras, etc.)

---

## Troubleshooting

### Import Errors
- Ensure `numpy` is installed: `pip install numpy`
- Check that all Python files are in the same directory

### H¹ Always Zero
- Check if program has recursion or cycles
- Verify dimensional tracking is enabled
- Use `verbose=True` to see detailed computation

### Pattern Dimension Not Detected
- Ensure AST is properly parsed
- Check binding forms are extracted
- Pattern dimension combines with access count (uses `max`)

---

**The framework is complete and ready to use!**

