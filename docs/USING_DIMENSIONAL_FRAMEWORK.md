# Using the Dimensional Framework

**Guide**: How to use Pattern Matching → Church Numerals → Dimensions → H¹

---

## Quick Start

### Compute H¹ with Dimensional Analysis

```racket
(require "racket-unified/src/algorithms/unified-pipeline.rkt")

(define program "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))")
(define result (compute-h1-from-source-detailed program))

(if (pipeline-result-success result)
    (begin
      (printf "H¹ = ~a\n" (pipeline-result-h1 result))
      (printf "H² = ~a\n" (pipeline-result-h2-value result))
      (printf "H³ = ~a\n" (pipeline-result-h3-value result))
      (printf "H⁴ = ~a\n" (pipeline-result-h4-value result)))
    (printf "Error: ~a\n" (pipeline-result-error-message result)))
```

---

## Understanding Dimensional Output

### Access Count = Dimension = Church Numeral

**0D (Church 0):**
- Binding never accessed
- Pattern: `()`
- Polynomial: degree 0 (constant)
- Topology: Affine point

**1D (Church 1):**
- Binding accessed once
- Pattern: `(P)`
- Polynomial: degree 1 (linear)
- Topology: Projective line
- **Creates cycles if recursive**

**nD (Church n):**
- Binding accessed n times
- Pattern: `(P ...)` with n repetitions
- Polynomial: degree n
- Topology: nD manifold

---

## Pattern Matching Interpretation

### Detecting Ellipsis Presence

**Ellipsis `...` Active (H¹ > 0):**
- Recursive patterns detected
- Variable repetition present
- Cycle in incidence structure
- Dimension ≥ 1

**Ellipsis Absent (H¹ = 0):**
- Fixed pattern
- No variable repetition
- No cycles
- Dimension = 0

---

## Examples

### Example 1: No Cycles (0D)

```scheme
(define x 10)
```

**Result:**
- H¹ = 0
- x: 0D (Church 0, never accessed)
- Pattern: `()` - no ellipsis

### Example 2: Recursive (1D+)

```scheme
(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
```

**Result:**
- H¹ = 1 ✓
- factorial: 1D (Church 1, accessed in own body)
- Pattern: `(P ...)` - ellipsis active
- Cycle: factorial → recursion → factorial

### Example 3: Multiple Accesses

```scheme
(define (add x y) (+ x y))
(define a (add 1 2))
(define b (add a 3))
```

**Result:**
- add: dimension = number of calls
- Each call increases dimension (Church successor)

---

## Running the Demonstration

```bash
cd racket-unified
racket demo-dimensional-framework.rkt
```

This shows:
- Dimensional breakdown for each program
- Pattern matching interpretation
- Church numeral encoding
- Polynomial degree
- Topology classification

---

## The Complete Framework

```
Pattern Matching  ≅  Polynomial  ≅  Church Numerals  ≅  Dimensions

(P ...)           =  P^n          =  λf. λx. f^n x    =  Dimension n
```

**The ellipsis `...` is the literal symbol encoding all of these.**

---

## Status

✅ **Fully Operational**

The dimensional framework is integrated, tested, and ready for use.

