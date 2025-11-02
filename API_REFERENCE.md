# API Reference

**Complete reference for Computational Scheme Theory public API**

---

## Overview

The public API provides functions for:
- Computing H¹-H⁴ cohomology from Scheme source
- Validating the hypothesis H¹ = V(G) - k
- Accessing service bridges for cyclomatic complexity

**Module**: `racket-unified/src/api.rkt`

---

## Main Functions

### `compute-h1-from-source`

```racket
(compute-h1-from-source source) → (values h1-cech h1-incidence h2-incidence h3-incidence h4-incidence dep-graph incidence-struct error-msg)
```

Compute H¹-H⁴ cohomology from Scheme source code.

**Parameters:**
- `source` : `(or/c string? list?)` - Scheme program as string or S-expression

**Returns:** Multiple values:
- `h1-cech` : `(or/c number? #f)` - H¹ via Čech complex
- `h1-incidence` : `(or/c number? #f)` - H¹ via incidence structure (recommended)
- `h2-incidence` : `(or/c number? #f)` - H² via incidence structure
- `h3-incidence` : `(or/c number? #f)` - H³ via incidence structure
- `h4-incidence` : `(or/c number? #f)` - H⁴ via incidence structure
- `dep-graph` : `dependency-graph?` - Dependency graph structure
- `incidence-struct` : `incidence-structure?` - Incidence structure
- `error-msg` : `(or/c string? #f)` - Error message or `#f` if successful

**Example:**
```racket
(let-values ([(h1-cech h1-inc h2-inc h3-inc h4-inc dep-graph inc-struct error)
              (compute-h1-from-source "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))")])
  (when (not error)
    (printf "H¹ = ~a\n" h1-inc)
    (printf "H² = ~a\n" h2-inc)))
```

**Errors:**
- Parsing errors return error message in last value
- Invalid Scheme syntax returns error message

---

### `compute-h1-from-source-detailed`

```racket
(compute-h1-from-source-detailed source) → pipeline-result?
```

Compute H¹-H⁴ with detailed statistics.

**Parameters:**
- `source` : `(or/c string? list?)` - Scheme program as string or S-expression

**Returns:**
- `pipeline-result?` - Structure containing all computation results

**Pipeline Result Fields:**
- `pipeline-result-h1-value` / `pipeline-result-h1` : `number?` - H¹ value
- `pipeline-result-h2-value` : `number?` - H² value
- `pipeline-result-h3-value` : `number?` - H³ value
- `pipeline-result-h4-value` : `number?` - H⁴ value
- `pipeline-result-beta0` : `number?` - β₀ (number of connected components)
- `pipeline-result-beta1` : `number?` - β₁ (from graph structure)
- `pipeline-result-num-bindings` : `number?` - Number of bindings found
- `pipeline-result-num-simplices0` : `number?` - Number of 0-simplices
- `pipeline-result-num-simplices1` : `number?` - Number of 1-simplices
- `pipeline-result-num-simplices2` : `number?` - Number of 2-simplices
- `pipeline-result-success` : `boolean?` - Whether computation succeeded
- `pipeline-result-error-message` / `pipeline-result-error` : `(or/c string? #f)` - Error message

**Example:**
```racket
(define result (compute-h1-from-source-detailed 
  "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"))

(when (pipeline-result-success result)
  (printf "H¹ = ~a\n" (pipeline-result-h1 result))
  (printf "H² = ~a\n" (pipeline-result-h2-value result))
  (printf "H³ = ~a\n" (pipeline-result-h3-value result))
  (printf "H⁴ = ~a\n" (pipeline-result-h4-value result))
  (printf "Bindings: ~a\n" (pipeline-result-num-bindings result))
  (printf "0-simplices: ~a\n" (pipeline-result-num-simplices0 result))
  (printf "1-simplices: ~a\n" (pipeline-result-num-simplices1 result))
  (printf "2-simplices: ~a\n" (pipeline-result-num-simplices2 result)))
```

---

## Hypothesis Validation

### `validate-hypothesis`

```racket
(validate-hypothesis h1 vg [k] [tolerance]) → (values valid? diff message)
```

Validate the hypothesis: H¹ = V(G) - k

**Parameters:**
- `h1` : `number?` - H¹ cohomology value
- `vg` : `number?` - V(G) cyclomatic complexity
- `k` : `number?` - Normalization constant (default: 0)
- `tolerance` : `number?` - Allowed tolerance (default: 0)

**Returns:** Three values:
- `valid?` : `boolean?` - Whether hypothesis holds
- `diff` : `number?` - Difference |H¹ - (V(G) - k)|
- `message` : `string?` - Validation message

**Example:**
```racket
(let-values ([(h1 error1) (compute-h1-from-source program)]
             [(vg error2) (call-racket-vg program)])
  (when (and h1 vg (not error1) (not error2))
    (let-values ([(valid? diff msg) (validate-hypothesis h1 vg 0 0)])
      (printf "Hypothesis holds: ~a\n" valid?)
      (printf "Difference: ~a\n" diff)
      (printf "~a\n" msg))))
```

---

## Service Bridges

### `racket-service-available?`

```racket
(racket-service-available?) → boolean?
```

Check if Racket service bridge is available.

**Returns:**
- `boolean?` - `#t` if service is available, `#f` otherwise

**Example:**
```racket
(when (racket-service-available?)
  (printf "Service bridge is ready\n"))
```

---

### `call-racket-vg`

```racket
(call-racket-vg source) → (values vg-value error-msg)
```

Compute V(G) cyclomatic complexity via service bridge.

**Parameters:**
- `source` : `(or/c string? list?)` - Scheme program

**Returns:** Two values:
- `vg-value` : `(or/c number? #f)` - V(G) value or `#f` on error
- `error-msg` : `(or/c string? #f)` - Error message or `#f` if successful

**Example:**
```racket
(when (racket-service-available?)
  (let-values ([(vg error) (call-racket-vg "(lambda (x) (if (> x 0) 1 -1))")])
    (if error
        (printf "Error: ~a\n" error)
        (printf "V(G) = ~a\n" vg))))
```

---

## Configuration

### `*racket-service-url*`

```racket
*racket-service-url* : string?
```

URL for Racket service bridge (if using remote service).

**Default**: Configuration-dependent

**Example:**
```racket
(set! *racket-service-url* "http://localhost:8080")
```

---

## Pipeline Result Accessors

All pipeline result fields have accessor functions:

### Basic Accessors
- `pipeline-result-h1-value` / `pipeline-result-h1` → `number?`
- `pipeline-result-h2-value` → `number?`
- `pipeline-result-h3-value` → `number?`
- `pipeline-result-h4-value` → `number?`

### Statistics Accessors
- `pipeline-result-beta0` → `number?`
- `pipeline-result-beta1` → `number?`
- `pipeline-result-num-bindings` → `number?`
- `pipeline-result-num-simplices0` → `number?`
- `pipeline-result-num-simplices1` → `number?`
- `pipeline-result-num-simplices2` → `number?`

### Status Accessors
- `pipeline-result-success` → `boolean?`
- `pipeline-result-error-message` / `pipeline-result-error` → `(or/c string? #f)`

---

## Error Handling

All functions use Racket's error handling conventions:

1. **Parsing errors**: Return error message in last value
2. **Invalid input**: Return error message
3. **Service unavailable**: Return `#f` or error message

**Best Practice:**
```racket
(let-values ([(h1 error) (compute-h1-from-source source)])
  (if error
      (printf "Error: ~a\n" error)
      (printf "H¹ = ~a\n" h1)))
```

---

## Examples

### Example 1: Simple H¹ Computation

```racket
(require "racket-unified/src/api.rkt")

(define source "(define x 10)")
(let-values ([(h1-cech h1-inc h2-inc h3-inc h4-inc dep-graph inc-struct error)
              (compute-h1-from-source source)])
  (if error
      (printf "Error: ~a\n" error)
      (printf "H¹ = ~a, H² = ~a, H³ = ~a, H⁴ = ~a\n" h1-inc h2-inc h3-inc h4-inc)))
```

### Example 2: Recursive Function Analysis

```racket
(require "racket-unified/src/api.rkt")

(define factorial "(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))")
(define result (compute-h1-from-source-detailed factorial))

(when (pipeline-result-success result)
  (printf "Recursive factorial analysis:\n")
  (printf "  H¹ = ~a\n" (pipeline-result-h1 result))
  (printf "  Bindings: ~a\n" (pipeline-result-num-bindings result))
  (printf "  Simplices: 0D=~a, 1D=~a, 2D=~a\n"
          (pipeline-result-num-simplices0 result)
          (pipeline-result-num-simplices1 result)
          (pipeline-result-num-simplices2 result)))
```

### Example 3: Hypothesis Validation

```racket
(require "racket-unified/src/api.rkt")

(define program "(lambda (x) (if (> x 0) 1 -1))")

;; Compute H¹
(let-values ([(h1 error1) (compute-h1-from-source program)])
  (when (and h1 (not error1))
    ;; Compute V(G) if service available
    (if (racket-service-available?)
        (let-values ([(vg error2) (call-racket-vg program)])
          (when (and vg (not error2))
            (let-values ([(valid? diff msg) (validate-hypothesis h1 vg 0 0)])
              (printf "H¹ = ~a, V(G) = ~a\n" h1 vg)
              (printf "Hypothesis holds: ~a (diff: ~a)\n" valid? diff)
              (printf "~a\n" msg))))
        (printf "Service not available\n"))))
```

---

## Type Definitions

### `pipeline-result?`

```racket
(pipeline-result? value) → boolean?
```

Predicate for pipeline result structures.

### `dependency-graph?`

```racket
(dependency-graph? value) → boolean?
```

Predicate for dependency graph structures.

### `incidence-structure?`

```racket
(incidence-structure? value) → boolean?
```

Predicate for incidence structure.

---

## Performance Notes

- **Parsing**: O(n) where n is program size
- **H¹ computation**: O(m²) where m is number of bindings
- **Service calls**: Network latency if using remote service

**Recommendation**: Use `compute-h1-from-source-detailed` for detailed analysis, `compute-h1-from-source` for quick results.

---

## See Also

- [User Guide](docs/USING_DIMENSIONAL_FRAMEWORK.md) - Usage examples
- [System Complete](SYSTEM_COMPLETE.md) - System overview
- [H¹ Computation Guide](docs/00%20-%20INBOX/update/files%20(4)/H1_COMPUTATION_GUIDE.md) - Mathematical details

---

*Last Updated: 2025-01-31*

