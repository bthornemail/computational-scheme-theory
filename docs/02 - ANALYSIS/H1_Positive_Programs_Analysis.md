# Analysis of Programs with H¹ > 0

**Date**: 2025-01-31  
**Purpose**: Document binding patterns from the 3 outlier programs that generate H¹=1 to understand boundary conditions for non-trivial topology.

---

## Programs Analyzed

1. `baseline/B002.scm` - H¹=1, V(G)=32
2. `debug/cycle-test-2.scm` - H¹=1, V(G)=16  
3. `functional/F009.scm` - H¹=1, V(G)=4

---

## Program 1: baseline/B002.scm

### Source Code
```scheme
(define (compute x y)
  (let ((a (+ x y))
        (b (* x y)))
    (/ a b)))
```

### Structural Analysis

**Bindings**:
- `compute` (top-level define)
- `a` (let binding)
- `b` (let binding)
- `x`, `y` (parameters)

**Scope Structure**:
- Global scope: contains `compute`
- Function scope: contains `x`, `y` parameters
- Let scope: contains `a`, `b` bindings

**Visibility Regions**:
- `compute` is visible globally
- `a` and `b` are both visible in the let body, creating scope overlap
- Both `a` and `b` reference `x` and `y` from outer scope

**H¹=1 Mechanism**:
The key pattern here is **multiple bindings in the same let block** that:
1. Both reference bindings from outer scope (closure capture)
2. Are visible in overlapping regions (same let body)
3. Create a non-trivial intersection in the Čech complex

The scope overlap between the visibility regions of `a`, `b`, and the outer scope (`x`, `y`) forms a cycle when the triple intersection is empty but pairwise intersections exist.

---

## Program 2: debug/cycle-test-2.scm

### Source Code
```scheme
;; Test program with letrec (mutually recursive bindings should create cycles)
(define (test-letrec)
  (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
    (even? 5)))
```

### Structural Analysis

**Bindings**:
- `test-letrec` (top-level define)
- `even?` (letrec binding)
- `odd?` (letrec binding)
- `n` (lambda parameters in each function)

**Scope Structure**:
- Global scope: contains `test-letrec`
- Function scope: contains `even?` and `odd?`
- Lambda scopes: each contains parameter `n`

**Key Feature - Mutual Recursion**:
This is the classic `letrec` pattern that enables mutual recursion:
- `even?` calls `odd?`
- `odd?` calls `even?`
- Both are defined in the same `letrec` block

**H¹=1 Mechanism**:
The `letrec` construct creates a **binding cycle**:
1. `even?` and `odd?` are defined simultaneously
2. Both reference each other (mutual recursion)
3. This creates a cycle in the binding dependency graph
4. The scope overlap between these mutually recursive bindings generates a 1-cycle in the Čech complex

This is the most direct example of how **mutual recursion via letrec** creates topological cycles.

---

## Program 3: functional/F009.scm

### Source Code
```scheme
(define (flip f)
  (lambda (x y) (f y x)))
```

### Structural Analysis

**Bindings**:
- `flip` (top-level define)
- `f` (parameter)
- `x`, `y` (lambda parameters)

**Scope Structure**:
- Global scope: contains `flip`
- Function scope: contains `f`
- Lambda scope: contains `x`, `y` and captures `f`

**Key Feature - Higher-Order Function**:
This is a higher-order function that:
- Takes a function `f` as parameter
- Returns a lambda that captures `f` from outer scope
- The returned lambda reverses the argument order

**H¹=1 Mechanism**:
The closure capture creates scope overlap:
1. The inner lambda captures `f` from outer scope (closure)
2. The visibility region of `f` (parameter) overlaps with the visibility region of the lambda
3. The lambda's own parameters (`x`, `y`) create additional scope intersections
4. This nested closure structure creates a non-trivial cycle when the Čech complex is constructed

---

## Common Patterns Identified

### Pattern 1: Multiple Let Bindings
**Examples**: B002.scm
- Multiple bindings in same `let` block
- All reference outer scope variables
- Creates overlapping visibility regions

### Pattern 2: Mutual Recursion (letrec)
**Examples**: cycle-test-2.scm
- Two or more functions defined in `letrec`
- Functions reference each other recursively
- Creates explicit binding cycles

### Pattern 3: Closure Capture
**Examples**: F009.scm, B002.scm (partially)
- Inner function/lambda captures bindings from outer scope
- Creates nested scope structure
- Generates non-trivial scope overlaps

---

## Requirements for H¹ > 0

Based on the analysis, programs generate H¹ > 0 when they contain:

1. **Binding Cycles**: At least one cycle in the static binding dependency graph
   - Mutual recursion (`letrec`) is the most direct mechanism
   - Closure capture can also create cycles

2. **Scope Overlaps**: Three or more bindings with:
   - Pairwise non-empty intersections
   - Empty triple intersection (or non-trivial triple relationship)

3. **Complex Scope Nesting**: Nested scopes where:
   - Inner scopes capture outer bindings
   - Multiple bindings share visibility regions
   - Scope boundaries create non-contractible topological structures

---

## Boundary Condition Summary

For a program to yield H¹ ≥ 1 under the current algorithm:

1. **Minimum Requirement**: Three scope regions with pairwise overlaps but non-trivial triple intersection
2. **Structural Patterns**:
   - `letrec` with mutual recursion (most reliable)
   - `let` with multiple bindings + closure captures
   - Higher-order functions with nested lambdas
3. **Topological Structure**: The Čech complex must contain at least one 1-simplex (edge) that forms part of a cycle, where the cycle cannot be "filled in" by 2-simplices (triangles)

---

## Implications for Test Case Generation

To systematically generate programs with H¹ > 0:

1. **T1 (H¹=1)**: Single `letrec` block with 2 mutually recursive functions
2. **T2 (H¹≥2)**: Multiple independent `letrec` blocks or nested structures creating independent cycles
3. **T3 (Complex)**: Deep closure nesting with three+ bindings having pairwise but not triple overlaps

This analysis confirms that **mutual recursion via letrec** is the most reliable pattern for generating known binding cycles that should yield H¹ > 0.

---

**Status**: Analysis complete. Patterns identified for Phase 2 test case generation.

