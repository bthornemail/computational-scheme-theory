# Computing H¹ Using Incidence Structure

Let me show you how to compute H¹ (first cohomology group) using the bipartite incidence structure from projective duality.

## Overview

The key insight: **H¹ measures "holes" or "cycles" in the incidence structure between points (bindings) and hyperplanes (constraints).**

## Step 1: Build the Incidence Structure

### Example Program

```scheme
(define (process x y)           ; Bindings: x, y
  (let ((z (+ x y)))           ; Binding: z (depends on x, y)
    (if (> z 0)                ; Use: z
        (let ((w (* z 2)))     ; Binding: w (depends on z)
          (display w))         ; Use: w
        (display "negative"))))
```

### Extract Points and Hyperplanes

**Points (Bindings - Affine & Projective):**
```
P₀: global scope (entry point)
P₁: binding x (parameter, affine)
P₂: binding y (parameter, affine)
P₃: binding z (let, affine)
P₄: binding w (let, affine)
P₅: exit point (optional, projective - may not reach)
```

**Hyperplanes (Constraints):**
```
H₀: "parameters x, y must be bound"
H₁: "z depends on x, y being in scope"
H₂: "if test needs z in scope"
H₃: "w depends on z being in scope"
H₄: "display needs w in scope"
H₅: "all paths must converge" (projective constraint)
```

### Incidence Matrix

**Point P_i lies on Hyperplane H_j:**

```
       H₀  H₁  H₂  H₃  H₄  H₅
    ┌─────────────────────────┐
P₀  │  1   0   0   0   0   1  │
P₁  │  1   1   0   0   0   0  │
P₂  │  1   1   0   0   0   0  │
P₃  │  0   1   1   1   0   0  │
P₄  │  0   0   0   1   1   0  │
P₅  │  0   0   0   0   1   1  │
    └─────────────────────────┘

(1 = point lies on hyperplane, 0 = doesn't)
```

## Step 2: Build the Chain Complex

### The Čech Complex

The incidence structure gives us a **chain complex**:

```
C₀ ←d₀─ C₁ ←d₁─ C₂ ←d₂─ ...

where:
C₀ = Free abelian group on points (0-cells)
C₁ = Free abelian group on edges (1-cells)
C₂ = Free abelian group on faces (2-cells)
```

### C₀: 0-Chains (Points)

```
C₀ = ℤ⟨P₀, P₁, P₂, P₃, P₄, P₅⟩

Elements: formal sums like:
  2P₀ - 3P₁ + P₃
  (integer coefficients)
```

### C₁: 1-Chains (Edges in Incidence Graph)

**Edges connect points to hyperplanes:**

```
C₁ = ℤ⟨e₀₀, e₁₀, e₂₀, e₁₁, e₂₁, e₃₁, e₃₂, e₃₃, e₄₃, e₄₄, e₀₅, e₅₅⟩

where eᵢⱼ = edge from Pᵢ to Hⱼ

Examples:
e₁₀: P₁ lies on H₀ (x satisfies parameter constraint)
e₃₁: P₃ lies on H₁ (z satisfies dependency constraint)
```

### C₂: 2-Chains (Cycles)

```
C₂ = ℤ⟨triangles, squares in incidence graph⟩

Example triangle:
  P₁ → H₁ → P₃ → H₂ → P₁
```

## Step 3: Define Boundary Maps

### Boundary Map d₁: C₁ → C₀

**d₁(edge) = endpoint - startpoint**

```
d₁(eᵢⱼ) = Pᵢ - Hⱼ  (oriented edge)

Examples:
d₁(e₁₀) = P₁ - H₀
d₁(e₃₁) = P₃ - H₁
```

**Matrix representation:**

```
d₁ = [∂eᵢⱼ/∂Pₖ] = incidence matrix

       e₀₀ e₁₀ e₂₀ e₁₁ e₂₁ e₃₁ e₃₂ e₃₃ e₄₃ e₄₄ e₀₅ e₅₅
    ┌─────────────────────────────────────────────────┐
P₀  │  1   0   0   0   0   0   0   0   0   0   1   0 │
P₁  │  0   1   0   1   0   0   0   0   0   0   0   0 │
P₂  │  0   0   1   0   1   0   0   0   0   0   0   0 │
P₃  │  0   0   0   0   0   1   1   1   0   0   0   0 │
P₄  │  0   0   0   0   0   0   0   0   1   1   0   0 │
P₅  │  0   0   0   0   0   0   0   0   0   0   0   1 │
H₀  │ -1  -1  -1   0   0   0   0   0   0   0   0   0 │
H₁  │  0   0   0  -1  -1  -1   0   0   0   0   0   0 │
H₂  │  0   0   0   0   0   0  -1   0   0   0   0   0 │
H₃  │  0   0   0   0   0   0   0  -1  -1   0   0   0 │
H₄  │  0   0   0   0   0   0   0   0   0  -1   0   0 │
H₅  │  0   0   0   0   0   0   0   0   0   0  -1  -1 │
    └─────────────────────────────────────────────────┘
```

### Boundary Map d₂: C₂ → C₁

**d₂(face) = sum of edges around face**

```
Example: Square face P₁ → H₁ → P₃ → H₂ → P₁

d₂(square) = e₁₁ + (edge H₁→P₃) - e₃₂ - (edge P₁→H₂)
```

## Step 4: Compute Homology Groups

### Homology Formula

```
Hⁿ = Ker(dⁿ) / Im(dⁿ₊₁)

H¹ = Ker(d₁) / Im(d₂)
```

**Interpretation:**
- **Ker(d₁)**: 1-cycles (closed loops: d₁(cycle) = 0)
- **Im(d₂)**: Boundaries (cycles that bound 2-faces)
- **H¹**: "True holes" = cycles that don't bound faces

### Computing Ker(d₁): 1-Cycles

**Find all linear combinations of edges with boundary 0:**

```
Solve: d₁(Σ aᵢeᵢ) = 0

Example cycle:
P₁ →(e₁₁)→ H₁ →(edge)→ P₃ →(e₃₂)→ H₂ →(edge)→ P₁

Verify:
d₁(e₁₁ + e₃₁⁻¹ + e₃₂ + back_edge) 
= (P₁ - H₁) + (H₁ - P₃) + (P₃ - H₂) + (H₂ - P₁)
= 0 ✓
```

### Computing Im(d₂): Boundaries

**Find which cycles are boundaries of 2-faces:**

```
A cycle is a boundary if it can be filled in.

Example: Triangle P₁ → H₁ → P₃ → P₁
This IS a boundary (it bounds a 2-face)
```

### Computing H¹

```
H¹ = Ker(d₁) / Im(d₂)
   = {1-cycles} / {boundaries}
   = "cycles that are NOT boundaries"
```

## Step 5: Example Calculation

### Simple Case: Linear Program (H¹ = 0)

```scheme
(define (linear x)
  (let ((y (+ x 1)))
    (display y)))
```

**Incidence Structure:**
```
Points: P₀(entry), P₁(x), P₂(y), P₃(exit)
Hyperplanes: H₀(params), H₁(y depends on x)

Edges:
P₀ → H₀
P₁ → H₀, P₁ → H₁
P₂ → H₁
```

**Chain complex:**
```
0 ← C₀ ←d₁─ C₁ ←d₂─ C₂ ← 0
    ↑        ↑        ↑
    4        5        0
   points   edges    faces
```

**No cycles:** Every edge is part of a tree structure.

```
Ker(d₁) = {0} (only trivial cycle)
H¹ = {0} / {0} = 0
```

### Complex Case: Cycle (H¹ > 0)

```scheme
(define (cyclic x)
  (let ((y (+ x 1)))
    (if (> y 0)
        (cyclic y)      ; Recursive call - creates cycle!
        (display y))))
```

**Incidence Structure:**
```
Points: P₀(entry), P₁(x), P₂(y), P₃(cyclic-call), P₄(exit)
Hyperplanes: H₀(params), H₁(y deps), H₂(recursive constraint)

Key: P₃ lies on BOTH H₂ and H₀ (recursive call re-enters)
```

**Cycle detected:**
```
P₁ →(e₁₀)→ H₀ ←(e₀₃)─ P₃ →(e₃₂)→ H₂ →(e₂₁)→ P₂ →(e₂₁)→ H₁ →(back)→ P₁

This cycle does NOT bound a face!
```

**Result:**
```
Ker(d₁) has non-trivial elements
Im(d₂) doesn't contain this cycle
H¹ ≠ 0 ✓
```

### Projective Case: Optional Binding (H¹ > 0)

```scheme
(define (safe-divide x y)
  (if (zero? y)
      'undefined      ; Projective point at infinity!
      (let ((z (/ x y)))
        (display z))))
```

**Incidence Structure WITH projective types:**
```
Points:
P₀(entry), P₁(x), P₂(y), P₃(z), P₄(exit-success), P₅(exit-fail, ∞)

Hyperplanes:
H₀(params), H₁(test y), H₂(z deps), H₃(divergent paths), H₄(projective closure)

Key: H₄ is the PROJECTIVE constraint that identifies paths at infinity
```

**New cycle through infinity:**
```
P₂ →(e₂₁)→ H₁ →(branch_fail)→ P₅(∞) →(e₅₄)→ H₄ →(closure)→ P₄ →(e₄₃)→ H₃ →(back)→ P₂

This cycle exists ONLY because of projective completion!
Without P₅(∞), this cycle doesn't close.
```

**Result:**
```
Affine analysis: Misses P₅, cycle incomplete, H¹ = 0
Projective analysis: Includes P₅, cycle complete, H¹ > 0 ✓
```

## Step 6: Algorithm Implementation

### Pseudocode

```typescript
function computeH1(program: Program): number {
  // Step 1: Extract incidence structure
  const points = extractBindings(program)  // Affine + Projective
  const hyperplanes = extractConstraints(program)
  const incidence = buildIncidenceMatrix(points, hyperplanes)
  
  // Step 2: Build chain complex
  const C0 = points.concat(hyperplanes)  // 0-cells
  const C1 = buildEdges(incidence)       // 1-cells from incidence
  const C2 = buildFaces(C1)              // 2-cells from edge combinations
  
  // Step 3: Compute boundary maps
  const d1 = computeBoundaryMap(C1, C0)  // C₁ → C₀
  const d2 = computeBoundaryMap(C2, C1)  // C₂ → C₁
  
  // Step 4: Compute homology
  const ker_d1 = kernel(d1)              // Null space of d₁
  const im_d2 = image(d2)                // Column space of d₂
  const H1 = quotient(ker_d1, im_d2)     // Ker / Im
  
  return rank(H1)  // Dimension of H¹
}

function extractBindings(program: Program): Point[] {
  const affine = program.bindings.filter(b => b.required)
  const projective = program.bindings.filter(b => b.optional)
  
  // Add projective points at infinity for partial functions
  const infinity_points = program.functions
    .filter(f => f.partial)
    .map(f => ({ type: 'infinity', source: f }))
  
  return affine.concat(projective).concat(infinity_points)
}

function extractConstraints(program: Program): Hyperplane[] {
  const dependencies = analyzeDependencies(program)
  const scoping = analyzeScopeRules(program)
  const typing = analyzeTypeConstraints(program)
  
  // Add projective constraints for convergence
  const projective_constraints = program.branches
    .filter(b => b.optional_paths)
    .map(b => ({ type: 'projective_closure', branch: b }))
  
  return dependencies.concat(scoping, typing, projective_constraints)
}
```

### Matrix Computation

```typescript
function kernel(matrix: Matrix): VectorSpace {
  // Find null space: solve Ax = 0
  const rref = rowReducedEchelonForm(matrix)
  const free_vars = identifyFreeVariables(rref)
  const basis = constructBasisVectors(free_vars)
  return new VectorSpace(basis)
}

function image(matrix: Matrix): VectorSpace {
  // Find column space: span of columns
  const pivot_cols = identifyPivotColumns(matrix)
  const basis = matrix.columns.filter((_, i) => pivot_cols.includes(i))
  return new VectorSpace(basis)
}

function quotient(V: VectorSpace, W: VectorSpace): VectorSpace {
  // Compute V / W
  // Choose representatives from V not in W
  const V_basis = V.basis
  const W_basis = W.basis
  const quotient_basis = V_basis.filter(v => !inSpan(v, W_basis))
  return new VectorSpace(quotient_basis)
}

function rank(V: VectorSpace): number {
  return V.basis.length  // Dimension
}
```

## Step 7: Comparison - Affine vs Projective

### Same Program, Different Analysis

**Program:**
```scheme
(define (process x y)
  (if (zero? y)
      'undefined
      (/ x y)))
```

### Affine Analysis (Current)

**Points:** P₀, P₁(x), P₂(y), P₃(result)
**Hyperplanes:** H₀(params), H₁(division)

**Incidence:**
```
     H₀  H₁
P₀ │ 1   0 │
P₁ │ 1   1 │
P₂ │ 1   1 │
P₃ │ 0   1 │
```

**Ker(d₁):** Only trivial cycle
**H¹ = 0** ❌ (misses the structure!)

### Projective Analysis (Proposed)

**Points:** P₀, P₁(x), P₂(y), P₃(result), **P₄(∞, undefined)**
**Hyperplanes:** H₀(params), H₁(division), **H₂(projective closure)**

**Incidence:**
```
     H₀  H₁  H₂
P₀ │ 1   0   1 │
P₁ │ 1   1   0 │
P₂ │ 1   1   1 │
P₃ │ 0   1   0 │
P₄ │ 0   0   1 │  ← Projective point!
```

**Cycle detected:**
```
P₂ → H₁ →(fail branch)→ P₄ → H₂ →(closure)→ P₀ →(re-enter)→ H₀ → P₂
```

**Ker(d₁):** Non-trivial cycle through P₄(∞)
**H¹ > 0** ✓ (detects the optional path!)

## Step 8: Complete Example with Computation

Let me create a concrete numerical example:

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```Perfect! The code demonstrates the key insight. Now let me create a comprehensive visual explanation document:Now let me move these files to the outputs directory for you:Perfect! Here's what I've created for you:

## Summary

I've shown you **how to compute H¹ using incidence structure** with two comprehensive resources:

### 📊 [Python Implementation](computer:///mnt/user-data/outputs/h1_incidence_computation.py)
A working implementation that demonstrates:
- Building incidence structures from programs
- Computing H¹ via kernel/image calculation
- **Three concrete examples** showing H¹ values
- Direct comparison of affine vs projective analysis

**Key Result:** The projective example detected a **cycle through the infinity point** that creates H¹ = 1, while linear programs show H¹ = 0.

### 📖 [Complete Guide](computer:///mnt/user-data/outputs/H1_COMPUTATION_GUIDE.md)
A comprehensive explanation covering:
- All 8 steps of the algorithm
- Mathematical foundations
- Visual diagrams of the bipartite structure
- Complete worked example (safe-divide)
- Implementation checklist

## The Key Insight

**H¹ counts cycles in a bipartite graph:**

```
POINTS (bindings)     HYPERPLANES (constraints)
  P₃(test) ────────────→ H₁(test_y)
     ↑                        ↓
     │                     P₅(∞) ← PROJECTIVE!
     │                        ↓
  H₂(division) ←─────── H₃(closure)
     ↑                        ↓
     │                     P₄(result)
     └────────────────────────┘
```

This cycle **only exists** because of the projective point P₅(∞) representing the `'undefined` case!

## Why This Matters for Your Research

Your research questions ask:
> "Why is H¹ mostly zero?"
> "Only 3 programs have H¹ > 0?"

**Answer:** Current analysis is affine-only and misses:
- Optional bindings → projective points
- Undefined/failure paths → points at infinity  
- Closure constraints → projective hyperplanes

By adding projective types, you should see **H¹ increase** because you'll detect cycles through these previously-ignored infinity points.

Would you like me to:
1. Create visualizations of the incidence structure?
2. Extend the implementation to handle your actual Scheme programs?
3. Design the integration with Computational Scheme Theory?
4. Something else?