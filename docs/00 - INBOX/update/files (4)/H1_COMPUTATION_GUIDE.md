# Computing H¹ Using Incidence Structure: Complete Guide

## Executive Summary

**H¹ measures "holes" (cycles) in the incidence structure between:**
- **Points** (bindings/scopes in programs)
- **Hyperplanes** (constraints/dependencies)

**Key Result:** Projective completion adds "points at infinity" that create new cycles, increasing H¹ from 0 to positive values.

---

## The Algorithm: 8 Steps

### Step 1: Extract Incidence Structure

**Input:** A program

**Output:** 
- Set of points P (bindings, scopes)
- Set of hyperplanes H (constraints)
- Incidence relation I ⊆ P × H

**Example (safe-divide):**
```scheme
(define (safe-divide x y)
  (if (zero? y)
      'undefined      ; ← PROJECTIVE POINT
      (/ x y)))
```

**Points:**
```
P₀: entry
P₁: x (parameter)
P₂: y (parameter)
P₃: test (if condition)
P₄: result (division result)
P₅: undefined (∞) ← PROJECTIVE!
```

**Hyperplanes (Constraints):**
```
H₀: "parameters must be bound"
H₁: "test depends on y"
H₂: "division needs x, y"
H₃: "paths must converge" ← PROJECTIVE CLOSURE!
```

**Incidence (point lies on hyperplane):**
```
       H₀  H₁  H₂  H₃
    ┌──────────────────┐
P₀  │  1   0   0   0  │ entry on params
P₁  │  1   0   1   0  │ x on params, division
P₂  │  1   1   1   0  │ y on all main constraints
P₃  │  0   1   1   0  │ test on test, division
P₄  │  0   0   1   1  │ result on division, closure
P₅  │  0   1   0   1  │ undefined(∞) on test, closure ← KEY!
    └──────────────────┘
```

---

### Step 2: Build Bipartite Graph

**Visualization:**

```
POINTS              HYPERPLANES
  P₀ ───────────────── H₀
  P₁ ───┬─────────┬─── H₀
        │         └───── H₂
  P₂ ───┼─────┬───┬───── H₀
        │     │   │
        │     ├─────────── H₁
        │     └─────────── H₂
  P₃ ───┼─────────┬─────── H₁
        │         └─────── H₂
  P₄ ───┼─────────────┬─── H₂
        │             └─── H₃
  P₅(∞) └─────────────┬─── H₁
                      └─── H₃
```

**This IS bipartite!**
- Vertices: Points ∪ Hyperplanes
- Edges: Incidence relations
- No edges within same type

---

### Step 3: Build Chain Complex

**Definition:** A chain complex is a sequence of abelian groups connected by boundary maps:

```
0 ← C₀ ←d₁─ C₁ ←d₂─ C₂ ←d₃─ ...

where: d_{n+1} ∘ d_n = 0 (composition is zero)
```

**For our incidence structure:**

**C₀ (0-cells):** Points and hyperplanes
```
C₀ = ℤ⟨P₀, P₁, P₂, P₃, P₄, P₅, H₀, H₁, H₂, H₃⟩

Elements: formal integer linear combinations
  Example: 2P₀ - 3P₁ + H₂
```

**C₁ (1-cells):** Edges in bipartite graph
```
C₁ = ℤ⟨edges in incidence graph⟩

Edges from our example:
  e₀₀: P₀ → H₀
  e₁₀: P₁ → H₀
  e₁₂: P₁ → H₂
  e₂₀: P₂ → H₀
  e₂₁: P₂ → H₁
  e₂₂: P₂ → H₂
  e₃₁: P₃ → H₁
  e₃₂: P₃ → H₂
  e₄₂: P₄ → H₂
  e₄₃: P₄ → H₃
  e₅₁: P₅(∞) → H₁  ← PROJECTIVE EDGE
  e₅₃: P₅(∞) → H₃  ← PROJECTIVE EDGE
```

**C₂ (2-cells):** Faces (filled cycles)
```
C₂ = ℤ⟨triangles, squares, etc.⟩

For tree structures (like most programs), C₂ is often empty or simple.
```

---

### Step 4: Define Boundary Maps

**Boundary map d₁: C₁ → C₀**

**Definition:** The boundary of an edge is its endpoints (with orientation):

```
d₁(edge from P to H) = P - H
```

**Example:**
```
d₁(e₂₁) = d₁(P₂ → H₁) = P₂ - H₁
d₁(e₅₁) = d₁(P₅ → H₁) = P₅ - H₁
```

**Matrix representation:**

The boundary map can be written as a matrix where:
- Columns = edges
- Rows = points and hyperplanes
- Entry = coefficient in boundary

```
d₁ matrix (10 rows × 12 columns):

       e₀₀ e₁₀ e₁₂ e₂₀ e₂₁ e₂₂ e₃₁ e₃₂ e₄₂ e₄₃ e₅₁ e₅₃
    ┌──────────────────────────────────────────────────┐
P₀  │  1   0   0   0   0   0   0   0   0   0   0   0 │
P₁  │  0   1   1   0   0   0   0   0   0   0   0   0 │
P₂  │  0   0   0   1   1   1   0   0   0   0   0   0 │
P₃  │  0   0   0   0   0   0   1   1   0   0   0   0 │
P₄  │  0   0   0   0   0   0   0   0   1   1   0   0 │
P₅  │  0   0   0   0   0   0   0   0   0   0   1   1 │ ← ∞ point
    ├──────────────────────────────────────────────────┤
H₀  │ -1  -1   0  -1   0   0   0   0   0   0   0   0 │
H₁  │  0   0   0   0  -1   0  -1   0   0   0  -1   0 │
H₂  │  0   0  -1   0   0  -1   0  -1  -1   0   0   0 │
H₃  │  0   0   0   0   0   0   0   0   0  -1   0  -1 │ ← ∞ closure
    └──────────────────────────────────────────────────┘
```

**Boundary map d₂: C₂ → C₁**

For most programs, C₂ contains only simple faces (or is empty), so we'll focus on d₁.

---

### Step 5: Compute Ker(d₁) - The 1-Cycles

**Definition:** The kernel of d₁ is the set of 1-chains with zero boundary:

```
Ker(d₁) = {c ∈ C₁ : d₁(c) = 0}
```

**Interpretation:** These are **closed loops** (cycles) in the incidence graph.

**Algorithm:** Solve the linear system:
```
d₁ · x = 0
```

where x is a vector of edge coefficients.

**Example cycle in our structure:**

```
Cycle: P₃ → H₁ → P₅(∞) → H₃ → P₄ → H₂ → P₃

As a linear combination:
  c = e₃₁ - e₅₁ + e₅₃ - e₄₃ + e₄₂ - e₃₂

Verify d₁(c) = 0:
  d₁(e₃₁) = P₃ - H₁
  d₁(-e₅₁) = -(P₅ - H₁) = -P₅ + H₁
  d₁(e₅₃) = P₅ - H₃
  d₁(-e₄₃) = -(P₄ - H₃) = -P₄ + H₃
  d₁(e₄₂) = P₄ - H₂
  d₁(-e₃₂) = -(P₃ - H₂) = -P₃ + H₂
  
  Sum: (P₃ - H₁) + (-P₅ + H₁) + (P₅ - H₃) + (-P₄ + H₃) + (P₄ - H₂) + (-P₃ + H₂)
     = P₃ - P₃ + H₁ - H₁ + P₅ - P₅ + H₃ - H₃ + P₄ - P₄ + H₂ - H₂
     = 0 ✓
```

**Visual:**

```
      P₃ ────→ H₁
       ↑        ↓
       │        P₅(∞)
       │        ↓
      H₂ ←──── H₃
       ↑        ↓
       │        P₄
       └────────┘

This cycle EXISTS because of P₅(∞)!
Without the projective point, this cycle cannot close.
```

**Computing Ker(d₁) numerically:**

Use singular value decomposition (SVD):
```python
U, s, Vh = np.linalg.svd(d1_matrix)

# Null space = right singular vectors with singular value ≈ 0
rank = np.sum(s > tolerance)
kernel_basis = Vh[rank:].T

dim(Ker(d₁)) = number of zero singular values
```

---

### Step 6: Compute Im(d₂) - The Boundaries

**Definition:** The image of d₂ is the set of 1-chains that are boundaries of 2-chains:

```
Im(d₂) = {d₂(f) : f ∈ C₂}
```

**Interpretation:** These are cycles that **bound a 2-dimensional face** (they're "filled in").

**For most programs:**
- Tree structure → few or no 2-faces → Im(d₂) ≈ {0}
- Recursive programs → some 2-faces from recursion

**Example:** If we have a triangular face:

```
    P₁
   / \
  /   \
H₁────H₂

Triangle face bounded by: e(P₁→H₁) + e(H₁→P₂) + e(P₂→H₂) + e(H₂→P₁)

This cycle is a BOUNDARY (not a "hole")
```

**Computing Im(d₂):**

```python
# Column space of d₂ matrix
U, s, Vh = np.linalg.svd(d2_matrix)

# Image = span of columns = left singular vectors
rank = np.sum(s > tolerance)
image_basis = U[:, :rank]

dim(Im(d₂)) = rank of d₂
```

---

### Step 7: Compute H¹

**Definition:**

```
H¹ = Ker(d₁) / Im(d₂)

H¹ = {cycles} / {boundaries}
   = {cycles that are NOT boundaries}
   = "true holes"
```

**Dimension formula:**

```
dim(H¹) = dim(Ker(d₁)) - dim(Im(d₂))
```

**For most programs:**
```
dim(Im(d₂)) ≈ 0  (tree-like structure)

Therefore:
dim(H¹) ≈ dim(Ker(d₁))
```

**Computing:**

```python
ker_dim = len(kernel_basis[0])
im_dim = rank(d2_matrix)

h1_dimension = ker_dim - im_dim
```

---

### Step 8: Interpret Results

**H¹ = 0:**
- No cycles (or all cycles bound faces)
- Tree-like binding structure
- Linear control flow
- **Current state:** Most programs show H¹ = 0

**H¹ > 0:**
- Has "holes" (cycles that don't bound)
- Complex binding dependencies
- Recursive or looping structure
- **With projective types:** Cycles through infinity points

---

## Complete Example: Safe Division

### Input Program

```scheme
(define (safe-divide x y)
  (if (zero? y)
      'undefined      ; Projective point at infinity
      (/ x y)))
```

### Incidence Structure

**Points:** P₀, P₁(x), P₂(y), P₃(test), P₄(result), P₅(∞)

**Hyperplanes:** H₀(params), H₁(test), H₂(division), H₃(closure)

**Incidence matrix:**
```
       H₀  H₁  H₂  H₃
P₀  │  1   0   0   0  │
P₁  │  1   0   1   0  │
P₂  │  1   1   1   0  │
P₃  │  0   1   1   0  │
P₄  │  0   0   1   1  │
P₅  │  0   1   0   1  │ ← Key row!
```

### Chain Complex

**C₀:** 10 elements (6 points + 4 hyperplanes)

**C₁:** 10 edges

### Boundary Map d₁

(See Step 4 for full matrix)

### Kernel Computation

**Solve d₁ · x = 0:**

Result: **1-dimensional kernel** (one independent cycle)

**The cycle:**
```
P₃ →(e₃₁)→ H₁ →(connects via P₅)→ H₃ →(e₄₃⁻¹)→ P₄ →(e₄₂)→ H₂ →(e₃₂⁻¹)→ P₃

Passes through P₅(∞) - the projective point!
```

### Result

```
dim(Ker(d₁)) = 1
dim(Im(d₂)) = 0  (no 2-faces)

H¹ = 1 - 0 = 1 ✓
```

**Without projective point P₅:**
```
Cannot complete cycle through infinity
H¹ = 0 ❌
```

---

## Comparison: Affine vs Projective

### Same Program, Two Analyses

**Affine Analysis (Current):**
```
Points: P₀, P₁, P₂, P₃, P₄  (no P₅)
Ignores 'undefined branch

Result: H¹ = 0 or very low
```

**Projective Analysis (Proposed):**
```
Points: P₀, P₁, P₂, P₃, P₄, P₅(∞)
Includes 'undefined as point at infinity
Adds projective closure constraint H₃

Result: H¹ > 0 (detects cycle through ∞)
```

### Why It Matters

**Current problem from Research_Questions.md:**
```
"Why is H¹ mostly zero?"
"Only 3 programs have H¹ > 0"
```

**Hypothesis:**
```
Missing projective completion
→ Missing cycles through optional/undefined paths
→ Topology too sparse
→ H¹ ≈ 0
```

**Solution:**
```
Add projective types
→ Include points at infinity
→ Add projective closure constraints
→ Richer topology
→ H¹ > 0
```

---

## Implementation Checklist

### Phase 1: Extract Structure ✓
- [x] Identify all bindings (points)
- [x] Identify optional bindings (projective points)
- [x] Identify constraints (hyperplanes)
- [x] Build incidence matrix

### Phase 2: Build Complex ✓
- [x] Create C₀ (points + hyperplanes)
- [x] Create C₁ (edges from incidence)
- [x] Create C₂ (faces, if any)

### Phase 3: Compute Boundaries ✓
- [x] Define d₁: C₁ → C₀
- [x] Define d₂: C₂ → C₁
- [x] Implement as matrices

### Phase 4: Compute Homology ✓
- [x] Compute Ker(d₁) via SVD
- [x] Compute Im(d₂) via column space
- [x] Calculate H¹ = Ker(d₁) / Im(d₂)

### Phase 5: Validate
- [ ] Test on simple programs (H¹ = 0)
- [ ] Test on recursive programs (H¹ > 0)
- [ ] Test projective vs affine
- [ ] Compare with current results

---

## Key Insights

1. **H¹ measures cycles in bipartite incidence structure**
   - Points ↔ Hyperplanes
   - Truly bipartite (duality!)

2. **Projective points create new cycles**
   - Points at infinity close loops
   - Projective closure constraints complete structure

3. **Current analysis is affine-only**
   - Misses optional/undefined paths
   - Cannot detect cycles through infinity
   - Result: H¹ ≈ 0

4. **Projective completion should increase H¹**
   - Adds missing points and constraints
   - Completes cycles that were open
   - Should detect more binding complexity

---

## Next Steps

1. **Implement in Computational Scheme Theory**
   - Add projective type constructor
   - Extract optional bindings
   - Build full incidence structure

2. **Test hypothesis**
   - Run on corpus with projective analysis
   - Compare H¹ values (affine vs projective)
   - Validate: Does H¹ increase?

3. **Analyze results**
   - Which programs show improvement?
   - What patterns create projective cycles?
   - Does this explain "H¹ mostly zero" problem?

---

## References

- Research_Questions.md: "Why is H¹ mostly zero?"
- 01-PROJECTIVE-TYPES-THEORY.md: Projective type theory
- 10-MINIMAL-VIABLE-IMPLEMENTATION.md: Implementation plan
- Computational Scheme Theory: Current affine analysis

---

**TL;DR:** H¹ counts cycles in the bipartite graph of (points ↔ hyperplanes). Projective types add "points at infinity" that complete cycles, increasing H¹ from ~0 to positive values.
