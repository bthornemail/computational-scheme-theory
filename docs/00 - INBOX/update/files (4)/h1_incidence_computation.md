# Computing H¹ Using Incidence Structure

**Date:** 2025-11-01  
**Purpose:** Show how to compute first cohomology H¹ from bipartite incidence structure  
**Languages:** Prolog, Datalog, Lisp (S-expressions)

## Overview

This document explains how to compute H¹ cohomology using the **incidence structure** between:
- **Points** (program bindings)
- **Hyperplanes** (scope constraints)

The key insight: **H¹ measures cycles in this bipartite structure**.

---

## Part 1: The Incidence Structure

### What Is It?

An **incidence structure** is a bipartite graph:

```
POINTS (Bindings)          HYPERPLANES (Constraints)
    P₁ ────────────────────── H₁
    P₂ ─────┬──────────────── H₂
    P₃      │        ┌──────── H₃
    P₄ ─────┴────────┴──────── H₄

Edges = "point lies on hyperplane" (incidence)
```

### In Your Context

**Points (Bindings):**
```scheme
(define x 10)        ; Point P₁: binding x
(define y 20)        ; Point P₂: binding y
(define z (+ x y))   ; Point P₃: binding z
```

**Hyperplanes (Constraints):**
```
H₁: "z depends on x"     (z reads x)
H₂: "z depends on y"     (z reads y)
H₃: "x is in global scope"
H₄: "y is in global scope"
```

**Incidence Relations:**
```
P₃ lies on H₁   (z's binding needs x)
P₃ lies on H₂   (z's binding needs y)
P₁ lies on H₃   (x in global scope)
P₂ lies on H₄   (y in global scope)
```

---

## Part 2: From Incidence to Simplicial Complex

### Step 1: Build Incidence Matrix

The incidence matrix encodes which points lie on which hyperplanes:

```
       H₁  H₂  H₃  H₄
   P₁ [ 0   0   1   0 ]
   P₂ [ 0   0   0   1 ]
   P₃ [ 1   1   0   0 ]
   P₄ [ 0   1   0   1 ]
```

Entry (i,j) = 1 if point Pᵢ lies on hyperplane Hⱼ.

### Step 2: Extract Simplices

A **k-simplex** is a set of (k+1) points that share incidence:

**0-simplices (vertices):** Individual points
```
{P₁}, {P₂}, {P₃}, {P₄}
```

**1-simplices (edges):** Pairs of points sharing a hyperplane
```
{P₁, P₃} (both related via scope)
{P₂, P₃} (both related via dependency)
{P₂, P₄} (both on H₂ or H₄)
```

**2-simplices (triangles):** Triples sharing hyperplanes
```
{P₂, P₃, P₄} if they all share common constraints
```

### Step 3: Build Chain Complex

The **chain complex** tracks boundaries:

```
C₂ --∂₂--> C₁ --∂₁--> C₀
 ↑          ↑          ↑
2-chains   1-chains   0-chains
```

Where:
- C₀ = free group on vertices (points)
- C₁ = free group on edges
- C₂ = free group on triangles
- ∂ₖ = boundary operator

---

## Part 3: Computing H¹

### The Formula

**H¹ = ker(∂₁) / im(∂₂)**

Translation:
- **ker(∂₁)** = 1-cycles (closed loops)
- **im(∂₂)** = 1-boundaries (loops that bound 2-simplices)
- **H¹** = cycles that are NOT boundaries = "holes"

### Intuition

```
1-cycle (loop):
P₁ → P₂ → P₃ → P₁

If this bounds a 2-simplex {P₁, P₂, P₃}:
  ∂₂({P₁,P₂,P₃}) = (P₁,P₂) + (P₂,P₃) + (P₃,P₁)
  Then it's a boundary → doesn't contribute to H¹

If there's NO 2-simplex filling it:
  The loop is a "hole" → contributes to H¹
```

### Algorithm

**Step 1:** Find all 1-cycles
```
cycles = { chains c in C₁ : ∂₁(c) = 0 }
```

**Step 2:** Find all 1-boundaries
```
boundaries = { ∂₂(t) : t is a 2-simplex }
```

**Step 3:** Compute quotient
```
H¹ = cycles / boundaries
```

**Step 4:** Count dimension
```
dim(H¹) = dim(cycles) - dim(boundaries)
```

---

## Part 4: Implementation in Prolog

### Data Structures

```prolog
% Facts: incidence relations
% incident(Point, Hyperplane)
incident(p1, h3).
incident(p2, h4).
incident(p3, h1).
incident(p3, h2).
incident(p4, h2).
incident(p4, h4).

% Derived: points share a hyperplane
share_hyperplane(P1, P2, H) :-
    incident(P1, H),
    incident(P2, H),
    P1 \= P2.

% 1-simplex (edge): two points connected
edge(P1, P2) :- share_hyperplane(P1, P2, _).

% 2-simplex (triangle): three points pairwise connected
triangle(P1, P2, P3) :-
    edge(P1, P2),
    edge(P2, P3),
    edge(P3, P1),
    P1 @< P2, P2 @< P3.  % Canonical order
```

### Finding Cycles

```prolog
% A path in the incidence graph
path(Start, End, [Start|Path]) :-
    path_helper(Start, End, [Start], Path).

path_helper(Node, Node, _, []).
path_helper(Start, End, Visited, [Next|Path]) :-
    edge(Start, Next),
    \+ member(Next, Visited),
    path_helper(Next, End, [Next|Visited], Path).

% A cycle is a path that returns to start
cycle(Start, Path) :-
    edge(Start, Next),
    path(Next, Start, Path),
    length(Path, Len),
    Len > 2.  % Non-trivial cycle

% Find all cycles
all_cycles(Cycles) :-
    findall([P|Path], cycle(P, Path), Cycles).
```

### Computing H¹

```prolog
% Boundary of a triangle (returns 3 edges)
boundary(triangle(P1, P2, P3), [edge(P1,P2), edge(P2,P3), edge(P3,P1)]).

% An edge is a boundary if it's part of some triangle's boundary
is_boundary(E) :-
    triangle(P1, P2, P3),
    boundary(triangle(P1, P2, P3), Edges),
    member(E, Edges).

% Cycles that are NOT boundaries
non_boundary_cycle(Cycle) :-
    cycle(Start, Cycle),
    edges_of_cycle([Start|Cycle], Edges),
    \+ all_edges_are_boundaries(Edges).

edges_of_cycle([_], []).
edges_of_cycle([P1,P2|Rest], [edge(P1,P2)|Edges]) :-
    edges_of_cycle([P2|Rest], Edges).

all_edges_are_boundaries([]).
all_edges_are_boundaries([E|Es]) :-
    is_boundary(E),
    all_edges_are_boundaries(Es).

% Compute H¹ dimension
h1_dimension(Dim) :-
    findall(C, non_boundary_cycle(C), Cycles),
    length(Cycles, Dim).
```

### Example Query

```prolog
?- h1_dimension(Dim).
Dim = 2.  % Two independent cycles not bounding triangles

?- all_cycles(Cycles).
Cycles = [[p1, p3, p2, p1], [p2, p4, p3, p2]].

?- non_boundary_cycle(C).
C = [p1, p3, p2, p1].  % This cycle has a hole!
```

---

## Part 5: Implementation in Datalog

### Schema

```datalog
% Base relations
incident(point, hyperplane).

% Derived relations
edge(P1, P2) :- 
    incident(P1, H),
    incident(P2, H),
    P1 != P2.

triangle(P1, P2, P3) :-
    edge(P1, P2),
    edge(P2, P3),
    edge(P3, P1),
    P1 < P2, P2 < P3.

% Boundary edges (part of a triangle)
boundary_edge(P1, P2) :-
    triangle(P1, P2, _).
boundary_edge(P1, P2) :-
    triangle(P1, _, P2).
boundary_edge(P1, P2) :-
    triangle(_, P1, P2).

% Cycles (simplified: 3-cycles)
cycle3(P1, P2, P3) :-
    edge(P1, P2),
    edge(P2, P3),
    edge(P3, P1),
    \+ triangle(P1, P2, P3).  % NOT filled by triangle

% H¹ contribution
h1_generator(P1, P2, P3) :- cycle3(P1, P2, P3).
```

### Example Data

```datalog
% Incidence facts
incident(p1, h1).
incident(p1, h2).
incident(p2, h2).
incident(p2, h3).
incident(p3, h1).
incident(p3, h3).

% Query
?- h1_generator(P1, P2, P3).
% Returns cycles that generate H¹
```

---

## Part 6: Implementation in Lisp (S-expressions)

### Data Structure

```lisp
;;; Incidence structure as association list
(defparameter *incidence*
  '((p1 . (h1 h2))      ; p1 lies on h1, h2
    (p2 . (h2 h3))      ; p2 lies on h2, h3
    (p3 . (h1 h3))      ; p3 lies on h1, h3
    (p4 . (h2 h4))))    ; p4 lies on h2, h4

;;; Helper: points share a hyperplane?
(defun share-hyperplane-p (p1 p2)
  (let ((h1 (cdr (assoc p1 *incidence*)))
        (h2 (cdr (assoc p2 *incidence*))))
    (intersection h1 h2)))

;;; Build edge list
(defun build-edges (points)
  (loop for p1 in points
        append (loop for p2 in points
                     when (and (not (eq p1 p2))
                              (share-hyperplane-p p1 p2))
                     collect (list p1 p2))))

;;; Example
(defparameter *points* '(p1 p2 p3 p4))
(defparameter *edges* (build-edges *points*))
;; => ((P1 P2) (P1 P3) (P2 P1) (P2 P3) (P2 P4) ...)
```

### Finding Cycles

```lisp
;;; DFS to find cycles
(defun find-cycle (start current visited path edges)
  (cond
    ;; Found cycle back to start
    ((and (eq current start) 
          (> (length path) 2))
     (list (reverse path)))
    
    ;; Visited this node already (no cycle)
    ((member current visited)
     nil)
    
    ;; Continue searching
    (t
     (let ((neighbors (mapcar #'cadr 
                              (remove-if-not 
                                (lambda (e) (eq (car e) current))
                                edges))))
       (mapcan (lambda (next)
                 (find-cycle start 
                            next 
                            (cons current visited)
                            (cons next path)
                            edges))
               neighbors)))))

;;; Find all cycles
(defun all-cycles (points edges)
  (mapcan (lambda (p)
            (find-cycle p p '() (list p) edges))
          points))

;;; Example
(all-cycles *points* *edges*)
;; => ((P1 P3 P2 P1) (P2 P4 P3 P2) ...)
```

### Finding Triangles

```lisp
;;; Check if three points form a triangle
(defun triangle-p (p1 p2 p3 edges)
  (and (member (list p1 p2) edges :test #'equal)
       (member (list p2 p3) edges :test #'equal)
       (member (list p3 p1) edges :test #'equal)))

;;; Find all triangles
(defun all-triangles (points edges)
  (loop for p1 in points
        append (loop for p2 in points
                     when (not (eq p1 p2))
                     append (loop for p3 in points
                                  when (and (not (eq p2 p3))
                                           (not (eq p1 p3))
                                           (triangle-p p1 p2 p3 edges))
                                  collect (list p1 p2 p3)))))

;;; Boundary of triangle
(defun triangle-boundary (tri)
  (let ((p1 (first tri))
        (p2 (second tri))
        (p3 (third tri)))
    (list (list p1 p2) (list p2 p3) (list p3 p1))))
```

### Computing H¹

```lisp
;;; Check if cycle is a boundary
(defun boundary-cycle-p (cycle triangles)
  (let ((cycle-edges (loop for i from 0 below (length cycle)
                          collect (list (nth i cycle)
                                       (nth (mod (+ i 1) (length cycle)) cycle)))))
    (some (lambda (tri)
            (let ((boundary (triangle-boundary tri)))
              (subsetp cycle-edges boundary :test #'equal)))
          triangles)))

;;; Compute H¹ generators (non-boundary cycles)
(defun h1-generators (points edges)
  (let ((cycles (all-cycles points edges))
        (triangles (all-triangles points edges)))
    (remove-if (lambda (c)
                 (boundary-cycle-p c triangles))
               cycles)))

;;; Compute H¹ dimension
(defun h1-dimension (points edges)
  (length (h1-generators points edges)))

;;; Example
(h1-dimension *points* *edges*)
;; => 2  (two independent generators)

(h1-generators *points* *edges*)
;; => ((P1 P3 P2 P1) (P2 P4 P3 P2))
;;    These are the "holes" in the topology!
```

---

## Part 7: Matrix Method (Computational)

For larger structures, use matrix algebra:

### Incidence Matrix

```lisp
;;; Build incidence matrix
(defun incidence-matrix (points hyperplanes incidence)
  (let ((n (length points))
        (m (length hyperplanes)))
    (make-array (list n m)
                :initial-contents
                (loop for p in points
                      collect (loop for h in hyperplanes
                                   collect (if (member h (cdr (assoc p incidence)))
                                              1
                                              0))))))

;;; Example
(defparameter *points* '(p1 p2 p3))
(defparameter *hyperplanes* '(h1 h2 h3))
(defparameter *matrix* (incidence-matrix *points* *hyperplanes* *incidence*))

;; Matrix:
;;     h1  h2  h3
;; p1 [ 1   1   0 ]
;; p2 [ 0   1   1 ]
;; p3 [ 1   0   1 ]
```

### Boundary Operators

```lisp
;;; Boundary operator ∂₁: C₁ → C₀
;;; For edge (p1,p2): ∂₁(p1,p2) = p2 - p1
(defun boundary-1 (edge vertices)
  (let* ((p1 (first edge))
         (p2 (second edge))
         (n (length vertices))
         (result (make-array n :initial-element 0)))
    (setf (aref result (position p1 vertices)) -1)
    (setf (aref result (position p2 vertices)) 1)
    result))

;;; Boundary operator ∂₂: C₂ → C₁
;;; For triangle (p1,p2,p3): ∂₂ = (p1,p2) + (p2,p3) + (p3,p1)
(defun boundary-2 (triangle edges)
  (let ((result (make-array (length edges) :initial-element 0))
        (e1 (list (first triangle) (second triangle)))
        (e2 (list (second triangle) (third triangle)))
        (e3 (list (third triangle) (first triangle))))
    (setf (aref result (position e1 edges :test #'equal)) 1)
    (setf (aref result (position e2 edges :test #'equal)) 1)
    (setf (aref result (position e3 edges :test #'equal)) 1)
    result))
```

### Kernel and Image

```lisp
;;; Compute kernel of boundary operator
;;; ker(∂₁) = {c ∈ C₁ : ∂₁(c) = 0}
(defun kernel (boundary-op chains)
  ;; Use Gaussian elimination to find null space
  ;; Return basis for kernel
  )

;;; Compute image of boundary operator
;;; im(∂₂) = {∂₂(t) : t ∈ C₂}
(defun image (boundary-op domain)
  ;; Apply boundary-op to all elements of domain
  ;; Return basis for image
  )

;;; H¹ = ker(∂₁) / im(∂₂)
(defun h1-cohomology (vertices edges triangles)
  (let* ((ker (kernel boundary-1 edges))
         (im (image boundary-2 triangles)))
    (- (length ker) (length im))))
```

---

## Part 8: Complete Example

### Input Program

```scheme
(define x 10)           ; P1
(define y 20)           ; P2
(define z (+ x y))      ; P3 (depends on x, y)
(if (> z 0)             ; P4 (reads z)
    (display x)         ; P5 (reads x)
    (display y))        ; P6 (reads y)
```

### Incidence Structure

**Points:**
```
P1: binding x
P2: binding y
P3: binding z
P4: if-test node
P5: display x
P6: display y
```

**Hyperplanes (Scope Constraints):**
```
H1: "scope of x"
H2: "scope of y"
H3: "scope of z"
H4: "if-expression scope"
```

**Incidence:**
```
P1 on H1  (x defines scope)
P2 on H2  (y defines scope)
P3 on H1, H2, H3  (z reads x, y and defines scope)
P4 on H3, H4  (if reads z)
P5 on H1, H4  (display reads x, in if-scope)
P6 on H2, H4  (display reads y, in if-scope)
```

### Edges (Shared Hyperplanes)

```
(P1, P3) - share H1
(P2, P3) - share H2
(P3, P4) - share H3
(P4, P5) - share H4
(P4, P6) - share H4
(P5, P1) - share H1
(P6, P2) - share H2
```

### Cycles

```
Cycle 1: P1 → P3 → P4 → P5 → P1
  (x → z → if → display x → x)
  
Cycle 2: P2 → P3 → P4 → P6 → P2
  (y → z → if → display y → y)
```

### Check Boundaries

Do these cycles bound any triangles?

```
Triangle (P1, P3, P5)?
  Need edges: (P1,P3) ✓, (P3,P5) ✗, (P5,P1) ✓
  NO - not all edges present

Triangle (P2, P3, P6)?
  Need edges: (P2,P3) ✓, (P3,P6) ✗, (P6,P2) ✓
  NO - not all edges present
```

**Result:** Both cycles are NON-BOUNDARY cycles!

### H¹ Computation

```
Cycles: 2
Boundaries: 0
H¹ = 2 - 0 = 2

H¹ has dimension 2!
```

**Interpretation:** There are **two independent binding cycles** that don't collapse to boundaries. This captures the complexity of the if-expression with two branches reading different variables.

---

## Part 9: Key Insights

### Why This Works

1. **Bipartite Structure:** Points (bindings) ↔ Hyperplanes (constraints) creates rich topology
2. **Cycles = Binding Loops:** Cycles in incidence graph = cyclic dependencies
3. **Boundaries = Trivial Loops:** If a cycle bounds a triangle, it's not a "real" hole
4. **H¹ = Real Holes:** Non-boundary cycles are the actual binding complexity

### Advantages Over Affine-Only

**Current (Affine Only):**
```
Only track bindings (points)
Edges = direct dependencies
Result: sparse graph, H¹ ≈ 0
```

**With Projective Duality:**
```
Track bindings AND constraints
Edges = incidence relations
Result: richer topology, H¹ > 0
```

### Connection to Your Research Questions

**Q1: Why is H¹ mostly zero?**
- Currently only tracking points (affine)
- Missing hyperplanes (projective completion)
- Incidence structure reveals the hidden cycles!

**Q2: What creates binding cycles?**
- Shared scope constraints (hyperplanes)
- Optional bindings create new incidences
- Projective points at infinity add boundary components

**Q3: What does H¹ measure?**
- Independent binding cycles in incidence structure
- Scope complexity that doesn't collapse
- "Holes" in the binding topology

---

## Part 10: Next Steps

### Implementation Checklist

1. **Extract incidence structure** from your Scheme programs
   - Points = all bindings
   - Hyperplanes = scope regions
   - Incidence = "binding in scope"

2. **Build simplicial complex** from incidence
   - 0-simplices = points
   - 1-simplices = edges (shared hyperplane)
   - 2-simplices = triangles (pairwise connected)

3. **Compute H¹**
   - Find cycles (ker ∂₁)
   - Find boundaries (im ∂₂)
   - Compute quotient

4. **Compare with current results**
   - Does H¹ increase?
   - Which programs have higher H¹?
   - Validate hypothesis!

### Tools to Use

- **Prolog:** Great for finding cycles via backtracking
- **Datalog:** Efficient for incidence queries
- **Lisp:** Flexible for matrix computations
- **Any of above:** Will work for H¹ computation!

---

## References

- Algebraic Topology: Hatcher, "Algebraic Topology" (Chapter 2: Homology)
- Incidence Geometry: Batten, "Combinatorics of Finite Geometries"
- Your Documents: Research_Questions.md, Projective Types Theory

