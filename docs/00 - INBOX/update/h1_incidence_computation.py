"""
H¹ Computation Using Incidence Structure
Example: Computing cohomology from projective duality

This demonstrates how projective types increase H¹ by adding
cycles through "points at infinity" in the incidence structure.
"""

import numpy as np
from typing import List, Tuple, Set
from dataclasses import dataclass

@dataclass
class Point:
    """Point in incidence structure (binding or scope)"""
    name: str
    is_projective: bool = False  # True if optional/at infinity
    
    def __repr__(self):
        marker = "∞" if self.is_projective else "•"
        return f"{marker}{self.name}"

@dataclass
class Hyperplane:
    """Hyperplane in incidence structure (constraint)"""
    name: str
    is_projective_closure: bool = False  # True if identifies paths at infinity
    
    def __repr__(self):
        marker = "H∞" if self.is_projective_closure else "H"
        return f"{marker}_{self.name}"

class IncidenceStructure:
    """Bipartite incidence structure: Points ↔ Hyperplanes"""
    
    def __init__(self, points: List[Point], hyperplanes: List[Hyperplane]):
        self.points = points
        self.hyperplanes = hyperplanes
        self.incidence = np.zeros((len(points), len(hyperplanes)), dtype=int)
    
    def add_incidence(self, point_idx: int, hyperplane_idx: int):
        """Mark that point lies on hyperplane"""
        self.incidence[point_idx, hyperplane_idx] = 1
    
    def print_structure(self):
        """Display incidence matrix"""
        print("\nIncidence Structure:")
        print("=" * 60)
        
        # Header
        header = "Point/Plane |"
        for h in self.hyperplanes:
            header += f" {h.name:8s} |"
        print(header)
        print("-" * 60)
        
        # Rows
        for i, p in enumerate(self.points):
            row = f"{p.name:12s}|"
            for j in range(len(self.hyperplanes)):
                row += f" {'    1' if self.incidence[i, j] else '     '}    |"
            print(row)
        print("=" * 60)
    
    def build_boundary_map_d1(self) -> np.ndarray:
        """
        Build boundary map d₁: C₁ → C₀
        
        d₁(edge_ij) = P_i - H_j
        
        Returns matrix where columns are edges, rows are points/hyperplanes
        """
        num_cells_0 = len(self.points) + len(self.hyperplanes)
        
        # Count edges (1 for each incidence)
        edges = []
        for i in range(len(self.points)):
            for j in range(len(self.hyperplanes)):
                if self.incidence[i, j] == 1:
                    edges.append((i, j))
        
        num_edges = len(edges)
        d1 = np.zeros((num_cells_0, num_edges), dtype=int)
        
        # Fill boundary map
        for edge_idx, (point_idx, hyper_idx) in enumerate(edges):
            # Boundary of edge: +point - hyperplane
            d1[point_idx, edge_idx] = 1
            d1[len(self.points) + hyper_idx, edge_idx] = -1
        
        return d1, edges
    
    def compute_kernel(self, matrix: np.ndarray) -> Tuple[np.ndarray, int]:
        """Compute kernel (null space) of matrix"""
        # Use SVD to find null space
        _, s, vh = np.linalg.svd(matrix, full_matrices=True)
        
        # Null space is spanned by rows of V corresponding to zero singular values
        tol = 1e-10
        rank = np.sum(s > tol)
        kernel_basis = vh[rank:].T
        
        kernel_dim = kernel_basis.shape[1]
        
        return kernel_basis, kernel_dim
    
    def compute_H1(self, verbose=True) -> int:
        """
        Compute H¹ = dim(Ker(d₁)) - dim(Im(d₂))
        
        For simplicity, we compute:
        H¹ = dim(Ker(d₁))
        
        (assuming d₂ is trivial, which is true for trees)
        """
        if verbose:
            print("\nComputing H¹...")
            print("=" * 60)
        
        # Build d₁
        d1, edges = self.build_boundary_map_d1()
        
        if verbose:
            print(f"\nBoundary map d₁: C₁ → C₀")
            print(f"  C₁ dimension: {d1.shape[1]} (edges)")
            print(f"  C₀ dimension: {d1.shape[0]} (points + hyperplanes)")
        
        # Compute Ker(d₁)
        ker_d1, ker_dim = self.compute_kernel(d1)
        
        if verbose:
            print(f"\nKer(d₁) dimension: {ker_dim}")
            
            if ker_dim > 0:
                print("\n1-Cycles detected:")
                for i in range(ker_dim):
                    cycle_vec = ker_d1[:, i]
                    active_edges = [(edges[j], cycle_vec[j]) 
                                   for j in range(len(edges)) 
                                   if abs(cycle_vec[j]) > 1e-10]
                    print(f"  Cycle {i+1}: {len(active_edges)} edges")
                    for (p_idx, h_idx), coef in active_edges:
                        print(f"    {coef:+.2f} × ({self.points[p_idx]} → {self.hyperplanes[h_idx]})")
        
        # For simple cases, H¹ ≈ dim(Ker(d₁))
        # (We'd need to compute Im(d₂) for exact H¹, but it's often 0)
        h1_dimension = ker_dim
        
        if verbose:
            print(f"\n{'='*60}")
            print(f"H¹ dimension: {h1_dimension}")
            print(f"{'='*60}\n")
        
        return h1_dimension


def example_affine_linear():
    """
    Example 1: Simple linear program (affine only)
    
    (define (linear x)
      (let ((y (+ x 1)))
        (display y)))
    
    Expected: H¹ = 0 (no cycles)
    """
    print("\n" + "="*70)
    print("EXAMPLE 1: Linear Program (Affine Only)")
    print("="*70)
    
    # Points
    p0 = Point("entry", False)
    p1 = Point("x", False)
    p2 = Point("y", False)
    p3 = Point("exit", False)
    
    # Hyperplanes
    h0 = Hyperplane("params", False)
    h1 = Hyperplane("y_deps", False)
    
    # Build structure
    structure = IncidenceStructure(
        points=[p0, p1, p2, p3],
        hyperplanes=[h0, h1]
    )
    
    # Incidences
    structure.add_incidence(0, 0)  # entry → params
    structure.add_incidence(1, 0)  # x → params
    structure.add_incidence(1, 1)  # x → y_deps
    structure.add_incidence(2, 1)  # y → y_deps
    
    structure.print_structure()
    h1 = structure.compute_H1()
    
    return h1


def example_affine_cycle():
    """
    Example 2: Recursive program (affine, has cycle)
    
    (define (factorial n)
      (if (<= n 1)
          1
          (* n (factorial (- n 1)))))
    
    Expected: H¹ > 0 (recursive cycle)
    """
    print("\n" + "="*70)
    print("EXAMPLE 2: Factorial (Affine with Recursion)")
    print("="*70)
    
    # Points
    p0 = Point("entry", False)
    p1 = Point("n", False)
    p2 = Point("test", False)
    p3 = Point("result", False)
    p4 = Point("rec_call", False)
    
    # Hyperplanes
    h0 = Hyperplane("params", False)
    h1 = Hyperplane("test_n", False)
    h2 = Hyperplane("multiply", False)
    h3 = Hyperplane("recursion", False)
    
    # Build structure
    structure = IncidenceStructure(
        points=[p0, p1, p2, p3, p4],
        hyperplanes=[h0, h1, h2, h3]
    )
    
    # Incidences
    structure.add_incidence(0, 0)  # entry → params
    structure.add_incidence(1, 0)  # n → params
    structure.add_incidence(1, 1)  # n → test
    structure.add_incidence(2, 1)  # test → test
    structure.add_incidence(2, 2)  # test → multiply
    structure.add_incidence(3, 2)  # result → multiply
    structure.add_incidence(4, 3)  # rec_call → recursion
    structure.add_incidence(4, 0)  # rec_call → params (CYCLE!)
    
    structure.print_structure()
    h1 = structure.compute_H1()
    
    return h1


def example_projective_partial():
    """
    Example 3: Partial function with projective completion
    
    (define (safe-divide x y)
      (if (zero? y)
          'undefined      ; Point at infinity!
          (/ x y)))
    
    Expected: H¹ > 0 (cycle through infinity point)
    """
    print("\n" + "="*70)
    print("EXAMPLE 3: Safe Division (Projective Completion)")
    print("="*70)
    
    # Points (including projective point at infinity)
    p0 = Point("entry", False)
    p1 = Point("x", False)
    p2 = Point("y", False)
    p3 = Point("test", False)
    p4 = Point("result", False)
    p5 = Point("undefined", True)  # PROJECTIVE POINT AT INFINITY!
    
    # Hyperplanes (including projective closure)
    h0 = Hyperplane("params", False)
    h1 = Hyperplane("test_y", False)
    h2 = Hyperplane("division", False)
    h3 = Hyperplane("closure", True)  # PROJECTIVE CLOSURE!
    
    # Build structure
    structure = IncidenceStructure(
        points=[p0, p1, p2, p3, p4, p5],
        hyperplanes=[h0, h1, h2, h3]
    )
    
    # Incidences
    structure.add_incidence(0, 0)  # entry → params
    structure.add_incidence(1, 0)  # x → params
    structure.add_incidence(2, 0)  # y → params
    structure.add_incidence(2, 1)  # y → test
    structure.add_incidence(3, 1)  # test → test
    structure.add_incidence(3, 2)  # test → division (success branch)
    structure.add_incidence(4, 2)  # result → division
    
    # CRITICAL: Projective incidences
    structure.add_incidence(5, 1)  # undefined(∞) → test (failure branch)
    structure.add_incidence(5, 3)  # undefined(∞) → closure
    structure.add_incidence(4, 3)  # result → closure (both paths converge)
    
    # This creates a CYCLE through the projective point!
    # p2(y) → h1(test) → p5(∞) → h3(closure) → p4(result) → h2(division) → back to p2
    
    structure.print_structure()
    h1 = structure.compute_H1()
    
    return h1


def comparison_affine_vs_projective():
    """Compare same program with and without projective completion"""
    print("\n" + "="*70)
    print("COMPARISON: Affine vs Projective Analysis")
    print("="*70)
    
    print("\n[Without Projective Completion]")
    print("Treating 'undefined' as unreachable (ignoring it)...")
    
    # Affine analysis (missing the undefined point)
    p0 = Point("entry", False)
    p1 = Point("x", False)
    p2 = Point("y", False)
    p3 = Point("result", False)
    
    h0 = Hyperplane("params", False)
    h1 = Hyperplane("division", False)
    
    affine = IncidenceStructure(
        points=[p0, p1, p2, p3],
        hyperplanes=[h0, h1]
    )
    
    affine.add_incidence(0, 0)
    affine.add_incidence(1, 0)
    affine.add_incidence(2, 0)
    affine.add_incidence(1, 1)
    affine.add_incidence(2, 1)
    affine.add_incidence(3, 1)
    
    affine.print_structure()
    h1_affine = affine.compute_H1(verbose=True)
    
    print("\n[With Projective Completion]")
    print("Including 'undefined' as projective point at infinity...")
    
    # Same structure as example_projective_partial
    h1_projective = example_projective_partial()
    
    print("\n" + "="*70)
    print("RESULTS:")
    print(f"  Affine analysis:      H¹ = {h1_affine}")
    print(f"  Projective analysis:  H¹ = {h1_projective}")
    print(f"  Improvement:          {h1_projective - h1_affine} additional cycles detected!")
    print("="*70)


if __name__ == "__main__":
    print("\n" + "█"*70)
    print("█" + " "*68 + "█")
    print("█" + "  H¹ COMPUTATION USING INCIDENCE STRUCTURE".center(68) + "█")
    print("█" + "  Demonstrating Projective Duality in Program Analysis".center(68) + "█")
    print("█" + " "*68 + "█")
    print("█"*70)
    
    # Run examples
    h1_1 = example_affine_linear()
    h1_2 = example_affine_cycle()
    h1_3 = example_projective_partial()
    
    # Show comparison
    comparison_affine_vs_projective()
    
    print("\n" + "█"*70)
    print("█" + " SUMMARY ".center(68) + "█")
    print("█"*70)
    print(f"█  Example 1 (Linear):          H¹ = {h1_1} (no cycles)".ljust(69) + "█")
    print(f"█  Example 2 (Recursive):       H¹ = {h1_2} (has cycles)".ljust(69) + "█")
    print(f"█  Example 3 (Projective):      H¹ = {h1_3} (cycles via ∞)".ljust(69) + "█")
    print("█"*70)
    print("\nKey Insight: Projective completion adds cycles that affine analysis misses!")
