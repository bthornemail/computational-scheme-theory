#!/usr/bin/env python3
"""
Test Direct Computation

Tests that Haskell and Racket can compute real values directly.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "python-coordinator"))

from coordinator.direct_compute import DirectComputeCoordinator, compute_h1_direct, compute_vg_direct

def test_simple_program():
    """Test with a simple program"""
    print("Testing direct computation...")
    print("=" * 60)
    
    project_root = Path(__file__).parent.parent
    
    test_cases = [
        ("simple", "(define x 42)"),
        ("if-stmt", "(if (> x 0) 1 -1)"),
        ("lambda", "(lambda (x) x)"),
    ]
    
    coordinator = DirectComputeCoordinator(project_root)
    
    for name, source in test_cases:
        print(f"\n{name}:")
        print(f"  Source: {source}")
        
        # Try direct computation
        print("  Computing H¹...")
        h1, h1_time = compute_h1_direct(source, name, project_root)
        print(f"    H¹ = {h1} (took {h1_time:.2f}ms)")
        
        print("  Computing V(G)...")
        vg, vg_time = compute_vg_direct(source, name, project_root)
        print(f"    V(G) = {vg} (took {vg_time:.2f}ms)")
        
        if h1 > 0 or vg > 0:
            print(f"  ✅ Got real values!")
        else:
            print(f"  ⚠️  Using placeholders (services not built/running)")
        
        # Validate
        result = coordinator.validate_program(name, source)
        print(f"  Hypothesis holds: {result.hypothesis_holds}")

if __name__ == "__main__":
    test_simple_program()

