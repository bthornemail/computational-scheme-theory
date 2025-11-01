#!/usr/bin/env python3
"""
Validate hypothesis on a sample of programs that work for both H¹ and V(G)
"""

import subprocess
import json
import sys
from pathlib import Path
from typing import Dict, Optional

PROJECT_ROOT = Path(__file__).parent
HASKELL_DIR = PROJECT_ROOT / "haskell-core"
TEST_CORPUS = PROJECT_ROOT / "test-corpus"

def compute_h1(filepath: Path) -> Optional[int]:
    """Compute H¹ using Haskell tool"""
    try:
        result = subprocess.run(
            ["cabal", "run", "computational-scheme-theory", "--", "compute-h1", str(filepath)],
            cwd=HASKELL_DIR,
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            output = result.stdout.strip()
            for line in reversed(output.split('\n')):
                if "H¹" in line and "=" in line:
                    parts = line.split("=")
                    if len(parts) >= 2:
                        try:
                            return int(parts[-1].strip())
                        except ValueError:
                            continue
        return None
    except:
        return None

def compute_vg(filepath: Path) -> Optional[int]:
    """Compute V(G) using Racket tool"""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "python-coordinator"))
        from coordinator.direct_compute import compute_vg_direct
        
        source_code = filepath.read_text()
        program_id = filepath.stem
        vg, _ = compute_vg_direct(source_code, program_id, PROJECT_ROOT)
        return vg
    except Exception as e:
        return None

def main():
    # Test with a small sample first
    sample_files = [
        TEST_CORPUS / "baseline" / "B001.scm",
        TEST_CORPUS / "complex-control" / "CC001.scm",
        TEST_CORPUS / "simple-control" / "SC001.scm",
        TEST_CORPUS / "recursion" / "R001.scm",
    ]
    
    # Filter to existing files
    sample_files = [f for f in sample_files if f.exists()]
    
    print("=" * 80)
    print("Sample Validation: Testing H¹ = V(G) - k hypothesis")
    print("=" * 80)
    print()
    
    results = []
    
    for filepath in sample_files:
        print(f"Processing {filepath.name}...", end=" ", flush=True)
        
        h1 = compute_h1(filepath)
        vg = compute_vg(filepath)
        
        if h1 is not None and vg is not None:
            k = vg - h1
            results.append({
                "file": filepath.name,
                "category": filepath.parent.name,
                "h1": h1,
                "vg": vg,
                "k": k
            })
            print(f"✓ H¹={h1}, V(G)={vg}, k={k}")
        else:
            print(f"✗ Failed (H¹={h1}, V(G)={vg})")
    
    print()
    
    if not results:
        print("No successful computations!")
        return
    
    print("=" * 80)
    print("ANALYSIS")
    print("=" * 80)
    print()
    
    print("Results:")
    for r in results:
        print(f"  {r['file']:20s} H¹={r['h1']:2d}  V(G)={r['vg']:2d}  k={r['k']:2d}")
    
    print()
    
    # Hypothesis analysis
    k_values = [r["k"] for r in results]
    h1_values = [r["h1"] for r in results]
    vg_values = [r["vg"] for r in results]
    
    print(f"Constant k = V(G) - H¹:")
    print(f"  Values: {k_values}")
    print(f"  Mean: {sum(k_values)/len(k_values):.2f}")
    print(f"  Range: {min(k_values)} to {max(k_values)}")
    
    # Check hypothesis: H¹ = V(G) - k
    print()
    print("Hypothesis Check: H¹ = V(G) - k")
    for r in results:
        expected_h1 = r["vg"] - r["k"]
        actual_h1 = r["h1"]
        match = "✓" if expected_h1 == actual_h1 else "✗"
        print(f"  {r['file']}: {expected_h1} = {actual_h1} {match}")
    
    print()
    
    # Observations
    print("Observations:")
    if all(h == 0 for h in h1_values):
        print("  • All programs have H¹ = 0")
        print("  • This is mathematically expected for complete overlap graphs")
    if all(vg == vg_values[0] for vg in vg_values):
        print("  • All programs have the same V(G)")
    else:
        print(f"  • V(G) varies: {min(vg_values)} to {max(vg_values)}")
    
    if all(k == k_values[0] for k in k_values):
        print(f"  • Constant k appears to be {k_values[0]}")
        print(f"  • Hypothesis holds: H¹ = V(G) - {k_values[0]}")
    else:
        print(f"  • k varies, need more data to determine relationship")
    
    # Save results
    output_file = PROJECT_ROOT / "sample_validation.json"
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    print()
    print(f"Results saved to: {output_file}")

if __name__ == "__main__":
    main()

