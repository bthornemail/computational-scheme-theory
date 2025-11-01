#!/usr/bin/env python3
"""
Hypothesis Validation Script

Computes H¹ and V(G) for all programs in the test corpus and analyzes
the correlation to test the hypothesis: H¹ = V(G) - k
"""

import subprocess
import json
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from collections import defaultdict
import statistics

# Project root
PROJECT_ROOT = Path(__file__).parent
HASKELL_DIR = PROJECT_ROOT / "haskell-core"
TEST_CORPUS = PROJECT_ROOT / "test-corpus"
RACKET_DIR = PROJECT_ROOT / "racket-metrics"

def compute_h1(filepath: Path) -> Optional[int]:
    """Compute H¹ using Haskell tool"""
    try:
        result = subprocess.run(
            ["cabal", "run", "--", "computational-scheme-theory", "--", "compute-h1", str(filepath)],
            cwd=HASKELL_DIR,
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            # Parse output like "H¹(X_Comp, O_Comp) = 0"
            output = result.stdout.strip()
            # Find the last line that contains "="
            for line in reversed(output.split('\n')):
                if "=" in line and "H¹" in line:
                    h1_str = line.split("=")[-1].strip()
                    try:
                        return int(h1_str)
                    except ValueError:
                        continue
            return None
        else:
            # Don't print errors, just return None
            return None
    except subprocess.TimeoutExpired:
        return None
    except Exception as e:
        return None

def compute_vg(filepath: Path) -> Optional[int]:
    """Compute V(G) using Racket tool via Python coordinator"""
    try:
        sys.path.insert(0, str(PROJECT_ROOT / "python-coordinator"))
        from coordinator.direct_compute import compute_vg_direct
        
        source_code = filepath.read_text()
        program_id = filepath.stem
        vg, _ = compute_vg_direct(source_code, program_id, PROJECT_ROOT)
        # Return None if vg is 0 and we suspect it's an error
        # (Some programs legitimately have V(G) = 0 or 1, so we can't filter too aggressively)
        return vg
    except Exception as e:
        return None

def find_all_scheme_files() -> List[Path]:
    """Find all .scm files in test corpus"""
    scheme_files = []
    for scm_file in TEST_CORPUS.rglob("*.scm"):
        # Skip JSON files and temp files
        if "json" not in scm_file.name.lower() and "temp" not in scm_file.name.lower():
            scheme_files.append(scm_file)
    return sorted(scheme_files)

def validate_hypothesis():
    """Main validation function"""
    print("=" * 80)
    print("Computational Scheme Theory - Hypothesis Validation")
    print("Testing: H¹ = V(G) - k")
    print("=" * 80)
    print()
    
    scheme_files = find_all_scheme_files()
    print(f"Found {len(scheme_files)} Scheme programs")
    print()
    
    results = []
    errors = []
    
    for i, filepath in enumerate(scheme_files, 1):
        if i % 10 == 0 or i == 1:
            print(f"[{i}/{len(scheme_files)}] Processing {filepath.name}...", end="", flush=True)
        
        h1 = compute_h1(filepath)
        vg = compute_vg(filepath)
        
        if h1 is not None and vg is not None:
            category = filepath.parent.name
            results.append({
                "file": str(filepath.relative_to(PROJECT_ROOT)),
                "category": category,
                "h1": h1,
                "vg": vg,
                "difference": vg - h1,
                "ratio": vg / h1 if h1 != 0 else None
            })
            if i % 10 == 0 or i == 1:
                print(f" ✓ H¹={h1}, V(G)={vg}, k={vg-h1}")
        else:
            errors.append({
                "file": str(filepath.relative_to(PROJECT_ROOT)),
                "h1": h1,
                "vg": vg
            })
            if i % 10 == 0 or i == 1:
                print(f" ✗")
    
    print(f"\nProcessed {len(scheme_files)} files: {len(results)} successful, {len(errors)} errors")
    print()
    
    # Analysis
    if not results:
        print("No successful computations!")
        return
    
    print("=" * 80)
    print("ANALYSIS")
    print("=" * 80)
    print()
    
    # Group by category
    by_category = defaultdict(list)
    for r in results:
        by_category[r["category"]].append(r)
    
    print(f"Total programs analyzed: {len(results)}")
    print(f"Programs with errors: {len(errors)}")
    print()
    
    # Summary statistics
    h1_values = [r["h1"] for r in results]
    vg_values = [r["vg"] for r in results]
    differences = [r["difference"] for r in results]
    
    print("Summary Statistics:")
    print(f"  H¹: min={min(h1_values)}, max={max(h1_values)}, mean={statistics.mean(h1_values):.2f}, median={statistics.median(h1_values):.2f}")
    print(f"  V(G): min={min(vg_values)}, max={max(vg_values)}, mean={statistics.mean(vg_values):.2f}, median={statistics.median(vg_values):.2f}")
    print(f"  V(G) - H¹: min={min(differences)}, max={max(differences)}, mean={statistics.mean(differences):.2f}, median={statistics.median(differences):.2f}")
    print()
    
    # Correlation analysis
    if len(results) > 1:
        try:
            correlation = statistics.correlation(h1_values, vg_values) if hasattr(statistics, 'correlation') else None
            if correlation is not None:
                print(f"Correlation (H¹, V(G)): {correlation:.4f}")
            else:
                # Manual correlation calculation
                if len(h1_values) > 1:
                    n = len(h1_values)
                    mean_h1 = statistics.mean(h1_values)
                    mean_vg = statistics.mean(vg_values)
                    numerator = sum((h1_values[i] - mean_h1) * (vg_values[i] - mean_vg) for i in range(n))
                    denominator_h1 = sum((h1_values[i] - mean_h1) ** 2 for i in range(n))
                    denominator_vg = sum((vg_values[i] - mean_vg) ** 2 for i in range(n))
                    if denominator_h1 > 0 and denominator_vg > 0:
                        correlation = numerator / ((denominator_h1 * denominator_vg) ** 0.5)
                        print(f"Correlation (H¹, V(G)): {correlation:.4f}")
        except Exception as e:
            print(f"Could not compute correlation: {e}")
        print()
    
    # Analyze constant k
    # If H¹ = V(G) - k, then k = V(G) - H¹
    k_values = [r["difference"] for r in results]
    k_mean = statistics.mean(k_values)
    k_median = statistics.median(k_values)
    k_mode = max(set(k_values), key=k_values.count) if k_values else None
    
    print(f"Estimated constant k = V(G) - H¹:")
    print(f"  Mean: {k_mean:.2f}")
    print(f"  Median: {k_median:.2f}")
    print(f"  Mode: {k_mode}")
    print(f"  Standard deviation: {statistics.stdev(k_values) if len(k_values) > 1 else 0:.2f}")
    print()
    
    # Category breakdown
    print("Results by Category:")
    for category, cat_results in sorted(by_category.items()):
        cat_h1 = [r["h1"] for r in cat_results]
        cat_vg = [r["vg"] for r in cat_results]
        cat_k = [r["difference"] for r in cat_results]
        print(f"  {category}: {len(cat_results)} programs")
        print(f"    H¹: mean={statistics.mean(cat_h1):.2f}")
        print(f"    V(G): mean={statistics.mean(cat_vg):.2f}")
        print(f"    k: mean={statistics.mean(cat_k):.2f}")
    print()
    
    # Programs where hypothesis holds
    # Check if k is approximately constant
    k_std = statistics.stdev(k_values) if len(k_values) > 1 else 0
    if k_std < 1.0:  # Low standard deviation means k is approximately constant
        print(f"✓ Hypothesis appears valid: k ≈ {k_mean:.2f} (std={k_std:.2f})")
    else:
        print(f"⚠ Hypothesis may not hold: k varies significantly (std={k_std:.2f})")
    print()
    
    # Save results
    output_file = PROJECT_ROOT / "validation_results.json"
    output_data = {
        "summary": {
            "total_programs": len(results),
            "errors": len(errors),
            "mean_h1": statistics.mean(h1_values),
            "mean_vg": statistics.mean(vg_values),
            "estimated_k": k_mean,
            "k_std": statistics.stdev(k_values) if len(k_values) > 1 else 0
        },
        "results": results,
        "errors": errors
    }
    
    with open(output_file, 'w') as f:
        json.dump(output_data, f, indent=2)
    
    print(f"Results saved to: {output_file}")
    print()
    
    # Print sample results
    print("Sample Results (first 10):")
    for r in results[:10]:
        print(f"  {r['file']}: H¹={r['h1']}, V(G)={r['vg']}, k={r['difference']}")
    
    if len(results) > 10:
        print(f"  ... and {len(results) - 10} more")

if __name__ == "__main__":
    validate_hypothesis()

