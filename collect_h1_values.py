#!/usr/bin/env python3
"""
Collect H¹ values for all programs in test corpus
"""

import subprocess
import json
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
            # Look for line containing H¹ and = 
            for line in reversed(output.split('\n')):
                if "H¹" in line and "=" in line:
                    # Extract number after =
                    parts = line.split("=")
                    if len(parts) >= 2:
                        try:
                            value = int(parts[-1].strip())
                            return value
                        except ValueError:
                            continue
        return None
    except:
        return None

def main():
    scheme_files = sorted([f for f in TEST_CORPUS.rglob("*.scm") 
                          if "json" not in f.name.lower() and "temp" not in f.name.lower()])
    
    print(f"Collecting H¹ values for {len(scheme_files)} programs...")
    print()
    
    results = []
    successful = 0
    
    for i, filepath in enumerate(scheme_files, 1):
        if i % 10 == 0:
            print(f"Progress: {i}/{len(scheme_files)}...", flush=True)
        
        h1 = compute_h1(filepath)
        category = filepath.parent.name
        
        if h1 is not None:
            successful += 1
            results.append({
                "file": str(filepath.relative_to(PROJECT_ROOT)),
                "category": category,
                "h1": h1
            })
    
    print(f"\n✓ Collected H¹ for {successful}/{len(scheme_files)} programs")
    
    # Save results
    output_file = PROJECT_ROOT / "h1_values.json"
    with open(output_file, 'w') as f:
        json.dump({
            "total_programs": len(scheme_files),
            "successful": successful,
            "results": results
        }, f, indent=2)
    
    print(f"Results saved to: {output_file}")
    
    # Summary by category
    from collections import defaultdict
    by_category = defaultdict(lambda: {"count": 0, "h1_sum": 0, "h1_values": []})
    
    for r in results:
        cat = r["category"]
        by_category[cat]["count"] += 1
        by_category[cat]["h1_sum"] += r["h1"]
        by_category[cat]["h1_values"].append(r["h1"])
    
    print("\nSummary by category:")
    for cat, data in sorted(by_category.items()):
        mean_h1 = data["h1_sum"] / data["count"] if data["count"] > 0 else 0
        print(f"  {cat}: {data['count']} programs, mean H¹ = {mean_h1:.2f}")
    
    # Overall statistics
    if results:
        h1_values = [r["h1"] for r in results]
        print(f"\nOverall H¹ statistics:")
        print(f"  Min: {min(h1_values)}")
        print(f"  Max: {max(h1_values)}")
        print(f"  Mean: {sum(h1_values)/len(h1_values):.2f}")
        print(f"  Zero count: {sum(1 for h in h1_values if h == 0)}/{len(h1_values)}")

if __name__ == "__main__":
    main()

