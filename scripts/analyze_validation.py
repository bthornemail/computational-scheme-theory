#!/usr/bin/env python3
"""
Analyze validation results and generate detailed report
"""

import json
import sys
from pathlib import Path
from collections import defaultdict
import statistics

def load_validation_results(report_file):
    """Load validation results from JSON file"""
    with open(report_file, 'r') as f:
        data = json.load(f)
    return data

def analyze_results(data):
    """Analyze validation results"""
    results = data.get('results', [])
    
    # Group by category
    by_category = defaultdict(list)
    by_h1 = defaultdict(int)
    by_vg = defaultdict(int)
    
    non_zero_h1 = []
    non_zero_vg = []
    
    for r in results:
        category = r.get('category', 'unknown')
        h1 = r.get('h1', 0)
        vg = r.get('vg', 0)
        
        by_category[category].append(r)
        by_h1[h1] += 1
        by_vg[vg] += 1
        
        if h1 != 0:
            non_zero_h1.append(h1)
        if vg != 0:
            non_zero_vg.append(vg)
    
    print("=" * 70)
    print("VALIDATION ANALYSIS REPORT")
    print("=" * 70)
    print()
    
    print(f"Total Programs Validated: {len(results)}")
    print()
    
    print("Results by Category:")
    print("-" * 70)
    for category, category_results in sorted(by_category.items()):
        holds = sum(1 for r in category_results if r.get('hypothesis_holds', False))
        total = len(category_results)
        print(f"  {category:20s}: {total:3d} programs, {holds:3d} hold ({holds/total*100:.1f}%)")
    print()
    
    print("H¹ Value Distribution:")
    print("-" * 70)
    for h1_val in sorted(by_h1.keys()):
        count = by_h1[h1_val]
        print(f"  H¹ = {h1_val:3d}: {count:3d} programs")
    print()
    
    print("V(G) Value Distribution:")
    print("-" * 70)
    for vg_val in sorted(by_vg.keys()):
        count = by_vg[vg_val]
        print(f"  V(G) = {vg_val:3d}: {count:3d} programs")
    print()
    
    if non_zero_h1:
        print(f"Non-zero H¹ values: {len(non_zero_h1)} programs")
        print(f"  Mean: {statistics.mean(non_zero_h1):.2f}")
        print(f"  Min: {min(non_zero_h1)}")
        print(f"  Max: {max(non_zero_h1)}")
    else:
        print("⚠️  All H¹ values are 0 - programs may be too simple or computation needs investigation")
    
    print()
    
    if non_zero_vg:
        print(f"Non-zero V(G) values: {len(non_zero_vg)} programs")
        print(f"  Mean: {statistics.mean(non_zero_vg):.2f}")
        print(f"  Min: {min(non_zero_vg)}")
        print(f"  Max: {max(non_zero_vg)}")
    else:
        print("⚠️  All V(G) values are 0 - Racket computation may need fixes")
    
    print()
    
    # Hypothesis validation
    holds = sum(1 for r in results if r.get('hypothesis_holds', False))
    total = len(results)
    print("Hypothesis Validation:")
    print("-" * 70)
    print(f"  Holds: {holds}/{total} ({holds/total*100:.1f}%)")
    print(f"  Fails: {total-holds}/{total} ({(total-holds)/total*100:.1f}%)")
    print()
    
    # Correlation analysis
    h1_vals = [r.get('h1', 0) for r in results]
    vg_vals = [r.get('vg', 0) for r in results]
    
    if len(set(h1_vals)) > 1 and len(set(vg_vals)) > 1:
        try:
            correlation = data.get('statistics', {}).get('correlation', 0)
            print(f"Correlation (H¹, V(G)): {correlation:.4f}")
        except:
            print("Correlation: Unable to compute")
    else:
        print("⚠️  Correlation cannot be computed (all values identical)")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        report_file = Path("validation_report.json")
    else:
        report_file = Path(sys.argv[1])
    
    if not report_file.exists():
        print(f"Error: Report file not found: {report_file}")
        sys.exit(1)
    
    data = load_validation_results(report_file)
    analyze_results(data)

