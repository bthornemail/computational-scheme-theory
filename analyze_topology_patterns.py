#!/usr/bin/env python3
"""
Analyze topology patterns across different program types
to understand why H¹ = 0 and identify patterns
"""

import subprocess
import json
from pathlib import Path
from collections import defaultdict

PROJECT_ROOT = Path(__file__).parent
HASKELL_DIR = PROJECT_ROOT / "haskell-core"

def compute_h1(filepath: Path):
    """Compute H¹ for a program"""
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
            # Parse nerve stats from output
            stats = {}
            for line in output.split('\n'):
                if "Enhanced nerve complete:" in line:
                    # Extract: "N vertices, E edges, T triangles"
                    parts = line.split(":")
                    if len(parts) > 1:
                        data = parts[1].strip()
                        # Parse numbers
                        import re
                        nums = re.findall(r'\d+', data)
                        if len(nums) >= 3:
                            stats['vertices'] = int(nums[0])
                            stats['edges'] = int(nums[1])
                            stats['triangles'] = int(nums[2])
                elif "H¹" in line and "=" in line:
                    parts = line.split("=")
                    if len(parts) >= 2:
                        try:
                            stats['h1'] = int(parts[-1].strip())
                        except ValueError:
                            pass
            return stats if stats else None
        return None
    except:
        return None

def analyze_topology_patterns():
    """Analyze topology patterns across program categories"""
    
    # Load existing H¹ data
    h1_file = PROJECT_ROOT / "h1_values.json"
    if h1_file.exists():
        with open(h1_file) as f:
            h1_data = json.load(f)
        
        # Get files with stats
        results = []
        for r in h1_data.get("results", []):
            filepath = PROJECT_ROOT / r["file"]
            if filepath.exists():
                stats = compute_h1(filepath)
                if stats:
                    stats['file'] = r["file"]
                    stats['category'] = r["category"]
                    results.append(stats)
        
        print("Topology Pattern Analysis")
        print("=" * 80)
        print()
        
        # Group by category
        by_category = defaultdict(list)
        for r in results:
            by_category[r['category']].append(r)
        
        print("Patterns by Category:")
        print()
        
        patterns = defaultdict(int)
        
        for cat, cat_results in sorted(by_category.items()):
            print(f"{cat}:")
            # Analyze patterns
            for r in cat_results[:5]:  # Show first 5
                v, e, t = r.get('vertices', 0), r.get('edges', 0), r.get('triangles', 0)
                h1 = r.get('h1', 0)
                
                # Classify pattern
                if v == 2 and e == 1:
                    pattern = "Tree (2 nodes)"
                elif v > 2 and e == v - 1:
                    pattern = f"Tree ({v} nodes)"
                elif e == v * (v - 1) // 2:
                    pattern = f"Complete K{v}"
                elif v == e + 1:
                    pattern = "Star graph"
                else:
                    pattern = f"Graph ({v}V, {e}E, {t}T)"
                
                print(f"  {Path(r['file']).name:20s} {pattern:20s} H¹={h1}")
                patterns[pattern] += 1
            
            if len(cat_results) > 5:
                print(f"  ... and {len(cat_results) - 5} more")
            print()
        
        print("Pattern Summary:")
        for pattern, count in sorted(patterns.items(), key=lambda x: -x[1]):
            print(f"  {pattern:30s}: {count} programs")
        
        print()
        
        # Analyze H¹ distribution
        h1_values = [r.get('h1', 0) for r in results]
        if h1_values:
            print("H¹ Distribution:")
            print(f"  H¹ = 0: {sum(1 for h in h1_values if h == 0)}/{len(h1_values)}")
            print(f"  H¹ > 0: {sum(1 for h in h1_values if h > 0)}/{len(h1_values)}")
            if any(h > 0 for h in h1_values):
                print(f"  Max H¹: {max(h1_values)}")
                print(f"  Programs with H¹ > 0:")
                for r in results:
                    if r.get('h1', 0) > 0:
                        print(f"    {r['file']}: H¹ = {r['h1']}")
        
        # Graph structure analysis
        print()
        print("Graph Structure Analysis:")
        all_edges = [r.get('edges', 0) for r in results]
        all_vertices = [r.get('vertices', 0) for r in results]
        
        if all_vertices and all_edges:
            print(f"  Average vertices: {sum(all_vertices)/len(all_vertices):.1f}")
            print(f"  Average edges: {sum(all_edges)/len(all_edges):.1f}")
            print(f"  Average edge/vertex ratio: {sum(all_edges)/sum(all_vertices):.2f}")
            
            # Check for cycles (for a graph, cycles exist if e >= v)
            cyclic = sum(1 for v, e in zip(all_vertices, all_edges) if e >= v and v > 2)
            print(f"  Potentially cyclic graphs: {cyclic}/{len(results)}")
            
            # Trees have e = v - 1
            trees = sum(1 for v, e in zip(all_vertices, all_edges) if e == v - 1)
            print(f"  Tree structures: {trees}/{len(results)}")

if __name__ == "__main__":
    analyze_topology_patterns()

