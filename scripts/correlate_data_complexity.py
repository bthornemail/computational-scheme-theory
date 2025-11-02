#!/usr/bin/env python3
"""
Correlation analysis: H¹ vs. data complexity metrics
Compares H¹↔V(G) vs. H¹↔DataMetrics
"""

import json
import sys
from pathlib import Path
from typing import Dict, List
import argparse

try:
    import numpy as np
    from scipy.stats import pearsonr, spearmanr
    HAS_NUMPY = True
except ImportError:
    HAS_NUMPY = False
    print("Warning: numpy/scipy not available. Using basic correlation only.")


def load_results(json_file: Path) -> List[Dict]:
    """Load validation results."""
    with open(json_file, 'r') as f:
        data = json.load(f)
    if isinstance(data, dict) and 'results' in data:
        return data['results']
    elif isinstance(data, list):
        return data
    else:
        return []


def extract_metrics(results: List[Dict]) -> Dict:
    """Extract metrics from results."""
    metrics = {
        'h1': [],
        'vg': [],
        'halstead_volume': [],
        'information_flow': [],
        'loc': [],
        'program_ids': []
    }
    
    for result in results:
        if result.get('success', False):
            metrics['program_ids'].append(result.get('program_id', 'unknown'))
            metrics['h1'].append(result.get('h1', 0))
            metrics['vg'].append(result.get('vg', 0))
            # Data complexity metrics (if available)
            metrics['halstead_volume'].append(result.get('halstead_volume', 0))
            metrics['information_flow'].append(result.get('information_flow_complexity', 0))
            metrics['loc'].append(result.get('loc', 0))
    
    return metrics


def compute_correlation(x_values: List[float], y_values: List[float]) -> Dict:
    """Compute correlation between two metric sets."""
    if not HAS_NUMPY or len(x_values) < 2 or len(y_values) < 2:
        return {'r': None, 'p_value': None, 'method': 'unavailable'}
    
    # Filter out pairs where both are available
    pairs = [(x, y) for x, y in zip(x_values, y_values) if x is not None and y is not None]
    if len(pairs) < 2:
        return {'r': None, 'p_value': None, 'method': 'insufficient_data'}
    
    x_vals = [p[0] for p in pairs]
    y_vals = [p[1] for p in pairs]
    
    pearson_r, pearson_p = pearsonr(x_vals, y_vals)
    spearman_r, spearman_p = spearmanr(x_vals, y_vals)
    
    return {
        'pearson': {'r': pearson_r, 'p_value': pearson_p},
        'spearman': {'r': spearman_r, 'p_value': spearman_p},
        'n': len(pairs),
        'method': 'scipy'
    }


def generate_correlation_report(metrics: Dict, output_file: Path = None):
    """Generate correlation analysis report."""
    lines = [
        "# Correlation Analysis: H¹ vs. Data Complexity Metrics",
        "",
        "**Date**: Generated automatically",
        "",
        "---",
        "",
        "## Correlation Summary",
        "",
    ]
    
    # H¹ vs. V(G)
    corr_h1_vg = compute_correlation(metrics['h1'], metrics['vg'])
    lines.extend([
        "### H¹ vs. V(G) (Control Flow Complexity)",
        "",
        f"- **Pearson r**: {corr_h1_vg.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_vg.get('pearson', {}).get('r') is not None else "- **Pearson r**: N/A",
        f"- **Spearman ρ**: {corr_h1_vg.get('spearman', {}).get('r', 'N/A'):.4f}" if corr_h1_vg.get('spearman', {}).get('r') is not None else "- **Spearman ρ**: N/A",
        f"- **n**: {corr_h1_vg.get('n', 0)}",
        "",
    ])
    
    # H¹ vs. Halstead Volume
    corr_h1_halstead = compute_correlation(metrics['h1'], metrics['halstead_volume'])
    lines.extend([
        "### H¹ vs. Halstead Volume (Data Complexity)",
        "",
        f"- **Pearson r**: {corr_h1_halstead.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_halstead.get('pearson', {}).get('r') is not None else "- **Pearson r**: N/A",
        f"- **Spearman ρ**: {corr_h1_halstead.get('spearman', {}).get('r', 'N/A'):.4f}" if corr_h1_halstead.get('spearman', {}).get('r') is not None else "- **Spearman ρ**: N/A",
        f"- **n**: {corr_h1_halstead.get('n', 0)}",
        "",
    ])
    
    # H¹ vs. Information Flow
    corr_h1_if = compute_correlation(metrics['h1'], metrics['information_flow'])
    lines.extend([
        "### H¹ vs. Information Flow Complexity (Data Complexity)",
        "",
        f"- **Pearson r**: {corr_h1_if.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_if.get('pearson', {}).get('r') is not None else "- **Pearson r**: N/A",
        f"- **Spearman ρ**: {corr_h1_if.get('spearman', {}).get('r', 'N/A'):.4f}" if corr_h1_if.get('spearman', {}).get('r') is not None else "- **Spearman ρ**: N/A",
        f"- **n**: {corr_h1_if.get('n', 0)}",
        "",
    ])
    
    lines.extend([
        "---",
        "",
        "## Interpretation",
        "",
        "### Hypothesis Testing",
        "",
        "**H₀**: H¹ correlates with V(G) (control complexity)",
        f"**Result**: r = {corr_h1_vg.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_vg.get('pearson', {}).get('r') is not None else "**Result**: r = N/A",
        "",
        "**H₁**: H¹ correlates with data complexity metrics (Halstead, Information Flow)",
        f"**Result**: r(H¹, Halstead) = {corr_h1_halstead.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_halstead.get('pearson', {}).get('r') is not None else "**Result**: r(H¹, Halstead) = N/A",
        f"**Result**: r(H¹, Information Flow) = {corr_h1_if.get('pearson', {}).get('r', 'N/A'):.4f}" if corr_h1_if.get('pearson', {}).get('r') is not None else "**Result**: r(H¹, Information Flow) = N/A",
        "",
        "### Conclusion",
        "",
        "If H¹ correlates more strongly with data complexity metrics than with V(G), this supports",
        "the hypothesis that H¹ measures **binding/data complexity** rather than control complexity.",
        "",
        "---",
        "",
        "**Generated**: Automatically",
    ])
    
    report = "\n".join(lines)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(report)
        print(f"Report written to {output_file}")
    else:
        print(report)
    
    return report


def main():
    parser = argparse.ArgumentParser(
        description="Correlation analysis: H¹ vs. data complexity metrics"
    )
    parser.add_argument(
        "results_file",
        type=Path,
        help="Path to validation results JSON file"
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output file path (default: results_file.parent/correlation_analysis.md)"
    )
    
    args = parser.parse_args()
    
    if not args.results_file.exists():
        print(f"Error: File not found: {args.results_file}", file=sys.stderr)
        sys.exit(1)
    
    results = load_results(args.results_file)
    metrics = extract_metrics(results)
    
    output_file = args.output if args.output else args.results_file.parent / "correlation_analysis.md"
    generate_correlation_report(metrics, output_file)


if __name__ == "__main__":
    main()

