#!/usr/bin/env python3
"""
Statistical re-analysis of expanded corpus.
Computes updated correlations, applies TDA techniques, and generates validation report.
"""

import json
import sys
from pathlib import Path
from typing import Dict, List, Tuple
import argparse
import numpy as np
from collections import defaultdict

# Try to import scipy for advanced statistics (optional)
try:
    from scipy import stats
    from scipy.stats import pearsonr, spearmanr
    HAS_SCIPY = True
except ImportError:
    HAS_SCIPY = False
    print("Warning: scipy not available. Using basic statistics only.")


def load_validation_results(json_file: Path) -> Dict:
    """Load validation results from JSON file."""
    with open(json_file, 'r') as f:
        return json.load(f)


def compute_statistics(results: List[Dict]) -> Dict:
    """Compute comprehensive statistics on H¹ and V(G)."""
    h1_values = [r.get('h1', 0) for r in results if r.get('success', False)]
    vg_values = [r.get('vg', 0) for r in results if r.get('success', False)]
    
    stats_dict = {
        'total_programs': len(results),
        'successful_computations': len([r for r in results if r.get('success', False)]),
        'h1': {
            'values': h1_values,
            'min': min(h1_values) if h1_values else 0,
            'max': max(h1_values) if h1_values else 0,
            'mean': np.mean(h1_values) if h1_values else 0,
            'median': np.median(h1_values) if h1_values else 0,
            'std': np.std(h1_values) if h1_values else 0,
            'unique_values': sorted(set(h1_values)),
            'distribution': {val: h1_values.count(val) for val in set(h1_values)}
        },
        'vg': {
            'values': vg_values,
            'min': min(vg_values) if vg_values else 0,
            'max': max(vg_values) if vg_values else 0,
            'mean': np.mean(vg_values) if vg_values else 0,
            'median': np.median(vg_values) if vg_values else 0,
            'std': np.std(vg_values) if vg_values else 0
        }
    }
    
    # Compute correlation
    if len(h1_values) > 1 and len(vg_values) > 1:
        if HAS_SCIPY:
            pearson_r, pearson_p = pearsonr(h1_values, vg_values)
            spearman_r, spearman_p = spearmanr(h1_values, vg_values)
            stats_dict['correlation'] = {
                'pearson': {'r': pearson_r, 'p_value': pearson_p},
                'spearman': {'r': spearman_r, 'p_value': spearman_p}
            }
        else:
            # Basic correlation using numpy
            correlation_matrix = np.corrcoef(h1_values, vg_values)
            stats_dict['correlation'] = {
                'pearson': {'r': correlation_matrix[0, 1], 'p_value': None},
                'spearman': {'r': None, 'p_value': None}
            }
    
    # Count H¹=0 vs H¹>0
    h1_zero_count = sum(1 for h in h1_values if h == 0)
    h1_positive_count = len(h1_values) - h1_zero_count
    stats_dict['h1']['zero_count'] = h1_zero_count
    stats_dict['h1']['positive_count'] = h1_positive_count
    stats_dict['h1']['zero_percentage'] = (h1_zero_count / len(h1_values) * 100) if h1_values else 0
    
    return stats_dict


def analyze_by_category(results: List[Dict]) -> Dict:
    """Analyze results by program category."""
    by_category = defaultdict(list)
    
    for result in results:
        if result.get('success', False):
            program_id = result.get('program_id', 'unknown')
            # Extract category from path (e.g., "baseline/B002" -> "baseline")
            category = 'unknown'
            if '/' in program_id:
                category = program_id.split('/')[0]
            elif '_' in program_id:
                category = program_id.split('_')[0]
            
            by_category[category].append(result)
    
    category_stats = {}
    for category, category_results in by_category.items():
        category_stats[category] = compute_statistics(category_results)
        category_stats[category]['program_count'] = len(category_results)
    
    return category_stats


def test_scale_mismatch_resolution(stats: Dict) -> Dict:
    """Test if Scale Mismatch is resolved (H¹ range spans ≥4)."""
    h1_max = stats['h1']['max']
    h1_range = stats['h1']['unique_values']
    h1_variance = stats['h1']['std']
    
    return {
        'max_h1_achieved': h1_max,
        'h1_range': h1_range,
        'range_span': max(h1_range) - min(h1_range) if len(h1_range) > 0 else 0,
        'variance': h1_variance,
        'scale_mismatch_resolved': h1_max >= 4,
        'sufficient_variance': h1_variance > 0.5  # Arbitrary threshold
    }


def generate_validation_report(stats: Dict, category_stats: Dict, scale_test: Dict, 
                               output_file: Path = None):
    """Generate comprehensive validation report."""
    lines = [
        "# Expanded Corpus Validation Report",
        "",
        "**Date**: Generated automatically",
        "",
        "---",
        "",
        "## Summary Statistics",
        "",
        f"- **Total Programs**: {stats['total_programs']}",
        f"- **Successful Computations**: {stats['successful_computations']}",
        "",
        "### H¹ Distribution",
        "",
        f"- **Range**: {stats['h1']['min']} to {stats['h1']['max']}",
        f"- **Mean**: {stats['h1']['mean']:.3f}",
        f"- **Median**: {stats['h1']['median']:.0f}",
        f"- **Std Dev**: {stats['h1']['std']:.3f}",
        f"- **Unique Values**: {stats['h1']['unique_values']}",
        f"- **H¹ = 0**: {stats['h1']['zero_count']} programs ({stats['h1']['zero_percentage']:.1f}%)",
        f"- **H¹ > 0**: {stats['h1']['positive_count']} programs",
        "",
        "### V(G) Distribution",
        "",
        f"- **Range**: {stats['vg']['min']} to {stats['vg']['max']}",
        f"- **Mean**: {stats['vg']['mean']:.3f}",
        f"- **Median**: {stats['vg']['median']:.0f}",
        f"- **Std Dev**: {stats['vg']['std']:.3f}",
        "",
        "### Correlation Analysis",
        "",
    ]
    
    if 'correlation' in stats:
        corr = stats['correlation']
        if corr.get('pearson'):
            pearson = corr['pearson']
            lines.append(f"- **Pearson r**: {pearson['r']:.4f}")
            if pearson.get('p_value') is not None:
                lines.append(f"- **Pearson p-value**: {pearson['p_value']:.4f}")
        
        if corr.get('spearman') and corr['spearman'].get('r') is not None:
            spearman = corr['spearman']
            lines.append(f"- **Spearman ρ**: {spearman['r']:.4f}")
            if spearman.get('p_value') is not None:
                lines.append(f"- **Spearman p-value**: {spearman['p_value']:.4f}")
    
    lines.extend([
        "",
        "---",
        "",
        "## Scale Mismatch Resolution Test",
        "",
        f"- **Maximum H¹ Achieved**: {scale_test['max_h1_achieved']}",
        f"- **H¹ Range Span**: {scale_test['range_span']}",
        f"- **H¹ Variance**: {scale_test['variance']:.3f}",
        f"- **Scale Mismatch Resolved**: {'✅ YES' if scale_test['scale_mismatch_resolved'] else '❌ NO'}",
        f"- **Sufficient Variance**: {'✅ YES' if scale_test['sufficient_variance'] else '❌ NO'}",
        "",
        "---",
        "",
        "## Category Analysis",
        "",
    ])
    
    for category, cat_stats in sorted(category_stats.items()):
        lines.extend([
            f"### {category.title()}",
            "",
            f"- **Programs**: {cat_stats['program_count']}",
            f"- **H¹ Mean**: {cat_stats['h1']['mean']:.3f}",
            f"- **H¹ Max**: {cat_stats['h1']['max']}",
            f"- **V(G) Mean**: {cat_stats['vg']['mean']:.3f}",
            "",
        ])
    
    lines.extend([
        "---",
        "",
        "## Hypothesis Testing: H¹ = V(G) - k",
        "",
    ])
    
    # Compute k statistics if possible
    if stats['successful_computations'] > 0:
        results_list = []  # Would need to pass actual results
        # For now, provide template
        lines.append("**Note**: Detailed k analysis requires full results data.")
        lines.append("")
    
    lines.extend([
        "## Conclusion",
        "",
        f"The expanded corpus contains {stats['total_programs']} programs with H¹ ranging from "
        f"{stats['h1']['min']} to {stats['h1']['max']}.",
        "",
        f"**Status**: {'Ready for Phase 3 analysis' if scale_test['scale_mismatch_resolved'] else 'May need additional corpus expansion'}",
        "",
        "---",
        "",
        "**Generated**: Automatically",
        f"**Data Source**: Validation results JSON",
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
        description="Analyze expanded corpus and generate validation report"
    )
    parser.add_argument(
        "results_file",
        type=Path,
        help="Path to validation results JSON file"
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output file path for report (default: stdout or results_file.parent/expanded_validation_report.md)"
    )
    parser.add_argument(
        "--json-output",
        type=Path,
        help="Also output statistics as JSON"
    )
    
    args = parser.parse_args()
    
    if not args.results_file.exists():
        print(f"Error: File not found: {args.results_file}", file=sys.stderr)
        sys.exit(1)
    
    # Load results
    data = load_validation_results(args.results_file)
    
    # Extract results list
    if isinstance(data, dict) and 'results' in data:
        results = data['results']
    elif isinstance(data, list):
        results = data
    else:
        print("Error: Unexpected JSON structure", file=sys.stderr)
        sys.exit(1)
    
    # Compute statistics
    stats = compute_statistics(results)
    category_stats = analyze_by_category(results)
    scale_test = test_scale_mismatch_resolution(stats)
    
    # Determine output file
    output_file = args.output
    if output_file is None:
        output_file = args.results_file.parent / "expanded_validation_report.md"
    
    # Generate report
    report = generate_validation_report(stats, category_stats, scale_test, output_file)
    
    # Output JSON statistics if requested
    if args.json_output:
        json_stats = {
            'overall': stats,
            'by_category': category_stats,
            'scale_mismatch_test': scale_test
        }
        with open(args.json_output, 'w') as f:
            json.dump(json_stats, f, indent=2, default=str)
        print(f"JSON statistics written to {args.json_output}")


if __name__ == "__main__":
    main()

