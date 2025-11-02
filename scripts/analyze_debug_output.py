#!/usr/bin/env python3
"""
Analyze debug output from H¹ computation pipeline.
Visualizes scope trees, Čech complex structure, and overlap detection results.
"""

import json
import sys
from pathlib import Path
from typing import Dict, Any, List
import argparse


def load_debug_result(filepath: Path) -> Dict[str, Any]:
    """Load debug result from JSON file."""
    with open(filepath, 'r') as f:
        return json.load(f)


def visualize_scope_tree(scope_tree: Dict[str, Any], indent: int = 0) -> str:
    """Visualize scope tree structure."""
    lines = []
    root = scope_tree.get('root', 'unknown')
    nodes = scope_tree.get('nodes', {})
    
    def format_node(node_id: str, node_data: Dict, level: int = 0) -> List[str]:
        indent_str = "  " * level
        node_info = [
            f"{indent_str}{node_id}",
            f"{indent_str}  depth: {node_data.get('depth', '?')}",
            f"{indent_str}  bindings: {node_data.get('bindings', [])}",
        ]
        children = node_data.get('children', [])
        if children:
            node_info.append(f"{indent_str}  children: {children}")
        return node_info
    
    lines.append(f"Root: {root}")
    lines.append("")
    
    # Start from root
    if root in nodes:
        lines.extend(format_node(root, nodes[root], 0))
        lines.append("")
        
        # Process children recursively
        def process_children(parent_id: str, level: int = 1):
            if parent_id in nodes:
                parent_data = nodes[parent_id]
                for child_id in parent_data.get('children', []):
                    if child_id in nodes:
                        lines.extend(format_node(child_id, nodes[child_id], level))
                        lines.append("")
                        process_children(child_id, level + 1)
        
        process_children(root, 1)
    
    return "\n".join(lines)


def analyze_overlap_detection(overlap_log: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Analyze overlap detection results."""
    pairs_overlap = sum(1 for entry in overlap_log if entry.get('type') == 'pair' and entry.get('overlaps', False))
    pairs_total = sum(1 for entry in overlap_log if entry.get('type') == 'pair')
    
    triples_all_overlap = sum(1 for entry in overlap_log 
                             if entry.get('type') == 'triple' and entry.get('all-pairs-overlap', False))
    triples_total = sum(1 for entry in overlap_log if entry.get('type') == 'triple')
    
    # Find critical triples (all pairs overlap but triple doesn't form simplex)
    critical_triples = [
        entry for entry in overlap_log
        if entry.get('type') == 'triple'
        and entry.get('all-pairs-overlap', False)
    ]
    
    return {
        'pair_overlap_count': pairs_overlap,
        'pair_total_count': pairs_total,
        'pair_overlap_ratio': pairs_overlap / pairs_total if pairs_total > 0 else 0,
        'triple_all_overlap_count': triples_all_overlap,
        'triple_total_count': triples_total,
        'critical_triples': critical_triples,
        'critical_triple_count': len(critical_triples)
    }


def visualize_cech_complex(complex_data: Dict[str, Any]) -> str:
    """Visualize Čech complex structure."""
    lines = [
        "Čech Complex Structure:",
        "=" * 50,
        f"0-simplices (vertices): {complex_data.get('count0', 0)}",
        f"1-simplices (edges): {complex_data.get('count1', 0)}",
        f"2-simplices (triangles): {complex_data.get('count2', 0)}",
        "",
    ]
    
    simplices1 = complex_data.get('simplices1', [])
    if simplices1:
        lines.append("1-simplices (edges):")
        for edge in simplices1[:20]:  # Limit to first 20
            lines.append(f"  {edge}")
        if len(simplices1) > 20:
            lines.append(f"  ... and {len(simplices1) - 20} more")
        lines.append("")
    
    simplices2 = complex_data.get('simplices2', [])
    if simplices2:
        lines.append("2-simplices (triangles):")
        for triangle in simplices2[:10]:  # Limit to first 10
            lines.append(f"  {triangle}")
        if len(simplices2) > 10:
            lines.append(f"  ... and {len(simplices2) - 10} more")
        lines.append("")
    
    return "\n".join(lines)


def generate_diagnostic_report(debug_result: Dict[str, Any], program_id: str = "unknown") -> str:
    """Generate comprehensive diagnostic report."""
    lines = [
        f"Diagnostic Report for Program: {program_id}",
        "=" * 70,
        "",
        "Summary Statistics:",
        "-" * 70,
        f"H¹ (cohomology): {debug_result.get('h1-value', 'N/A')}",
        f"β₀ (connected components): {debug_result.get('beta0', 'N/A')}",
        f"β₁ (from graph): {debug_result.get('beta1', 'N/A')}",
        f"Number of bindings: {debug_result.get('num-bindings', 'N/A')}",
        f"Simplices (0, 1, 2): ({debug_result.get('num-simplices0', 0)}, "
        f"{debug_result.get('num-simplices1', 0)}, {debug_result.get('num-simplices2', 0)})",
        "",
    ]
    
    # Validate H¹ = β₁
    h1 = debug_result.get('h1-value', 0)
    beta1 = debug_result.get('beta1', 0)
    if h1 != beta1:
        lines.append(f"⚠️  WARNING: H¹ ({h1}) does not match β₁ ({beta1})")
        lines.append("")
    else:
        lines.append(f"✓ Validation: H¹ = β₁ = {h1}")
        lines.append("")
    
    # Scope tree visualization
    scope_tree = debug_result.get('scope-tree-json')
    if scope_tree:
        lines.append("Scope Tree Structure:")
        lines.append("-" * 70)
        lines.append(visualize_scope_tree(scope_tree))
        lines.append("")
    
    # Čech complex visualization
    cech_complex = debug_result.get('cech-complex-structure')
    if cech_complex:
        lines.append(visualize_cech_complex(cech_complex))
        lines.append("")
    
    # Overlap detection analysis
    overlap_log = debug_result.get('overlap-detection-log', [])
    if overlap_log:
        lines.append("Overlap Detection Analysis:")
        lines.append("-" * 70)
        analysis = analyze_overlap_detection(overlap_log)
        lines.append(f"Pairs with overlap: {analysis['pair_overlap_count']} / {analysis['pair_total_count']} "
                    f"({analysis['pair_overlap_ratio']:.2%})")
        lines.append(f"Triples with all pairs overlapping: {analysis['triple_all_overlap_count']} / "
                    f"{analysis['triple_total_count']}")
        lines.append(f"Critical triples (potential 1-cycles): {analysis['critical_triple_count']}")
        lines.append("")
    
    # Visibility regions summary
    visibility_regions = debug_result.get('visibility-regions')
    if visibility_regions:
        lines.append(f"Visibility Regions: {len(visibility_regions)} bindings tracked")
        lines.append("")
    
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Analyze debug output from H¹ computation pipeline"
    )
    parser.add_argument(
        "debug_file",
        type=Path,
        help="Path to debug result JSON file"
    )
    parser.add_argument(
        "--program-id",
        type=str,
        default="unknown",
        help="Program identifier for report header"
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output file path (default: stdout)"
    )
    
    args = parser.parse_args()
    
    if not args.debug_file.exists():
        print(f"Error: File not found: {args.debug_file}", file=sys.stderr)
        sys.exit(1)
    
    debug_result = load_debug_result(args.debug_file)
    report = generate_diagnostic_report(debug_result, args.program_id)
    
    if args.output:
        with open(args.output, 'w') as f:
            f.write(report)
        print(f"Report written to {args.output}")
    else:
        print(report)


if __name__ == "__main__":
    main()

