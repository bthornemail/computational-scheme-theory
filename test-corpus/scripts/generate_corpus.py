#!/usr/bin/env python3
"""
Generate test corpus for Computational Scheme Theory validation

Generates R5RS Scheme programs across different categories for empirical validation.
"""

import json
import os
from pathlib import Path
from typing import Dict, List


# Corpus generation functions

def generate_baseline_programs() -> List[Dict]:
    """Generate baseline programs (straight-line code, H¹ ≈ 0-1)"""
    programs = [
        {
            "program_id": "baseline-001",
            "source_code": "(define x 42)",
            "description": "Simple variable definition",
            "expected_h1": 0,
            "expected_vg": 1,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "baseline-002",
            "source_code": "(define pi 3.14159)",
            "description": "Constant definition",
            "expected_h1": 0,
            "expected_vg": 1,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "baseline-003",
            "source_code": "(define (id x) x)",
            "description": "Identity function",
            "expected_h1": 0,
            "expected_vg": 1,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "baseline-004",
            "source_code": "(define (add x y) (+ x y))",
            "description": "Simple addition function",
            "expected_h1": 0,
            "expected_vg": 1,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "baseline-005",
            "source_code": "(begin (define x 1) (define y 2) (+ x y))",
            "description": "Sequence of definitions",
            "expected_h1": 0,
            "expected_vg": 1,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        }
    ]
    return programs


def generate_simple_control_programs() -> List[Dict]:
    """Generate simple control flow programs (single if/loop, H¹ ≈ 2)"""
    programs = [
        {
            "program_id": "simple-control-001",
            "source_code": "(if (> x 0) 1 -1)",
            "description": "Simple if statement",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "simple-control-002",
            "source_code": "(define (abs x) (if (< x 0) (- x) x))",
            "description": "Absolute value function",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "simple-control-003",
            "source_code": "(define (max a b) (if (> a b) a b))",
            "description": "Maximum function",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "simple-control-004",
            "source_code": "(cond [(< x 0) 'negative] [(= x 0) 'zero] [else 'positive])",
            "description": "Cond with multiple clauses",
            "expected_h1": 4,
            "expected_vg": 4,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        },
        {
            "program_id": "simple-control-005",
            "source_code": "(define (sign x) (if (> x 0) 1 (if (< x 0) -1 0)))",
            "description": "Nested if statements",
            "expected_h1": 3,
            "expected_vg": 3,
            "features": {"recursion": False, "call_cc": False, "macros": False}
        }
    ]
    return programs


def generate_recursion_programs() -> List[Dict]:
    """Generate recursive programs"""
    programs = [
        {
            "program_id": "recursion-001",
            "source_code": "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))",
            "description": "Factorial function",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": True, "call_cc": False, "macros": False}
        },
        {
            "program_id": "recursion-002",
            "source_code": "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))",
            "description": "Fibonacci function",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": True, "call_cc": False, "macros": False}
        },
        {
            "program_id": "recursion-003",
            "source_code": "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1)))))",
            "description": "Sum from 1 to n",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": True, "call_cc": False, "macros": False}
        },
        {
            "program_id": "recursion-004",
            "source_code": "(define (length lst) (if (null? lst) 0 (+ 1 (length (cdr lst)))))",
            "description": "List length function",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": True, "call_cc": False, "macros": False}
        },
        {
            "program_id": "recursion-005",
            "source_code": "(define (sum-tail n acc) (if (= n 0) acc (sum-tail (- n 1) (+ acc n))))",
            "description": "Tail-recursive sum",
            "expected_h1": 2,
            "expected_vg": 2,
            "features": {"recursion": True, "call_cc": False, "macros": False}
        }
    ]
    return programs


def save_program(category: str, program: Dict, corpus_dir: Path):
    """Save a program and its metadata to the corpus"""
    category_dir = corpus_dir / category
    category_dir.mkdir(parents=True, exist_ok=True)
    
    # Save source code
    source_file = category_dir / f"{program['program_id']}.scm"
    source_file.write_text(program['source_code'])
    
    # Save metadata
    metadata_file = category_dir / f"{program['program_id']}.json"
    metadata = {
        "program_id": program["program_id"],
        "category": category,
        "description": program["description"],
        "expected_h1": program.get("expected_h1", None),
        "expected_vg": program.get("expected_vg", None),
        "features": program.get("features", {}),
        "source": "synthetic"
    }
    metadata_file.write_text(json.dumps(metadata, indent=2))


def generate_corpus(corpus_dir: Path, target_sizes: Dict[str, int] = None):
    """
    Generate test corpus
    
    Args:
        corpus_dir: Directory to save corpus
        target_sizes: Target number of programs per category
    """
    if target_sizes is None:
        target_sizes = {
            "baseline": 10,
            "simple-control": 20,
            "recursion": 20
        }
    
    print(f"Generating test corpus in {corpus_dir}")
    
    # Generate programs
    all_baseline = generate_baseline_programs()
    all_simple = generate_simple_control_programs()
    all_recursion = generate_recursion_programs()
    
    # Save programs (up to target sizes)
    for i, program in enumerate(all_baseline[:target_sizes.get("baseline", 10)]):
        save_program("baseline", program, corpus_dir)
    
    for i, program in enumerate(all_simple[:target_sizes.get("simple-control", 20)]):
        save_program("simple-control", program, corpus_dir)
    
    for i, program in enumerate(all_recursion[:target_sizes.get("recursion", 20)]):
        save_program("recursion", program, corpus_dir)
    
    print(f"Generated {len(all_baseline[:target_sizes.get('baseline', 10)]) + len(all_simple[:target_sizes.get('simple-control', 20)]) + len(all_recursion[:target_sizes.get('recursion', 20)])} programs")


if __name__ == "__main__":
    import sys
    
    corpus_dir = Path(__file__).parent.parent
    if len(sys.argv) > 1:
        corpus_dir = Path(sys.argv[1])
    
    generate_corpus(corpus_dir)

