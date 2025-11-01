#!/usr/bin/env python3
"""
H¹ Computation Diagnostic Tool

Analyzes the H¹ computation pipeline to understand why programs
return H¹ = 0 or H¹ > 0.
"""

import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent
sys.path.insert(0, str(PROJECT_ROOT / "python-coordinator"))

from coordinator.direct_compute import compute_h1_direct
import subprocess
import tempfile
import os


def diagnose_h1(source_code, program_id):
    """Diagnose H¹ computation step by step"""
    print(f"\n{'='*80}")
    print(f"Diagnosing H¹ for: {program_id}")
    print(f"{'='*80}\n")
    
    # Write source to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.scm', delete=False) as f:
        f.write(source_code)
        temp_file = f.name
    
    try:
        racket_unified_dir = PROJECT_ROOT / "racket-unified" / "src"
        script_file = racket_unified_dir / "diagnose_h1_temp.rkt"
        
        with open(script_file, 'w') as f:
            f.write(f"""#lang racket/base
(require racket/file
         racket/set
         "algorithms/algorithm1.rkt"
         "algorithms/algorithm2.rkt"
         "algorithms/algorithm3.rkt"
         (only-in "algorithms/unified-pipeline.rkt" r-scheme compute-topology-enhanced)
         (except-in "algorithms/algorithm4.rkt" compute-h1-from-source))

(define source (file->string "{temp_file}"))

(printf "=== H¹ DIAGNOSTIC ===\\n\\n")

;; Step 1: Parse and extract bindings
(define ast (sexpr->ast (read (open-input-string source))))
(define alpha-ast (alpha-convert ast))
(define bindings (extract-bindings alpha-ast))

(printf "Step 1: Bindings extracted\\n")
(printf "  Number of bindings: ~a\\n" (set-count bindings))
(printf "  Bindings: ~a\\n\\n" (set->list bindings))

;; Step 2: Scope analysis
(define-values (scope-map scope-tree) (analyze-scopes-enhanced alpha-ast))
(printf "Step 2: Scope analysis\\n")
(printf "  Scope map keys: ~a\\n" (if (hash? scope-map) (hash-count scope-map) "N/A"))
(if (hash? scope-map)
    (for ([(k v) (in-hash scope-map)])
      (printf "    ~a: ~a\\n" k v))
    (printf "    Scope map: ~a\\n" scope-map))
(printf "\\n")

;; Step 3: Build topology
(require "algorithms/unified-pipeline.rkt")
(define r-scheme-rig (r-scheme bindings))
(define topology (compute-topology-enhanced r-scheme-rig scope-map scope-tree))
(printf "Step 3: Topology\\n")
(printf "  Open sets: ~a\\n" (if (hash? topology) (hash-ref topology 'open-sets #f) topology))
(printf "\\n")

;; Step 4: Build Čech complex
(define complex (build-cech-complex topology))
(printf "Step 4: Čech Complex\\n")
(printf "  0-simplices (vertices): ~a\\n" (set-count (simplicial-complex-simplices0 complex)))
(printf "  1-simplices (edges): ~a\\n" (set-count (simplicial-complex-simplices1 complex)))
(printf "  2-simplices (triangles): ~a\\n" (set-count (simplicial-complex-simplices2 complex)))
(printf "\\n")

;; Step 5: Compute H¹
(require "algorithms/algorithm4.rkt")
(define h1 (compute-h1 complex))
(printf "Step 5: H¹ Computation\\n")
(printf "  H¹ = ~a\\n\\n" h1)

(printf "=== END DIAGNOSTIC ===\\n")
""")
        
        # Run diagnostic script
        result = subprocess.run(
            ["racket", str(script_file.resolve())],
            cwd=str(racket_unified_dir.resolve()),
            capture_output=True,
            text=True,
            timeout=30
        )
        
        if result.returncode == 0:
            print(result.stdout)
            if result.stderr:
                print("Warnings/Errors:")
                print(result.stderr)
        else:
            print("Error running diagnostic:")
            print(result.stderr)
        
        # Cleanup
        if script_file.exists():
            os.unlink(script_file)
        
    finally:
        if os.path.exists(temp_file):
            os.unlink(temp_file)


def main():
    """Run diagnostics on test programs"""
    
    # Test programs
    test_programs = [
        ("Simple lambda", "(lambda (x) x)"),
        ("B002 - Has let", "(define (compute x y)\n  (let ((a (+ x y))\n        (b (* x y)))\n    (/ a b)))"),
        ("F009 - Higher-order", "(define (flip f)\n  (lambda (x y) (f y x)))"),
        ("Letrec example", "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))\n           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))\n  (even? 5))"),
    ]
    
    for name, source in test_programs:
        diagnose_h1(source, name)
    
    print(f"\n{'='*80}")
    print("Diagnostic Complete")
    print(f"{'='*80}\n")


if __name__ == "__main__":
    main()

