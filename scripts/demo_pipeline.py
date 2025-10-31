#!/usr/bin/env python3
"""
Demonstration of the Computational Scheme Theory Validation Pipeline

Shows the complete flow from Scheme source code to hypothesis validation,
even when services are running in placeholder mode.
"""

import sys
from pathlib import Path

# Add python-coordinator to path
sys.path.insert(0, str(Path(__file__).parent.parent / "python-coordinator"))

from coordinator.validation import HypothesisValidator
from coordinator.service import ValidationCoordinator
import logging

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')


def demonstrate_single_program():
    """Demonstrate validation of a single program"""
    print("\n" + "="*70)
    print("COMPUTATIONAL SCHEME THEORY - PIPELINE DEMONSTRATION")
    print("="*70)
    
    # Example program
    program_id = "demo-001"
    source_code = """
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
"""
    
    print(f"\n📝 Program: {program_id}")
    print(f"Source Code:")
    print(source_code)
    
    print("\n🔧 Pipeline Steps:")
    print("  1. Parse Scheme source → AST")
    print("  2. Compute H¹(X_Comp, O_Comp) [Haskell service]")
    print("  3. Compute V(G) cyclomatic complexity [Racket service]")
    print("  4. Test hypothesis: H¹ = V(G) - k")
    
    print("\n⚙️  Running validation...")
    
    # Initialize coordinator
    coordinator = ValidationCoordinator()
    
    # Validate
    result = coordinator.validate_program(program_id, source_code.strip())
    
    print("\n📊 Results:")
    print(f"  Program ID: {result.program_id}")
    print(f"  H¹ (cohomology): {result.h1}")
    print(f"  V(G) (cyclomatic): {result.vg}")
    print(f"  k (normalization): {result.k}")
    print(f"  Difference: |H¹ - (V(G) - k)| = {result.difference}")
    print(f"  Hypothesis holds: {'✅ YES' if result.hypothesis_holds else '❌ NO'}")
    print(f"  Success: {result.success}")
    
    if result.error:
        print(f"  Error: {result.error}")
    
    if result.h1 == 0 and result.vg == 0:
        print("\n⚠️  Note: Currently running in placeholder mode.")
        print("   Real values will be computed when services are connected:")
        print("   - Start Racket service: racket metrics-api.rkt")
        print("   - Start Haskell service: cabal run computational-scheme-theory")
    
    return result


def demonstrate_multiple_programs():
    """Demonstrate validation of multiple programs"""
    print("\n" + "="*70)
    print("VALIDATING MULTIPLE PROGRAMS")
    print("="*70)
    
    programs = [
        {
            "program_id": "demo-simple",
            "source_code": "(define x 42)"
        },
        {
            "program_id": "demo-if",
            "source_code": "(if (> x 0) 1 -1)"
        },
        {
            "program_id": "demo-factorial",
            "source_code": "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
        }
    ]
    
    coordinator = ValidationCoordinator()
    
    results = []
    for program in programs:
        print(f"\nValidating: {program['program_id']}")
        result = coordinator.validate_program(
            program["program_id"],
            program["source_code"]
        )
        results.append(result)
        
        status = "✓" if result.hypothesis_holds else "✗"
        print(f"  {status} H¹={result.h1}, V(G)={result.vg}, k={result.k}")
    
    # Statistics
    from coordinator.validation import compute_statistics
    stats = compute_statistics(results)
    
    print("\n" + "-"*70)
    print("Summary Statistics:")
    print(f"  Total: {stats['total']}")
    print(f"  Hypothesis holds: {stats['hypothesis_holds_count']}")
    print(f"  Hypothesis fails: {stats['hypothesis_fails_count']}")
    print(f"  Success rate: {stats['hypothesis_success_rate']:.1%}")
    print(f"  Correlation: {stats.get('correlation', 0):.3f}")


def show_pipeline_overview():
    """Show overview of the complete pipeline"""
    print("\n" + "="*70)
    print("COMPLETE PIPELINE ARCHITECTURE")
    print("="*70)
    
    print("""
┌─────────────────────────────────────────────────────────────────┐
│                    Scheme Program Source                        │
│                    "(define (fact n) ...)"                       │
└─────────────────────┬───────────────────────────────────────────┘
                      │
          ┌───────────┴───────────┐
          │                       │
          ▼                       ▼
    ┌──────────┐          ┌──────────┐
    │  Haskell │          │  Racket  │
    │   Core   │          │ Metrics  │
    └────┬─────┘          └────┬─────┘
         │                     │
         │ Algorithm 1-4       │ CFG Builder
         │ Parse → Rig →       │ AST → CFG
         │ Topology → Complex  │
         │ → H¹                │ → V(G)
         │                     │
         └──────────┬──────────┘
                    │
                    ▼
            ┌───────────────┐
            │   Python      │
            │ Coordinator   │
            └───────┬───────┘
                    │
                    ▼
            ┌───────────────┐
            │  Validation   │
            │  H¹ = V(G)-k? │
            └───────────────┘
                    │
                    ▼
            ┌───────────────┐
            │    Results    │
            │   Statistics  │
            └───────────────┘
    """)


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Demonstrate the validation pipeline")
    parser.add_argument(
        "--demo",
        choices=["single", "multiple", "overview", "all"],
        default="all",
        help="Which demonstration to run"
    )
    
    args = parser.parse_args()
    
    if args.demo in ["overview", "all"]:
        show_pipeline_overview()
    
    if args.demo in ["single", "all"]:
        demonstrate_single_program()
    
    if args.demo in ["multiple", "all"]:
        demonstrate_multiple_programs()
    
    print("\n" + "="*70)
    print("✅ Demonstration complete!")
    print("="*70)
    print("\nNext steps:")
    print("  1. Set up Haskell and Racket services")
    print("  2. Run full validation: python3 scripts/run_validation.py")
    print("  3. Analyze results for hypothesis validation")


