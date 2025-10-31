#!/usr/bin/env python3
"""
Run validation experiments

Orchestrates validation of programs against the hypothesis:
    H¹(X_Comp, O_Comp) = V(G) - k
"""

import json
import sys
import time
from pathlib import Path
from typing import List, Dict
import logging

# Add python-coordinator to path
sys.path.insert(0, str(Path(__file__).parent.parent / "python-coordinator"))

from coordinator.validation import (
    ValidationResult,
    HypothesisValidator,
    compute_statistics,
    compute_correlation
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def load_program(corpus_dir: Path, category: str, program_id: str) -> Dict:
    """Load a program from the corpus"""
    program_dir = corpus_dir / category
    source_file = program_dir / f"{program_id}.scm"
    metadata_file = program_dir / f"{program_id}.json"
    
    if not source_file.exists():
        raise FileNotFoundError(f"Source file not found: {source_file}")
    
    source_code = source_file.read_text()
    metadata = json.loads(metadata_file.read_text()) if metadata_file.exists() else {}
    
    return {
        "program_id": program_id,
        "source_code": source_code,
        "category": category,
        "metadata": metadata
    }


def load_corpus(corpus_dir: Path, categories: List[str] = None) -> List[Dict]:
    """Load all programs from corpus"""
    if categories is None:
        categories = ["baseline", "simple-control", "recursion"]
    
    programs = []
    
    for category in categories:
        category_dir = corpus_dir / category
        if not category_dir.exists():
            continue
        
        for source_file in category_dir.glob("*.scm"):
            program_id = source_file.stem
            try:
                program = load_program(corpus_dir, category, program_id)
                programs.append(program)
            except Exception as e:
                logger.warning(f"Failed to load {category}/{program_id}: {e}")
    
    return programs


def run_validation(programs: List[Dict], coordinator) -> List[ValidationResult]:
    """
    Run validation on a list of programs
    
    Args:
        programs: List of program dictionaries
        coordinator: ValidationCoordinator instance
        
    Returns:
        List of ValidationResult
    """
    results = []
    
    logger.info(f"Validating {len(programs)} programs...")
    
    for i, program in enumerate(programs, 1):
        logger.info(f"[{i}/{len(programs)}] Validating {program['program_id']}...")
        
        try:
            result = coordinator.validate_program(
                program["program_id"],
                program["source_code"]
            )
            results.append(result)
            
            if result.success:
                status = "✓" if result.hypothesis_holds else "✗"
                logger.info(
                    f"  {status} H¹={result.h1}, V(G)={result.vg}, "
                    f"k={result.k}, diff={result.difference}"
                )
            else:
                logger.error(f"  Failed: {result.error}")
                
        except Exception as e:
            logger.error(f"  Error validating {program['program_id']}: {e}")
            results.append(ValidationResult(
                program_id=program["program_id"],
                h1=0,
                vg=0,
                k=0,
                difference=0,
                hypothesis_holds=False,
                success=False,
                error=str(e)
            ))
    
    return results


def print_summary(results: List[ValidationResult]):
    """Print validation summary"""
    stats = compute_statistics(results)
    
    print("\n" + "="*60)
    print("VALIDATION SUMMARY")
    print("="*60)
    print(f"Total programs: {stats['total']}")
    print(f"Succeeded: {stats['succeeded']}")
    print(f"Failed: {stats['failed']}")
    print()
    print("Hypothesis Validation:")
    print(f"  Holds: {stats['hypothesis_holds_count']}")
    print(f"  Fails: {stats['hypothesis_fails_count']}")
    print(f"  Success rate: {stats['hypothesis_success_rate']:.2%}")
    print()
    print(f"Mean difference: {stats.get('mean_difference', 0):.2f}")
    print(f"Correlation (H¹, V(G)): {stats.get('correlation', 0):.3f}")
    print("="*60)


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Run validation experiments")
    parser.add_argument(
        "--corpus",
        type=Path,
        default=Path(__file__).parent.parent / "test-corpus",
        help="Path to test corpus directory"
    )
    parser.add_argument(
        "--categories",
        nargs="+",
        default=["baseline", "simple-control", "recursion"],
        help="Categories to validate"
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output file for results (JSON)"
    )
    
    args = parser.parse_args()
    
    # Load programs
    programs = load_corpus(args.corpus, args.categories)
    
    if not programs:
        logger.error(f"No programs found in {args.corpus}")
        sys.exit(1)
    
    logger.info(f"Loaded {len(programs)} programs")
    
    # Initialize coordinator
    # Try direct computation first, fall back to service coordinator
    try:
        from coordinator.direct_compute import DirectComputeCoordinator
        coordinator = DirectComputeCoordinator()
        logger.info("Using direct computation (Haskell/Racket executables)")
    except Exception as e:
        logger.info("Direct computation not available, using service coordinator")
        from coordinator.service import ValidationCoordinator
        coordinator = ValidationCoordinator()
    
    # Run validation
    results = run_validation(programs, coordinator)
    
    # Print summary
    print_summary(results)
    
    # Save results if output specified
    if args.output:
        output_data = {
            "results": [
                {
                    "program_id": r.program_id,
                    "h1": r.h1,
                    "vg": r.vg,
                    "k": r.k,
                    "difference": r.difference,
                    "hypothesis_holds": r.hypothesis_holds,
                    "success": r.success,
                    "error": r.error
                }
                for r in results
            ],
            "statistics": compute_statistics(results)
        }
        args.output.write_text(json.dumps(output_data, indent=2))
        logger.info(f"Results saved to {args.output}")


if __name__ == "__main__":
    main()

