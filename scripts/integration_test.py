#!/usr/bin/env python3
"""
Integration Test - End-to-End Pipeline Test

Tests the complete validation pipeline even when services are in placeholder mode.
"""

import sys
from pathlib import Path

# Add python-coordinator to path
sys.path.insert(0, str(Path(__file__).parent.parent / "python-coordinator"))

from coordinator.service import ValidationCoordinator
from coordinator.validation import HypothesisValidator, compute_statistics
import logging

logging.basicConfig(level=logging.WARNING)  # Reduce noise for test


def test_single_program():
    """Test validation of a single program"""
    print("Test 1: Single Program Validation")
    print("-" * 50)
    
    coordinator = ValidationCoordinator()
    result = coordinator.validate_program(
        "test-single",
        "(lambda (x) x)"
    )
    
    assert result.success, "Validation should succeed"
    assert isinstance(result.h1, int), "H¹ should be an integer"
    assert isinstance(result.vg, int), "V(G) should be an integer"
    assert isinstance(result.hypothesis_holds, bool), "hypothesis_holds should be boolean"
    
    print(f"✓ H¹={result.h1}, V(G)={result.vg}, holds={result.hypothesis_holds}")
    return True


def test_hypothesis_validator():
    """Test hypothesis validation logic"""
    print("\nTest 2: Hypothesis Validation Logic")
    print("-" * 50)
    
    validator = HypothesisValidator(default_k=1)
    
    # Perfect match
    result1 = validator.validate_program("test-1", h1=2, vg=3, k=1)
    assert result1.hypothesis_holds, "Should hold when H¹ = V(G) - k"
    print(f"✓ Perfect match: H¹={result1.h1}, V(G)={result1.vg}, k={result1.k}")
    
    # Mismatch
    result2 = validator.validate_program("test-2", h1=5, vg=3, k=1)
    assert not result2.hypothesis_holds, "Should not hold when values differ"
    print(f"✓ Mismatch detected: H¹={result2.h1}, V(G)={result2.vg}, diff={result2.difference}")
    
    # k estimation
    result3 = validator.validate_program("test-3", h1=2, vg=3)
    assert result3.k == 1, "Should estimate k=1"
    print(f"✓ k estimation: k={result3.k}")
    
    return True


def test_corpus_validation():
    """Test validation of multiple programs"""
    print("\nTest 3: Corpus Validation")
    print("-" * 50)
    
    programs = [
        {"program_id": "test-1", "source_code": "(define x 42)"},
        {"program_id": "test-2", "source_code": "(if (> x 0) 1 -1)"},
        {"program_id": "test-3", "source_code": "(lambda (x) x)"}
    ]
    
    coordinator = ValidationCoordinator()
    results = coordinator.validate_corpus(programs)
    
    assert len(results) == 3, "Should validate 3 programs"
    assert all(r.success for r in results), "All validations should succeed"
    
    stats = compute_statistics(results)
    assert stats['total'] == 3, "Should have 3 total programs"
    assert stats['succeeded'] == 3, "All should succeed"
    
    print(f"✓ Validated {stats['total']} programs")
    print(f"✓ Success rate: {stats['hypothesis_success_rate']:.1%}")
    return True


def test_statistics_computation():
    """Test statistical computation"""
    print("\nTest 4: Statistics Computation")
    print("-" * 50)
    
    from coordinator.validation import ValidationResult
    
    results = [
        ValidationResult("1", h1=1, vg=2, k=1, difference=0, hypothesis_holds=True, success=True),
        ValidationResult("2", h1=2, vg=3, k=1, difference=0, hypothesis_holds=True, success=True),
        ValidationResult("3", h1=5, vg=3, k=1, difference=3, hypothesis_holds=False, success=True),
    ]
    
    stats = compute_statistics(results)
    
    assert stats['total'] == 3
    assert stats['hypothesis_holds_count'] == 2
    assert stats['hypothesis_fails_count'] == 1
    assert abs(stats['hypothesis_success_rate'] - 2/3) < 0.01
    
    print(f"✓ Statistics computed correctly")
    print(f"  Total: {stats['total']}")
    print(f"  Holds: {stats['hypothesis_holds_count']}")
    print(f"  Rate: {stats['hypothesis_success_rate']:.1%}")
    return True


def main():
    """Run all integration tests"""
    print("=" * 70)
    print("INTEGRATION TESTS - Computational Scheme Theory")
    print("=" * 70)
    
    tests = [
        test_single_program,
        test_hypothesis_validator,
        test_corpus_validation,
        test_statistics_computation
    ]
    
    passed = 0
    failed = 0
    
    for test in tests:
        try:
            if test():
                passed += 1
        except AssertionError as e:
            print(f"✗ FAILED: {e}")
            failed += 1
        except Exception as e:
            print(f"✗ ERROR: {e}")
            failed += 1
    
    print("\n" + "=" * 70)
    print("TEST SUMMARY")
    print("=" * 70)
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Total: {len(tests)}")
    
    if failed == 0:
        print("\n✅ All integration tests passed!")
        return 0
    else:
        print(f"\n❌ {failed} test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())

