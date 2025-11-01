#!/usr/bin/env python3
"""
Test H¹ computation on programs with different structures
to find patterns that produce H¹ > 0
"""

import subprocess
from pathlib import Path

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
            for line in reversed(output.split('\n')):
                if "H¹" in line and "=" in line:
                    parts = line.split("=")
                    if len(parts) >= 2:
                        try:
                            return int(parts[-1].strip())
                        except ValueError:
                            continue
        return None
    except:
        return None

def main():
    # Test programs with different structures
    test_cases = [
        # Nested lets with cross-references
        ("(define (test) (let ((x 1)) (let ((y (+ x 2))) (let ((z (+ x y))) z))))", "nested-lets"),
        
        # Letrec (mutually recursive)
        ("(define (test) (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 5)))", "letrec"),
        
        # Closure capturing outer variable
        ("(define (make-counter) (let ((count 0)) (lambda () (set! count (+ count 1)) count)))", "closure"),
        
        # Complex nested structure
        ("(define (complex x) (if (> x 0) (let ((y (* x 2))) (if (> y 0) (let ((z (+ x y))) z) y)) x))", "complex-nested"),
    ]
    
    print("Testing H¹ for programs with different structures:")
    print("=" * 80)
    print()
    
    results = []
    for source, name in test_cases:
        # Write to temp file
        temp_file = PROJECT_ROOT / f"temp_{name}.scm"
        temp_file.write_text(source)
        
        h1 = compute_h1(temp_file)
        results.append((name, h1, source))
        
        print(f"{name:20s} H¹ = {h1 if h1 is not None else 'ERROR'}")
        
        # Cleanup
        temp_file.unlink()
    
    print()
    print("Analysis:")
    h1_values = [h for _, h, _ in results if h is not None]
    if h1_values:
        print(f"  Non-zero H¹ found: {sum(1 for h in h1_values if h > 0)}/{len(h1_values)}")
        if any(h > 0 for h in h1_values):
            print("  ✓ Found programs with H¹ > 0!")
            for name, h1, _ in results:
                if h1 and h1 > 0:
                    print(f"    {name}: H¹ = {h1}")
        else:
            print("  ⚠ All programs still have H¹ = 0")
            print("  This suggests the topology structure may need further refinement")

if __name__ == "__main__":
    main()

