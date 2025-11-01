# Quick Start Guide - Computational Scheme Theory

**Last Updated**: 2025-01-31

---

## 🚀 Getting Started

This guide will help you quickly validate the hypothesis **H¹ = V(G) - k** using the unified implementation.

---

## Prerequisites

- **Racket** 8.0 or later
- **Python** 3.9 or later
- Test corpus (optional, in `test-corpus/` directory)

---

## Option 1: Python Coordinator (Recommended)

### Setup

```bash
# Install Python dependencies
cd python-coordinator
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
```

### Validate a Single Program

```python
from coordinator.direct_compute import DirectComputeCoordinator

coordinator = DirectComputeCoordinator()
result = coordinator.validate_program(
    "test-001",
    "(lambda (x) (if (> x 0) 1 -1))"
)

print(f"H¹ = {result.h1}")
print(f"V(G) = {result.vg}")
print(f"k = {result.k}")
print(f"Hypothesis holds: {result.hypothesis_holds}")
print(f"Difference: {result.difference}")
```

### Validate Entire Corpus

```bash
cd ..
python validate_hypothesis.py
```

This will:
- Compute H¹ and V(G) for all programs in `test-corpus/`
- Analyze correlation
- Generate `validation_results.json`
- Display summary statistics

---

## Option 2: Racket Direct

### Compute H¹

```bash
cd racket-unified/src
racket -e '
(require "algorithms/unified-pipeline.rkt")
(define result (compute-h1-from-source-detailed "(lambda (x) x)"))
(displayln (format "H¹ = ~a" (pipeline-result-h1 result)))
'
```

### Compute V(G)

```bash
cd racket-unified/src
racket -e '
(require "algorithms/cfg-builder.rkt" "algorithms/cyclomatic.rkt")
(define cfg (build-cfg-from-source "(lambda (x) (if (> x 0) 1 -1))"))
(define metrics (compute-cyclomatic-complexity cfg))
(displayln (format "V(G) = ~a" (complexity-metrics-v-g metrics)))
'
```

---

## Option 3: Natural Language Interface

```bash
cd racket-unified
racket -e '
(require "src/nlp-integration.rkt")
(execute-nl-query "compute H1 for program test-001")
(execute-nl-query "compute V(G) for program test-001")
(execute-nl-query "validate hypothesis for program test-001")
'
```

---

## Understanding Results

### ValidationResult Fields

- `h1`: H¹ cohomology value
- `vg`: V(G) cyclomatic complexity
- `k`: Normalization constant (estimated)
- `difference`: |H¹ - (V(G) - k)|
- `hypothesis_holds`: True if difference = 0

### Hypothesis Interpretation

**H¹ = V(G) - k**

- If `hypothesis_holds = True`: Hypothesis validated for this program
- If `difference` is small (< 2): Close match, may need k adjustment
- If `difference` is large: Hypothesis may not hold for this program

---

## Example Output

```
Processing test-corpus/baseline/prog01.scm...
 ✓ H¹=0, V(G)=1, k=1

Processing test-corpus/simple-control/prog01.scm...
 ✓ H¹=1, V(G)=2, k=1

Summary Statistics:
  H¹: min=0, max=5, mean=2.34, median=2
  V(G): min=1, max=6, mean=3.45, median=3
  V(G) - H¹: min=0, max=2, mean=1.11, median=1

Correlation (H¹, V(G)): 0.9234

Estimated constant k = V(G) - H¹:
  Mean: 1.11
  Median: 1.0
  Mode: 1
```

---

## Troubleshooting

### Racket Not Found

```bash
# Check Racket installation
which racket
racket --version  # Should be 8.0+

# If not installed:
# Ubuntu/Debian: sudo apt-get install racket
# macOS: brew install racket
# Windows: Download from https://racket-lang.org
```

### Module Not Found Errors

Make sure you're running from the correct directory:
- For Python: from project root or `python-coordinator/`
- For Racket: from `racket-unified/src/` or use absolute paths

### Computation Errors

If computations return 0 or fail:
1. Check that source code is valid Scheme
2. Verify file paths are correct
3. Check Racket/Python error messages

---

## Next Steps

1. **Run validation** on the test corpus
2. **Analyze results** in `validation_results.json`
3. **Expand corpus** if needed (target: 350 programs)
4. **Run statistical analysis** on correlation

---

## Documentation

- **Architecture**: See `INTEGRATION_COMPLETE.md`
- **Coverage**: See `racket-unified/FINAL_COVERAGE_REPORT.md`
- **Python API**: See `python-coordinator/README.md`

---

**Happy Validating!** 🎉

