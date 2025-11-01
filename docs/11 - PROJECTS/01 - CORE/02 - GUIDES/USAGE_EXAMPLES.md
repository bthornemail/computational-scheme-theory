# Usage Examples

Quick reference for using the Computational Scheme Theory validation system.

## Command-Line Usage

### Validate a Single Program

```bash
python3 scripts/run_validation.py --corpus test-corpus
```

### Generate Test Corpus

```bash
python3 test-corpus/scripts/generate_corpus.py
python3 test-corpus/scripts/validate_corpus.py
```

### Run Pipeline Demonstration

```bash
python3 scripts/demo_pipeline.py
```

### Compute H¹ from Haskell

```bash
cd haskell-core
cabal build
echo "(lambda (x) x)" > /tmp/test.scm
cabal run computational-scheme-theory -- compute-h1 /tmp/test.scm
```

### Compute V(G) from Racket

```bash
cd racket-metrics
racket -e '(require "cyclomatic.rkt" "cfg-builder.rkt" "r5rs-parser.rkt") 
           (define ast (parse-r5rs "(lambda (x) x)"))
           (define cfg (build-cfg (first ast)))
           (displayln (complexity-metrics-v-g (compute-cyclomatic-complexity cfg)))'
```

## Python API Usage

### Validate Single Program

```python
from coordinator.service import ValidationCoordinator

coordinator = ValidationCoordinator()
result = coordinator.validate_program(
    "test-001",
    "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
)

print(f"H¹ = {result.h1}")
print(f"V(G) = {result.vg}")
print(f"Hypothesis holds: {result.hypothesis_holds}")
```

### Validate Corpus

```python
from coordinator.service import ValidationCoordinator
from pathlib import Path

coordinator = ValidationCoordinator()
corpus_dir = Path("test-corpus")

# Load programs
programs = []
for category in ["baseline", "simple-control", "recursion"]:
    category_dir = corpus_dir / category
    for source_file in category_dir.glob("*.scm"):
        program_id = source_file.stem
        source_code = source_file.read_text()
        programs.append({
            "program_id": program_id,
            "source_code": source_code
        })

# Validate
results = coordinator.validate_corpus(programs)

# Compute statistics
from coordinator.validation import compute_statistics
stats = compute_statistics(results)
print(f"Success rate: {stats['hypothesis_success_rate']:.1%}")
```

## Haskell API Usage

### Compute H¹ from Source

```haskell
import ComputationalScheme.Service.API
import qualified Data.Text as T

main = do
  let source = T.pack "(lambda (x) x)"
  case computeH1FromSource source of
    Left err -> putStrLn $ "Error: " ++ err
    Right h1 -> putStrLn $ "H¹ = " ++ show h1
```

### Use Individual Algorithms

```haskell
import ComputationalScheme.Algorithm1.BindingExtractor
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm3.CechComplex
import ComputationalScheme.Algorithm4.Cohomology

-- Step by step
let rig = extractBindingAlgebra source  -- Algorithm 1
let topo = buildTopology rig scopeMap    -- Algorithm 2
let complex = buildCechComplex topo     -- Algorithm 3
let h1 = computeH1 complex              -- Algorithm 4
```

## Racket API Usage

### Compute V(G)

```racket
#lang racket
(require "cyclomatic.rkt"
         "cfg-builder.rkt"
         "r5rs-parser.rkt")

(define source "(lambda (x) x)")
(define ast-list (parse-r5rs source))
(define cfg (build-cfg (first ast-list)))
(define metrics (compute-cyclomatic-complexity cfg))

(displayln (complexity-metrics-v-g metrics))
```

## Service Integration (When Running)

### Start Racket HTTP Service

```bash
cd racket-metrics
racket metrics-api.rkt 50052
```

### Query Racket Service

```bash
curl -X POST http://localhost:50052/compute-vg \
  -H "Content-Type: application/json" \
  -d '{"program_id": "test", "source_code": "(lambda (x) x)"}'
```

### Start Haskell gRPC Service

```bash
cd haskell-core
cabal run computational-scheme-theory -- --server --port 50051
```

(Implementation pending)

## Full Validation Workflow

```bash
# 1. Generate corpus
python3 test-corpus/scripts/generate_corpus.py

# 2. Validate corpus
python3 test-corpus/scripts/validate_corpus.py

# 3. (Optional) Start services
# In separate terminals:
#   racket metrics-api.rkt 50052
#   cabal run computational-scheme-theory -- --server 50051

# 4. Run validation
python3 scripts/run_validation.py \
  --corpus test-corpus \
  --categories baseline simple-control recursion \
  --output results.json

# 5. Analyze results
cat results.json | python3 -m json.tool
```

## Expected Output

When running validation, you should see:

```
INFO: Loaded 15 programs
INFO: Validating 15 programs...
INFO: [1/15] Validating baseline-001...
INFO:   ✓ H¹=0, V(G)=1, k=1, diff=0

============================================================
VALIDATION SUMMARY
============================================================
Total programs: 15
Succeeded: 15
Hypothesis Validation:
  Holds: 12
  Fails: 3
  Success rate: 80.00%
Mean difference: 0.50
Correlation (H¹, V(G)): 0.950
============================================================
```

---

For more details, see:
- `SETUP_GUIDE.md` - Setup instructions
- `IMPLEMENTATION_STATUS.md` - Implementation details
- `docs/10 - IMPLEMENTATION/` - Design documentation


