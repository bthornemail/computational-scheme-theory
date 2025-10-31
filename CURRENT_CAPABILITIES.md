# Current Capabilities

What works **right now** vs what requires additional setup.

## ✅ Works Right Now (No Additional Setup)

### 1. Test Corpus Management
```bash
# Generate test programs
python3 test-corpus/scripts/generate_corpus.py

# Validate corpus structure
python3 test-corpus/scripts/validate_corpus.py
```
**Status**: ✅ Fully functional

### 2. Validation Pipeline (Placeholder Mode)
```bash
# Run validation (returns placeholder values)
python3 scripts/run_validation.py --corpus test-corpus

# View results
python3 scripts/run_validation.py --corpus test-corpus --output results.json
```
**Status**: ✅ Works, returns H¹=0, V(G)=0 until services compute real values

### 3. Integration Tests
```bash
python3 scripts/integration_test.py
```
**Status**: ✅ All 4 tests passing

### 4. Pipeline Demonstration
```bash
python3 scripts/demo_pipeline.py
```
**Status**: ✅ Shows complete pipeline architecture

### 5. Hypothesis Validation Logic
```python
from coordinator.validation import HypothesisValidator

validator = HypothesisValidator()
result = validator.validate_program("test", h1=2, vg=3, k=1)
# Returns: hypothesis_holds=True, difference=0
```
**Status**: ✅ Fully functional

### 6. Statistical Analysis
```python
from coordinator.validation import compute_statistics, compute_correlation

stats = compute_statistics(results)
corr = compute_correlation(results)
```
**Status**: ✅ Fully functional

---

## ⚙️ Requires Tool Installation

### Haskell Computation (Requires: GHC + Cabal)

**Install**:
```bash
# Ubuntu/Debian
sudo apt install ghc cabal-install

# Or use Stack
curl -sSL https://get.haskellstack.org/ | sh
```

**Build**:
```bash
cd haskell-core
cabal build
```

**Use**:
```bash
# Compute H¹ from file
cabal run computational-scheme-theory -- compute-h1 program.scm

# Or use Python direct computation
python3 scripts/test_direct_compute.py
```

**Status**: ✅ Code ready, requires GHC/Cabal installation

### Racket Computation (Requires: Racket)

**Install**:
```bash
# Ubuntu/Debian
sudo apt install racket

# Or download from https://racket-lang.org/
```

**Use**:
```bash
# Start HTTP service
cd racket-metrics
racket metrics-api.rkt 50052

# Or compute directly
racket -e '(require "cyclomatic.rkt" "cfg-builder.rkt" "r5rs-parser.rkt")
           (define ast (parse-r5rs "(lambda (x) x)"))
           (displayln (complexity-metrics-v-g 
                       (compute-cyclomatic-complexity (build-cfg (first ast)))))'
```

**Status**: ✅ Code ready, requires Racket installation

### Real Validation (Requires: Both Haskell + Racket)

**Once both are installed**:
```bash
# Direct computation (no services needed)
python3 scripts/run_validation.py --corpus test-corpus

# Or use services
# Terminal 1: racket metrics-api.rkt 50052
# Terminal 2: python3 scripts/run_validation.py
```

**Status**: ✅ Code ready, requires both tools

---

## 📋 Feature Matrix

| Feature | Status | Requires |
|---------|--------|----------|
| Corpus generation | ✅ Ready | Python 3 |
| Corpus validation | ✅ Ready | Python 3 |
| Validation pipeline | ✅ Ready | Python 3 |
| Hypothesis testing | ✅ Ready | Python 3 |
| Statistics | ✅ Ready | Python 3 |
| H¹ computation | ⚙️ Ready | GHC + Cabal |
| V(G) computation | ⚙️ Ready | Racket |
| Direct computation | ⚙️ Ready | GHC + Racket |
| gRPC services | 📋 Pending | gRPC setup |
| Full validation | ⚙️ Ready | GHC + Racket |

**Legend**:
- ✅ Ready to use now
- ⚙️ Code ready, needs tool installation
- 📋 Planned but not implemented

---

## 🚀 Quick Start Options

### Option 1: Just Test Infrastructure (No Tools Needed)
```bash
python3 test-corpus/scripts/generate_corpus.py
python3 scripts/run_validation.py  # Placeholder mode
python3 scripts/integration_test.py
```
✅ Works immediately

### Option 2: With Haskell Only
```bash
# Install: sudo apt install ghc cabal-install
cd haskell-core && cabal build
cabal run computational-scheme-theory -- compute-h1 test.scm
```
⚙️ Requires GHC installation

### Option 3: With Racket Only
```bash
# Install: sudo apt install racket
cd racket-metrics
racket metrics-api.rkt 50052  # Start HTTP service
```
⚙️ Requires Racket installation

### Option 4: Full Validation (Both Tools)
```bash
# Install both GHC and Racket
python3 scripts/run_validation.py  # Auto-detects and uses direct computation
```
⚙️ Requires both tools, but then computes real values

---

## 💡 Recommendations

**For immediate testing**:
- Use Option 1 (test infrastructure)
- All Python code works without any tools

**For development**:
- Install Racket first (easier setup)
- Test V(G) computation
- Then install GHC/Cabal for H¹

**For production**:
- Install both tools
- Use direct computation for MVP
- Add gRPC services later

---

See `SETUP_GUIDE.md` for detailed installation instructions.

