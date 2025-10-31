# Quick Start Guide

Get started with the Computational Scheme Theory validation system in 5 minutes.

## Prerequisites Check

```bash
./scripts/verify-env.sh
```

## Installation

### 1. Python Dependencies (Optional but Recommended)

```bash
cd python-coordinator
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

**Note**: The system works without installing dependencies (runs in placeholder mode).

### 2. Generate Test Corpus

```bash
python3 test-corpus/scripts/generate_corpus.py
```

This creates 15 test programs in `test-corpus/`.

## Quick Demo

### Run Pipeline Demonstration

```bash
python3 scripts/demo_pipeline.py
```

Shows the complete pipeline from Scheme source to validation.

### Run Single Program Validation

```bash
python3 scripts/run_validation.py --corpus test-corpus
```

Validates all programs in the corpus (currently in placeholder mode).

## What You Can Do Now

### ✅ Working Features

1. **Test Corpus Management**
   - Generate programs: `python3 test-corpus/scripts/generate_corpus.py`
   - Validate corpus: `python3 test-corpus/scripts/validate_corpus.py`

2. **Validation Pipeline**
   - Run validation: `python3 scripts/run_validation.py`
   - View results in JSON format
   - Compute statistics and correlations

3. **Haskell Computation** (when built)
   - `cabal build` in `haskell-core/`
   - Use `computeH1FromSource` function
   - Run: `cabal run computational-scheme-theory -- compute-h1 file.scm`

4. **Racket Computation** (when built)
   - Use `compute-cyclomatic-complexity` function
   - HTTP API available: `racket metrics-api.rkt`

## Current Status

- ✅ **Core Algorithms**: All 4 algorithms implemented (Haskell)
- ✅ **V(G) Calculator**: Full implementation (Racket)
- ✅ **Validation Logic**: Complete (Python)
- ✅ **Test Corpus**: 15 programs generated
- ⏳ **Service Integration**: Placeholder mode (needs gRPC setup)
- ⏳ **Real Computation**: Requires services running

## Next Steps

1. **For Testing Algorithms**:
   ```bash
   cd haskell-core && cabal build
   cd racket-metrics && raco test .
   ```

2. **For Service Integration**:
   - Generate proto code: `make proto`
   - Implement gRPC endpoints
   - Connect services

3. **For Full Validation**:
   - Start Haskell service
   - Start Racket service
   - Run validation: `python3 scripts/run_validation.py`

## Help

- See `SETUP_GUIDE.md` for detailed setup
- See `USAGE_EXAMPLES.md` for code examples
- See `IMPLEMENTATION_STATUS.md` for current status
- See `docs/10 - IMPLEMENTATION/` for design docs

