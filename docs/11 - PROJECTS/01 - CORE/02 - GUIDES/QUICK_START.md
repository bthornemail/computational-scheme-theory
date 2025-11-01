# Quick Start Guide

## Prerequisites

You need:
- **GHC** (Glasgow Haskell Compiler) 9.0+
- **Cabal** (Haskell package manager) 3.8+
- **Racket** 8.0+
- **Python** 3.10+
- **System libraries**: BLAS and LAPACK (for hmatrix)

## Step 1: Install System Dependencies

Run the setup script (requires sudo):

```bash
bash scripts/setup-system-deps.sh
```

Or manually:
```bash
sudo apt update
sudo apt install -y libblas-dev liblapack-dev python3-venv
```

## Step 2: Set Up Python Virtual Environment

```bash
cd /home/main/computational-scheme-theory
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install -r python-coordinator/requirements.txt
```

**Note**: Always activate the virtual environment before running Python scripts:
```bash
source venv/bin/activate
```

## Step 3: Build Haskell Project

```bash
cd haskell-core
cabal update
cabal build
```

If you see errors about missing BLAS/LAPACK, go back to Step 1.

After building, you can test the executable:
```bash
cabal run computational-scheme-theory -- --help
cabal run computational-scheme-theory -- --demo
```

## Step 4: Test Racket

```bash
cd racket-metrics
racket -e '(displayln "Racket ready")'
```

## Step 5: Run Direct Computation

Once both Haskell and Racket are built, you can run direct computation:

```bash
cd /home/main/computational-scheme-theory
source venv/bin/activate
python3 scripts/demo_pipeline.py
```

Or test on a single program:
```bash
python3 scripts/test_direct_compute.py
```

## Step 6: Run Validation

```bash
source venv/bin/activate
python3 scripts/run_validation.py --help
python3 scripts/run_validation.py --corpus test-corpus
```

## Troubleshooting

### Haskell won't build - missing BLAS/LAPACK
```bash
sudo apt install -y libblas-dev liblapack-dev
cd haskell-core
cabal clean
cabal build
```

### Python packages fail to install
Make sure you're using a virtual environment:
```bash
python3 -m venv venv
source venv/bin/activate
pip install -r python-coordinator/requirements.txt
```

### Racket module not found errors
Make sure you're in the correct directory:
```bash
cd racket-metrics
racket your-script.rkt
```

## Next Steps

- Read `CURRENT_CAPABILITIES.md` for what's working
- Read `docs/10 - IMPLEMENTATION/00-IMPLEMENTATION-OVERVIEW.md` for architecture
- Generate test corpus: `python3 test-corpus/scripts/generate_corpus.py`
