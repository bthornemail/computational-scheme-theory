# Setup Guide

Quick setup instructions for getting the Computational Scheme Theory project running.

## Quick Start

### 1. Verify Prerequisites

Run the environment verification script:

```bash
./scripts/verify-env.sh
```

This checks for:
- GHC and Cabal (Haskell)
- Racket
- Python 3.9+
- Docker
- Protocol Buffers compiler

### 2. Install Python Dependencies

**Option A: Using Virtual Environment (Recommended)**

```bash
cd python-coordinator
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
```

**Option B: Using pipx (Alternative)**

```bash
pipx install --include-deps -r python-coordinator/requirements.txt
```

**Option C: System-wide (Not Recommended)**

```bash
pip install --user -r python-coordinator/requirements.txt
```

### 3. Generate Test Corpus

```bash
python3 test-corpus/scripts/generate_corpus.py
python3 test-corpus/scripts/validate_corpus.py
```

### 4. Run Validation (Placeholder Mode)

The validation script will run in placeholder mode until services are connected:

```bash
python3 scripts/run_validation.py
```

**Note**: Currently returns placeholder values (H¹=0, V(G)=0) because services aren't running.

## Running Services (For Full Validation)

### Start Racket Metrics Service

```bash
cd racket-metrics
racket metrics-api.rkt 50052
```

### Start Haskell Math Core Service

```bash
cd haskell-core
cabal build
cabal run computational-scheme-theory -- --server --port 50051
```

(Note: Service endpoints need to be implemented)

### Run Full Validation

Once services are running:

```bash
python3 scripts/run_validation.py --corpus test-corpus --output results.json
```

## Troubleshooting

### Error: "No module named 'grpc'"

**Solution**: Install Python dependencies (see step 2 above)

The code now handles missing gRPC gracefully - it will run in placeholder mode.

### Error: "ModuleNotFoundError: No module named 'requests'"

**Solution**: Install requirements:
```bash
pip install requests
```

Or use the full requirements.txt.

### Haskell Build Errors

If `cabal` is not installed:
```bash
# Ubuntu/Debian
sudo apt install cabal-install

# Or use Stack
curl -sSL https://get.haskellstack.org/ | sh
```

### Racket Not Found

Install Racket:
```bash
# Ubuntu/Debian
sudo apt install racket

# Or download from https://racket-lang.org/
```

## Development Workflow

### Build All Components

```bash
make build
```

### Run Tests

```bash
make test
```

### Generate Protocol Buffers

```bash
make proto
```

## Current Status

- ✅ All algorithms implemented
- ✅ Test corpus generation working
- ✅ Validation script runs (placeholder mode)
- ⏳ Services need to be connected for real computation
- ⏳ gRPC code generation pending

## Next Steps

1. **Set up Python virtual environment** (if not done)
2. **Generate test corpus** (already done - 15 programs)
3. **Implement service endpoints** (Haskell gRPC, Racket HTTP)
4. **Run full validation** with real computations

---

For more details, see `IMPLEMENTATION_STATUS.md`.

