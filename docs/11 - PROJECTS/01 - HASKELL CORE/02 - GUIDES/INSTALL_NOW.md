# Installation Steps - Run These Commands

## You need to run these with sudo:

```bash
sudo apt update
sudo apt install -y libblas-dev liblapack-dev python3-venv
```

Or use the setup script:
```bash
bash scripts/setup-system-deps.sh
```

## After that, build and test:

```bash
# Build Haskell (this should work after installing BLAS/LAPACK)
cd haskell-core
cabal build

# Set up Python environment
cd ..
python3 -m venv venv
source venv/bin/activate
pip install --upgrade pip setuptools wheel
pip install -r python-coordinator/requirements.txt

# Test everything
cd haskell-core
cabal run computational-scheme-theory -- --demo

cd ../racket-metrics
racket -e '(displayln "Racket test")'

cd ..
source venv/bin/activate
python3 scripts/demo_pipeline.py
```
