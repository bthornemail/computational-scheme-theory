#!/bin/bash
# Setup script for system dependencies required by Computational Scheme Theory

set -e

echo "=== Installing System Dependencies for Computational Scheme Theory ==="
echo ""

# Check if running as root or with sudo
if [ "$EUID" -eq 0 ]; then
    SUDO=""
else
    SUDO="sudo"
    echo "Note: This script requires sudo privileges to install system packages."
    echo ""
fi

# Update package lists
echo "📦 Updating package lists..."
$SUDO apt update

# Install BLAS and LAPACK (required for hmatrix in Haskell)
echo ""
echo "📦 Installing BLAS and LAPACK (required for hmatrix)..."
$SUDO apt install -y libblas-dev liblapack-dev

# Install Python venv support
echo ""
echo "📦 Installing Python venv support..."
$SUDO apt install -y python3-venv

# Install Python dependencies (if needed)
echo ""
echo "📦 Checking Python dependencies..."
if command -v pip3 &> /dev/null; then
    echo "Installing Python packages..."
    pip3 install --user -r python-coordinator/requirements.txt || echo "Warning: Some Python packages may need manual installation"
else
    echo "Warning: pip3 not found. Python dependencies may need manual installation."
fi

# Verify installations
echo ""
echo "✅ Verifying installations..."

if [ -f /usr/lib/x86_64-linux-gnu/libblas.so ] || [ -f /usr/lib/libblas.so ]; then
    echo "  ✓ BLAS found"
else
    echo "  ✗ BLAS not found"
fi

if [ -f /usr/lib/x86_64-linux-gnu/liblapack.so ] || [ -f /usr/lib/liblapack.so ]; then
    echo "  ✓ LAPACK found"
else
    echo "  ✗ LAPACK not found"
fi

echo ""
echo "=== Setup Complete ==="
echo ""
echo "Next steps:"
echo "  1. Build Haskell project: cd haskell-core && cabal build"
echo "  2. Test Racket: cd racket-metrics && racket -e '(displayln \"Racket ready\")'"
echo "  3. Run validation: python3 scripts/run_validation.py --help"

