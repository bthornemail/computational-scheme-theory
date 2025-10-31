#!/bin/bash
# Generate protocol buffer code for all languages

set -e

PROTO_DIR="proto"
PYTHON_OUT="python-coordinator/coordinator/proto"

echo "Generating protocol buffer code..."

# Check protoc
if ! command -v protoc &> /dev/null; then
    echo "Error: protoc not found. Please install Protocol Buffers compiler."
    echo "  Ubuntu/Debian: sudo apt install protobuf-compiler"
    echo "  Or download from: https://grpc.io/docs/protoc-installation/"
    exit 1
fi

# Python code generation
echo "Generating Python code..."
mkdir -p "$PYTHON_OUT"

# Check if grpc_tools.protoc is available
if python3 -m grpc_tools.protoc --version &> /dev/null; then
    python3 -m grpc_tools.protoc \
        --proto_path="$PROTO_DIR" \
        --python_out="$PYTHON_OUT" \
        --grpc_python_out="$PYTHON_OUT" \
        "$PROTO_DIR"/*.proto
    echo "✓ Python proto code generated"
else
    echo "⚠ grpc_tools not available - installing..."
    pip3 install --user grpcio-tools || echo "⚠ Could not install grpcio-tools"
    if python3 -m grpc_tools.protoc --version &> /dev/null; then
        python3 -m grpc_tools.protoc \
            --proto_path="$PROTO_DIR" \
            --python_out="$PYTHON_OUT" \
            --grpc_python_out="$PYTHON_OUT" \
            "$PROTO_DIR"/*.proto
        echo "✓ Python proto code generated"
    else
        echo "⚠ Python proto generation skipped - install grpcio-tools"
    fi
fi

# Haskell proto generation (requires proto-lens-protoc)
echo ""
echo "Haskell proto generation:"
if command -v proto-lens-protoc &> /dev/null; then
    echo "  Generating Haskell proto code..."
    HASKELL_OUT="haskell-core/src/Proto"
    mkdir -p "$HASKELL_OUT"
    # Haskell proto generation would go here
    echo "  ✓ Haskell proto generation (requires proto-lens setup)"
else
    echo "  ⚠ proto-lens-protoc not found - Haskell proto generation skipped"
    echo "  Install with: cabal install proto-lens-protoc"
fi

# Racket proto generation
echo ""
echo "Racket proto generation:"
echo "  ⚠ Racket proto generation not yet implemented"
echo "  For MVP, using JSON over HTTP instead of gRPC"

echo ""
echo "=== Proto Generation Summary ==="
if [ -d "$PYTHON_OUT" ] && [ "$(ls -A $PYTHON_OUT 2>/dev/null)" ]; then
    echo "✓ Python: $(ls -1 $PYTHON_OUT/*.py 2>/dev/null | wc -l) files generated"
else
    echo "✗ Python: No files generated"
fi
echo "✓ Protocol buffer definitions: $(ls -1 $PROTO_DIR/*.proto | wc -l) files"

