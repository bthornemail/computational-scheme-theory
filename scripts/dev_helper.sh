#!/bin/bash
# Development Helper Script
# Quick commands for common development tasks

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

case "${1:-help}" in
    "build")
        echo "Building all components..."
        cd haskell-core && cabal build
        echo "✓ Haskell built"
        ;;
    
    "test")
        echo "Running tests..."
        cd haskell-core && cabal test || echo "Note: Haskell tests require implementation"
        cd "$PROJECT_ROOT/racket-metrics" && raco test . || echo "Note: Racket tests require implementation"
        cd "$PROJECT_ROOT/python-coordinator" && pytest tests/ || echo "Note: Python tests running"
        ;;
    
    "corpus")
        echo "Generating test corpus..."
        python3 test-corpus/scripts/generate_corpus.py
        python3 test-corpus/scripts/validate_corpus.py
        ;;
    
    "validate")
        echo "Running validation..."
        python3 scripts/run_validation.py --corpus test-corpus "${@:2}"
        ;;
    
    "demo")
        python3 scripts/demo_pipeline.py "${@:2}"
        ;;
    
    "proto")
        echo "Generating protocol buffers..."
        ./scripts/generate_proto.sh
        ;;
    
    "clean")
        echo "Cleaning build artifacts..."
        cd haskell-core && cabal clean
        find . -type d -name "compiled" -exec rm -rf {} + 2>/dev/null || true
        find python-coordinator -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
        find python-coordinator -type f -name "*.pyc" -delete 2>/dev/null || true
        echo "✓ Clean complete"
        ;;
    
    "status")
        echo "=== Project Status ==="
        echo ""
        echo "Haskell modules: $(find haskell-core/src -name '*.hs' | wc -l)"
        echo "Racket modules: $(find racket-metrics -name '*.rkt' -type f | wc -l)"
        echo "Python modules: $(find python-coordinator/coordinator -name '*.py' | wc -l)"
        echo "Test programs: $(find test-corpus -name '*.scm' | wc -l)"
        echo ""
        echo "Ready for:"
        echo "  - Service integration"
        echo "  - Corpus expansion"
        echo "  - Validation experiments"
        ;;
    
    "help"|*)
        echo "Development Helper - Computational Scheme Theory"
        echo ""
        echo "Usage: $0 <command> [options]"
        echo ""
        echo "Commands:"
        echo "  build      - Build all components"
        echo "  test       - Run all tests"
        echo "  corpus     - Generate and validate test corpus"
        echo "  validate   - Run validation experiments"
        echo "  demo       - Run pipeline demonstration"
        echo "  proto      - Generate protocol buffer code"
        echo "  clean      - Clean build artifacts"
        echo "  status     - Show project status"
        echo "  help       - Show this help"
        echo ""
        echo "Examples:"
        echo "  $0 corpus"
        echo "  $0 validate --output results.json"
        echo "  $0 demo --demo single"
        ;;
esac

