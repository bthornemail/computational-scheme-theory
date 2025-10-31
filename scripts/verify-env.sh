#!/bin/bash
# Environment verification script for Computational Scheme Theory project
# Checks that all required tools and dependencies are installed

set -e

ERRORS=0
WARNINGS=0

echo "=========================================="
echo "Computational Scheme Theory - Environment Verification"
echo "=========================================="
echo ""

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Check function
check_cmd() {
    local name=$1
    local cmd=$2
    local version_flag=${3:-"--version"}
    
    if command -v "$cmd" &> /dev/null; then
        version=$($cmd $version_flag 2>&1 | head -n 1)
        echo -e "${GREEN}✓${NC} $name: $version"
        return 0
    else
        echo -e "${RED}✗${NC} $name: Not found"
        ((ERRORS++))
        return 1
    fi
}

check_cmd_optional() {
    local name=$1
    local cmd=$2
    local version_flag=${3:-"--version"}
    
    if command -v "$cmd" &> /dev/null; then
        version=$($cmd $version_flag 2>&1 | head -n 1)
        echo -e "${GREEN}✓${NC} $name: $version"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} $name: Not found (optional)"
        ((WARNINGS++))
        return 1
    fi
}

# System dependencies
echo "System Dependencies:"
echo "-------------------"
check_cmd "Git" git
check_cmd "Docker" docker
check_cmd "Docker Compose" docker-compose
check_cmd_optional "Protocol Buffers Compiler" protoc
echo ""

# Haskell toolchain
echo "Haskell Toolchain:"
echo "------------------"
check_cmd "GHC (Glasgow Haskell Compiler)" ghc
check_cmd "Cabal" cabal
check_cmd_optional "Stack" stack
echo ""

# Racket
echo "Racket:"
echo "-------"
if command -v racket &> /dev/null; then
    version=$(racket --version 2>&1 | head -n 1)
    echo -e "${GREEN}✓${NC} Racket: $version"
else
    echo -e "${RED}✗${NC} Racket: Not found"
    ((ERRORS++))
fi
echo ""

# Python
echo "Python:"
echo "-------"
if command -v python3 &> /dev/null; then
    version=$(python3 --version 2>&1)
    echo -e "${GREEN}✓${NC} Python: $version"
    
    # Check Python version
    python_version=$(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:2])))')
    major=$(echo $python_version | cut -d. -f1)
    minor=$(echo $python_version | cut -d. -f2)
    if [ "$major" -lt 3 ] || ([ "$major" -eq 3 ] && [ "$minor" -lt 9 ]); then
        echo -e "${YELLOW}⚠${NC} Python version $python_version is below recommended 3.9+"
        ((WARNINGS++))
    fi
else
    echo -e "${RED}✗${NC} Python 3: Not found"
    ((ERRORS++))
fi

if command -v pip3 &> /dev/null || command -v pip &> /dev/null; then
    pip_cmd=$(command -v pip3 || command -v pip)
    version=$($pip_cmd --version 2>&1 | head -n 1)
    echo -e "${GREEN}✓${NC} pip: $version"
else
    echo -e "${RED}✗${NC} pip: Not found"
    ((ERRORS++))
fi
echo ""

# Project structure
echo "Project Structure:"
echo "-----------------"
if [ -d "haskell-core" ]; then
    echo -e "${GREEN}✓${NC} haskell-core/ directory exists"
else
    echo -e "${RED}✗${NC} haskell-core/ directory missing"
    ((ERRORS++))
fi

if [ -d "racket-metrics" ]; then
    echo -e "${GREEN}✓${NC} racket-metrics/ directory exists"
else
    echo -e "${RED}✗${NC} racket-metrics/ directory missing"
    ((ERRORS++))
fi

if [ -d "python-coordinator" ]; then
    echo -e "${GREEN}✓${NC} python-coordinator/ directory exists"
else
    echo -e "${RED}✗${NC} python-coordinator/ directory missing"
    ((ERRORS++))
fi

if [ -d "proto" ]; then
    echo -e "${GREEN}✓${NC} proto/ directory exists"
else
    echo -e "${RED}✗${NC} proto/ directory missing"
    ((ERRORS++))
fi

if [ -d "test-corpus" ]; then
    echo -e "${GREEN}✓${NC} test-corpus/ directory exists"
else
    echo -e "${RED}✗${NC} test-corpus/ directory missing"
    ((ERRORS++))
fi
echo ""

# Docker status
echo "Docker Status:"
echo "-------------"
if docker info &> /dev/null; then
    echo -e "${GREEN}✓${NC} Docker daemon is running"
else
    echo -e "${RED}✗${NC} Docker daemon is not running"
    ((ERRORS++))
fi
echo ""

# Summary
echo "=========================================="
if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed!${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ All required checks passed, but $WARNINGS warning(s)${NC}"
    exit 0
else
    echo -e "${RED}✗ $ERRORS error(s) found, $WARNINGS warning(s)${NC}"
    echo ""
    echo "Please install missing dependencies:"
    echo "  - System dependencies: See docs/10 - IMPLEMENTATION/00-IMPLEMENTATION-OVERVIEW.md"
    echo "  - Run 'make setup' to install project dependencies"
    exit 1
fi

