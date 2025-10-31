# Haskell Mathematical Core

This component implements the mathematical core algorithms for Computational Scheme Theory, computing Čech cohomology H¹(X_Comp, O_Comp) from R5RS Scheme programs.

## Overview

The Haskell core implements four main algorithms:

1. **Algorithm 1**: Binding Algebra Extractor - Extracts R_Scheme rig from Scheme source
2. **Algorithm 2**: Scope Topology Constructor - Builds Zariski topology from bindings
3. **Algorithm 3**: Čech Complex Builder - Constructs simplicial complex from topology
4. **Algorithm 4**: Cohomology Calculator - Computes H¹ cohomology group

## Requirements

- **GHC** 9.0 or later
- **Cabal** 3.8 or later

## Quick Start

### Build

```bash
cabal build
```

### Run Tests

```bash
cabal test
```

### Run Benchmarks

```bash
cabal bench
```

### Run Executable

```bash
cabal run computational-scheme-theory -- --help
```

## Project Structure

```
haskell-core/
├── src/
│   ├── ComputationalScheme/
│   │   ├── Types.hs              # Core type definitions
│   │   ├── Rig.hs                # R_Scheme rig implementation
│   │   ├── Algorithm1/           # Binding algebra extraction
│   │   ├── Algorithm2/           # Scope topology construction
│   │   ├── Algorithm3/           # Čech complex building
│   │   ├── Algorithm4/           # Cohomology computation
│   │   ├── Distributed/          # Distributed coordination (Phase 3)
│   │   ├── Service/               # gRPC service (Phase 1 Week 13)
│   │   └── Utils/                 # Utilities
│   └── Main.hs                    # CLI entry point
├── test/                          # Test suites
├── bench/                         # Benchmark suites
└── computational-scheme-theory.cabal
```

## Development

### Adding Dependencies

Edit `computational-scheme-theory.cabal` and run:

```bash
cabal update
cabal build
```

### Running Individual Tests

```bash
cabal test --test-show-details=streaming
```

### Code Generation from Protocol Buffers

Protocol buffer code generation is handled by the root `Makefile`:

```bash
make proto
```

## Architecture

See `docs/10 - IMPLEMENTATION/01-mathematical-core-architecture.md` for detailed architecture documentation.

## Next Steps

1. Implement `Types.hs` and `Rig.hs` (Month 1, Week 1-2)
2. Implement Algorithm 1 (Month 1, Week 3-4)
3. Implement Algorithms 2-4 (Month 2-3)
4. Add gRPC service (Month 4, Week 1-2)

