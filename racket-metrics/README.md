# Racket Metrics Calculator

This component computes traditional cyclomatic complexity V(G) from R5RS Scheme programs for comparison with the H¹ cohomology computed by the Haskell core.

## Overview

The Racket metrics calculator:

1. Parses R5RS Scheme source code to AST
2. Builds control flow graph (CFG) from AST
3. Computes V(G) = E - N + 2P

## Requirements

- **Racket** 8.0 or later

## Quick Start

### Install Dependencies

```bash
raco pkg install --deps search-auto
```

### Run Tests

```bash
raco test .
```

### Run Main Module

```bash
racket main.rkt
```

## Project Structure

```
racket-metrics/
├── main.rkt                  # Entry point
├── r5rs-parser.rkt           # S-expression → AST
├── ast-types.rkt             # AST data structures
├── cfg-builder.rkt           # AST → CFG
├── cfg-types.rkt             # CFG data structures
├── cyclomatic.rkt            # CFG → V(G)
├── graph-utils.rkt           # Graph algorithms
├── metrics-api.rkt           # Service interface (Month 4)
├── pretty-print.rkt          # CFG visualization
├── test/                      # Test suites
├── examples/                  # Example programs
└── info.rkt                   # Package metadata
```

## Development

### Running Individual Tests

```bash
raco test test/parser-test.rkt
raco test test/cfg-builder-test.rkt
raco test test/cyclomatic-test.rkt
```

### Code Generation from Protocol Buffers

Protocol buffer code generation is handled by the root `Makefile`:

```bash
make proto
```

Note: For MVP (Phase 1), we use JSON over HTTP. gRPC support will be added in Week 13 if a mature Racket gRPC library is available.

## Architecture

See `docs/10 - IMPLEMENTATION/02-metrics-calculator-design.md` for detailed architecture documentation.

## Next Steps

1. Implement R5RS parser (Month 3, Week 3)
2. Implement CFG builder (Month 3, Week 3-4)
3. Implement V(G) calculator (Month 3, Week 4)
4. Add service interface (Month 4, Week 1)

