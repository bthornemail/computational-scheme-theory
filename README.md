# Computational Scheme Theory

**Empirical Validation of Computational Scheme Theory**

This project implements a system to empirically validate the core hypothesis of Computational Scheme Theory:

**H¹(X_Comp, O_Comp) = V(G) - k**

This hypothesis claims that the topological complexity of a program (measured by Čech cohomology H¹) equals its traditional cyclomatic complexity (V(G)) with a small normalization constant k.

## Project Status

**Status**: ✅ **Phase 1 Core Implementation COMPLETE** 

All core algorithms and infrastructure have been implemented:
- ✅ Algorithm 1-4 (Pure Racket) - Complete unified pipeline for H¹ computation
- ✅ V(G) Calculator (Racket) - Cyclomatic complexity computation
- ✅ Validation Coordinator (Python) - Hypothesis testing
- ✅ Test Corpus (15 programs) - Ready for expansion to 50

**Next**: Service integration and initial validation experiments

See `PROJECT_COMPLETE.md` for implementation summary.

## Quick Start

### Prerequisites

- **Racket** 8.0+ (Primary implementation)
- **Python** 3.9+ (Optional: for coordinator)
- **Docker** and **Docker Compose** (Optional)
- **Protocol Buffers** compiler (`protoc`) (Optional)
- **Git**

### Setup Environment

1. **Verify prerequisites**:
   ```bash
   ./scripts/verify-env.sh
   ```

2. **Install all dependencies**:
   ```bash
   make setup
   ```

3. **Build all components**:
   ```bash
   make build
   ```

4. **Run tests**:
   ```bash
   make test
   ```

### Development Workflow

```bash
# Start Docker services (if needed)
make docker-up

# Generate protocol buffer code
make proto

# Build specific component
cd haskell-core && cabal build
cd racket-metrics && raco test .
cd python-coordinator && pytest

# Clean build artifacts
make clean
```

## Project Structure

```
computational-scheme-theory/
├── racket-unified/          # Pure Racket unified implementation (current)
├── racket-metrics/         # Racket V(G) calculator
├── python-coordinator/     # Python orchestration service
├── proto/                  # Protocol buffer definitions
├── test-corpus/            # Test programs for validation
├── scripts/                # Utility scripts
├── docs/                   # Documentation
│   └── 10 - IMPLEMENTATION/  # Implementation plans
├── docker-compose.yml      # Docker services
├── Makefile                # Build automation
└── README.md               # This file
```

## Architecture Overview

The system consists of three main services:

1. **Unified Lisp Substrate** (`racket-unified/`)
   - Pure Racket implementation of all four algorithms
   - Complete unified system: M/S-expressions, Prolog/Datalog, Y/Z-combinators
   - See `racket-unified/README.md`

2. **Racket Metrics Calculator** (`racket-metrics/`)
   - Parses R5RS Scheme and computes cyclomatic complexity V(G)
   - See `racket-metrics/README.md`

3. **Python Coordinator** (`python-coordinator/`)
   - Orchestrates validation experiments
   - Tests hypothesis: H¹ = V(G) - k
   - See `python-coordinator/README.md`

## Documentation

### Implementation Plans

- **[00-IMPLEMENTATION-OVERVIEW.md](docs/10%20-%20IMPLEMENTATION/00-IMPLEMENTATION-OVERVIEW.md)** - Complete implementation overview
- **[01-mathematical-core-architecture.md](docs/10%20-%20IMPLEMENTATION/01-mathematical-core-architecture.md)** - Haskell core design
- **[02-metrics-calculator-design.md](docs/10%20-%20IMPLEMENTATION/02-metrics-calculator-design.md)** - Racket calculator design
- **[03-grpc-service-architecture.md](docs/10%20-%20IMPLEMENTATION/03-grpc-service-architecture.md)** - Service architecture
- **[04-test-corpus-design.md](docs/10%20-%20IMPLEMENTATION/04-test-corpus-design.md)** - Test corpus design
- **[05-four-layer-architecture-integration.md](docs/10%20-%20IMPLEMENTATION/05-four-layer-architecture-integration.md)** - Phase-based architecture
- **[06-project-roadmap.md](docs/10%20-%20IMPLEMENTATION/06-project-roadmap.md)** - 16-month timeline

### Theory Documentation

See `docs/` for comprehensive theoretical documentation:
- Research and analysis (folders 01-02)
- Formalizations and specifications (folders 05)
- Proposals and explanations (folders 06, 08)

## Phase 1 Timeline (Months 1-4)

### Month 1: Haskell Core Setup (Week 1-2)
- ✅ Project structure created
- ⏳ Initialize Haskell project (Week 1)
- ⏳ Implement Algorithm 1 (Week 3-4)

### Month 2: Topology and Complex
- ⏳ Implement Algorithm 2 (Scope topology)
- ⏳ Implement Algorithm 3 (Čech complex)

### Month 3: Cohomology and Metrics
- ⏳ Implement Algorithm 4 (Cohomology)
- ⏳ Implement Racket V(G) calculator

### Month 4: Integration and Validation
- ⏳ Set up gRPC services
- ⏳ Build Python coordinator
- ⏳ Create 50-program test corpus
- ⏳ Run initial validation

## Makefile Targets

```bash
make help          # Show available targets
make setup         # Install all dependencies
make build         # Build all components
make test          # Run all tests
make clean         # Clean build artifacts
make proto         # Generate protocol buffer code
make docker-up     # Start Docker services
make docker-down   # Stop Docker services
make verify-env    # Verify development environment
```

## Contributing

This is a research project. See implementation documentation for:
- Code review process
- Testing requirements
- Development workflow

## License

MIT License - See LICENSE file (to be added)

## Citation

If you use this software in your research, please cite:

```bibtex
@software{computational-scheme-theory,
  title = {Computational Scheme Theory - Empirical Validation},
  author = {Computational Scheme Theory Research Team},
  year = {2025},
  url = {https://github.com/your-org/computational-scheme-theory}
}
```

## Contact

For questions about implementation:
- See `docs/10 - IMPLEMENTATION/` for detailed design docs
- Check individual component README files

---

**Ready to Start?** See `docs/10 - IMPLEMENTATION/06-project-roadmap.md` for Week 1 tasks.
