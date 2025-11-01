# Python Coordinator Service

The Python coordinator orchestrates validation experiments by using the unified Racket implementation for both H¹ and V(G) computation.

## Overview

The coordinator service:

1. Receives validation requests (single program or corpus)
2. Calls Racket unified pipeline to compute H¹
3. Calls Racket unified pipeline to compute V(G)
4. Tests hypothesis: H¹ = V(G) - k
5. Returns validation results with statistics

**Note**: Both H¹ and V(G) are now computed using the unified Racket implementation (`racket-unified/src/algorithms/`). The direct computation mode uses subprocess calls to Racket scripts.

## Requirements

- **Python** 3.9 or later
- **pip** (Python package installer)

## Quick Start

### Setup Virtual Environment

```bash
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
```

### Install Package (Development)

```bash
pip install -e .
```

### Run Tests

```bash
pytest
```

### Generate Protocol Buffer Code

```bash
make proto  # From project root
```

### Run Coordinator Service

```bash
python -m coordinator.service
```

Or after installation:

```bash
coordinator --help
```

## Project Structure

```
python-coordinator/
├── coordinator/
│   ├── __init__.py
│   ├── service.py           # gRPC service implementation
│   ├── fsm.py               # FSM for validation (Phase 2)
│   ├── event_store.py       # Event store client (Phase 2)
│   └── validation.py        # Validation logic
├── tests/
│   ├── test_service.py
│   ├── test_validation.py
│   └── test_integration.py
├── requirements.txt
├── setup.py
└── README.md
```

## Development

### Running Individual Tests

```bash
pytest tests/test_service.py
pytest tests/test_validation.py -v
```

### Code Formatting

```bash
black coordinator/ tests/
```

### Type Checking

```bash
mypy coordinator/
```

### Linting

```bash
flake8 coordinator/ tests/
```

## Protocol Buffer Code Generation

Protocol buffer code is generated into `coordinator/proto/`:

```bash
# From project root
make proto
```

This generates Python code from `.proto` files in the `proto/` directory.

## Architecture

See `docs/10 - IMPLEMENTATION/03-grpc-service-architecture.md` for detailed service architecture.

## Phase 1 MVP Notes

- Racket service uses JSON over HTTP (gRPC in Week 13 if available)
- No event sourcing or FSM yet (Phase 2)
- Simple sequential validation for first 50 programs

## Next Steps

1. Implement coordinator service (Month 4, Week 1)
2. Add integration tests (Month 4, Week 1-2)
3. Create test corpus runner (Month 4, Week 2)
4. Add event sourcing and FSM (Phase 2)

