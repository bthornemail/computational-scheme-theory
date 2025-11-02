# Python Coordinator Service

The Python coordinator orchestrates validation experiments by using the unified Racket implementation for both H¹ and V(G) computation. It also provides LLM-enhanced natural language processing capabilities for improved query understanding and natural language explanations.

## Overview

The coordinator service:

1. Receives validation requests (single program or corpus)
2. Calls Racket unified pipeline to compute H¹
3. Calls Racket unified pipeline to compute V(G)
4. Tests hypothesis: H¹ = V(G) - k
5. Returns validation results with statistics
6. **LLM Integration**: Provides enhanced NLP with intent classification, natural language explanations, and conversational interfaces

**Note**: Both H¹ and V(G) are now computed using the unified Racket implementation (`racket-unified/src/algorithms/`). The direct computation mode uses subprocess calls to Racket scripts. LLM features are optional and work alongside the existing rule-based SGP-ASLN parser.

## Requirements

- **Python** 3.9 or later
- **pip** (Python package installer)
- **Racket** (for unified pipeline computation)
- **Optional LLM dependencies** (see LLM Integration section)

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
│   ├── validation.py        # Validation logic
│   ├── direct_compute.py    # Direct computation coordinator
│   ├── llm_bridge.py        # LLM provider interfaces
│   └── llm_integration.py   # LLM-enhanced NLP coordinator
├── examples/
│   ├── llm_demo.py          # LLM integration demo
│   └── test_ollama.py       # Ollama LLM test suite
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

## LLM Integration

The coordinator includes optional LLM (Large Language Model) integration for enhanced natural language processing capabilities.

### Features

- **Intent Classification**: Understands varied query phrasings and extracts operations
- **Natural Language Explanations**: Generates human-readable explanations of computation results
- **Conversational Interface**: Maintains context across multiple queries
- **Semantic Search**: Finds similar queries and documents using embeddings
- **Hybrid Reliability**: Falls back to deterministic rule-based parsing when needed

### Supported Providers

1. **OpenAI API** (Recommended for quick start)
   - Requires: `openai` package and `OPENAI_API_KEY` environment variable
   - Models: GPT-4, GPT-3.5-turbo, etc.

2. **Local Models** (Transformers)
   - Requires: `transformers`, `torch` packages
   - Models: Phi-3, Llama-2, Mistral, etc.

3. **Ollama** (Local LLM service)
   - Requires: Ollama service running locally
   - Models: Any Ollama-compatible model

### Quick Start with LLM

```bash
# Install LLM dependencies (optional)
pip install openai>=1.0.0  # For OpenAI
# OR
pip install transformers>=4.35.0 torch>=2.0.0  # For local models

# Set API key (for OpenAI)
export OPENAI_API_KEY="your-api-key-here"

# Run LLM demo
python examples/llm_demo.py
```

See [LLM Integration Guide](../../docs/LLM_INTEGRATION.md) for detailed documentation.

### Usage Examples

#### Using Direct Compute Coordinator

```python
from coordinator.direct_compute import DirectComputeCoordinator

coordinator = DirectComputeCoordinator()
result = coordinator.validate_program(
    "test-001",
    "(lambda (x) (if (> x 0) 1 -1))"
)

print(f"H¹={result.h1}, V(G)={result.vg}, Hypothesis holds: {result.hypothesis_holds}")
```

#### Using LLM-Enhanced Coordinator

```python
from coordinator.llm_integration import EnhancedNLPCoordinator

# Initialize with OpenAI (requires OPENAI_API_KEY env var)
coordinator = EnhancedNLPCoordinator(llm_type="openai")

# Process natural language query
result = coordinator.process_nl_query("compute H1 for program test")
print(result["explanation"])  # Natural language explanation

# Or use Ollama (requires Ollama service running)
coordinator = EnhancedNLPCoordinator(
    llm_type="ollama",
    model="gemma3:12b"
)

# Or use local model
coordinator = EnhancedNLPCoordinator(
    llm_type="local",
    model_name="microsoft/Phi-3-mini-4k-instruct"
)
```

#### Conversational Interface

```python
# Chat with context
response1 = coordinator.chat("What is H1?")
response2 = coordinator.chat("How do I compute it?")  # Maintains context
```

#### Intent Classification

```python
# Get LLM intent classification
result = coordinator.nlp_engine.process_query("calculate H1 for test")
print(f"Operation: {result['operation']}")
print(f"Confidence: {result['confidence']}")
```

## Architecture

See `docs/10 - IMPLEMENTATION/03-grpc-service-architecture.md` for detailed service architecture.

For LLM integration architecture, see `docs/LLM_INTEGRATION.md`.

## Phase 1 MVP Notes

- Both H¹ and V(G) use unified Racket implementation
- Direct computation mode available via `DirectComputeCoordinator`
- **LLM integration available** (optional, hybrid with rule-based fallback)
- No event sourcing or FSM yet (Phase 2)
- Simple sequential validation for test corpus

## Next Steps

1. ✅ Implement direct computation coordinator **COMPLETE**
2. ✅ Implement LLM integration **COMPLETE**
3. Add comprehensive integration tests
4. Create automated test corpus runner
5. Add event sourcing and FSM (Phase 2)

