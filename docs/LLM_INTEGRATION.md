# LLM Integration Guide

**LLM-Enhanced Natural Language Processing for Computational Scheme Theory**

This document describes how to set up and use LLM (Large Language Model) integration with the Computational Scheme Theory NLP system.

---

## Overview

The LLM integration enhances the existing rule-based SGP-ASLN parser with neural language model capabilities, providing:

- **Better query understanding**: Handle varied phrasings and ambiguous queries
- **Natural language explanations**: Generate human-readable explanations of results
- **Conversational interface**: Maintain context across multiple queries
- **Semantic search**: Find similar queries and documents using embeddings
- **Hybrid reliability**: Fallback to deterministic rule-based parsing when needed

---

## Architecture

The LLM integration follows a hybrid approach:

```
User Query
    ↓
LLM Classification (if available)
    ↓
Confidence Check (≥0.7)
    ↓
High Confidence → Use LLM result → Execute → Generate Explanation
    ↓
Low Confidence → Fallback to Rule-Based → Execute
```

### Components

1. **Python LLM Bridge** (`python-coordinator/coordinator/llm_bridge.py`)
   - Abstract `LLMInterface` with provider implementations
   - Supports OpenAI, local models (transformers), and Ollama

2. **LLM Integration** (`python-coordinator/coordinator/llm_integration.py`)
   - `HybridNLPEngine`: Combines LLM + rule-based parsing
   - `EnhancedNLPCoordinator`: Main coordinator with LLM capabilities

3. **Racket Bridge** (`racket-unified/src/nlp/llm-bridge.rkt`)
   - Subprocess interface to Python LLM service
   - Optional enhancement for Racket NLP queries

---

## Setup

### Prerequisites

- Python 3.9+
- Racket (for unified pipeline)
- Optional: LLM provider credentials/model

### Installation

1. **Install Python dependencies:**

```bash
cd python-coordinator
pip install -r requirements.txt
```

2. **Choose LLM provider:**

#### Option A: OpenAI API (Recommended for Quick Start)

```bash
# Set API key
export OPENAI_API_KEY="your-api-key-here"

# Install OpenAI package (if not already installed)
pip install openai>=1.0.0
```

#### Option B: Local Models (Transformers)

```bash
# Install transformers and PyTorch
pip install transformers>=4.35.0 torch>=2.0.0

# Optional: Install sentence-transformers for better embeddings
pip install sentence-transformers>=2.2.0
```

Models will be downloaded automatically on first use. Recommended models:
- `microsoft/Phi-3-mini-4k-instruct` (small, fast)
- `meta-llama/Llama-2-7b-chat-hf` (larger, more capable)
- `mistralai/Mistral-7B-Instruct-v0.2` (balanced)

#### Option C: Ollama (Local LLM Service)

```bash
# Install Ollama (see https://ollama.ai)
# Then run Ollama service
ollama serve

# Pull a model
ollama pull llama2

# Python code uses requests (already in requirements.txt)
```

---

## Usage

### Basic Usage

```python
from coordinator.llm_integration import EnhancedNLPCoordinator

# Initialize with OpenAI (default)
coordinator = EnhancedNLPCoordinator(llm_type="openai")

# Process natural language query
result = coordinator.process_nl_query("compute H1 for program test")
print(result["explanation"])  # Natural language explanation
```

### Provider Selection

```python
# OpenAI (requires OPENAI_API_KEY env var)
coordinator = EnhancedNLPCoordinator(llm_type="openai")

# Local model (requires transformers)
coordinator = EnhancedNLPCoordinator(
    llm_type="local",
    model_name="microsoft/Phi-3-mini-4k-instruct"
)

# Ollama (requires Ollama service running)
coordinator = EnhancedNLPCoordinator(
    llm_type="ollama",
    base_url="http://localhost:11434",
    model="llama2"
)

# Rule-based only (no LLM)
coordinator = EnhancedNLPCoordinator(llm_type="none")
```

### Intent Classification

```python
# Get LLM intent classification
result = coordinator.nlp_engine.process_query("calculate H1 for test")

print(f"Operation: {result['operation']}")
print(f"Confidence: {result['confidence']}")
print(f"Reasoning: {result['reasoning']}")
```

### Natural Language Explanations

```python
# Generate explanation for computation result
operation = "computeH1"
result = {"h1": 1, "num_bindings": 3}

explanation = coordinator.nlp_engine.generate_explanation(operation, result)
print(explanation)
```

### Conversational Interface

```python
# Chat with context
response1 = coordinator.chat("What is H1?")
response2 = coordinator.chat("How do I compute it?")  # Maintains context
```

### Semantic Search

```python
documents = [
    "Compute H1 cohomology",
    "Calculate cyclomatic complexity",
    "Validate hypothesis"
]

results = coordinator.nlp_engine.semantic_search(
    "find cohomology",
    documents,
    top_k=3
)

for doc, score in results:
    print(f"[{score:.3f}] {doc}")
```

---

## Racket Integration

### Enhanced NLP Queries

Use LLM-enhanced parsing from Racket:

```racket
(require "nlp-integration.rkt")

;; Enhanced query with LLM support
(execute-nl-query-enhanced "compute H1 for program test")
```

### LLM Bridge Functions

```racket
(require "nlp/llm-bridge.rkt")

;; Check if LLM available
(llm-available?)  ; → #t or #f

;; Enhanced parsing
(llm-enhanced-parse "calculate H1")

;; Generate explanation
(llm-generate-response "computeH1" result)

;; Intent classification
(llm-classify-intent "what is the complexity?")
```

---

## Configuration

### Environment Variables

- `OPENAI_API_KEY`: OpenAI API key (for OpenAI provider)
- `LLM_TYPE`: Provider type (`openai`, `local`, `ollama`, `none`)
- `LLM_MODEL`: Model name (for local/Ollama providers)

### Provider-Specific Settings

#### OpenAI

```python
coordinator = EnhancedNLPCoordinator(
    llm_type="openai",
    api_key="sk-...",  # Optional if OPENAI_API_KEY set
    model="gpt-4"       # or "gpt-3.5-turbo"
)
```

#### Local Models

```python
coordinator = EnhancedNLPCoordinator(
    llm_type="local",
    model_name="microsoft/Phi-3-mini-4k-instruct"
)
```

#### Ollama

```python
coordinator = EnhancedNLPCoordinator(
    llm_type="ollama",
    base_url="http://localhost:11434",
    model="llama2"
)
```

---

## Hybrid Processing Flow

1. **User submits query** → `EnhancedNLPCoordinator.process_nl_query()`

2. **LLM classification** → Extracts operation, parameters, confidence

3. **Confidence check**:
   - **High confidence (≥0.7)**: Use LLM result
   - **Low confidence (<0.7)**: Fallback to Racket rule-based parsing

4. **Execute computation** → Call Racket unified pipeline

5. **Generate explanation** → LLM generates natural language response

6. **Return result** → Combined result with explanation

---

## Intent Classification Format

LLM returns JSON with:

```json
{
  "operation": "computeH1|computeVG|validateHypothesis|...",
  "program_name": "extracted identifier",
  "parameters": {"k": 0, ...},
  "confidence": 0.0-1.0,
  "reasoning": "brief explanation"
}
```

### Supported Operations

- `computeH1`: Compute H¹ cohomology
- `computeVG`: Compute cyclomatic complexity
- `validateHypothesis`: Validate H¹ = V(G) - k
- `analyzePatterns`: Analyze program patterns
- `exportPolynomial`: Export polynomial representation
- `getPatternDimensions`: Get pattern dimensions
- `explain`: Generate explanation
- `generate`: Generate code/patterns

---

## Examples

See `python-coordinator/examples/llm_demo.py` for comprehensive examples:

```bash
cd python-coordinator/examples
python llm_demo.py
```

Or set environment variables:

```bash
export OPENAI_API_KEY="your-key"
export LLM_TYPE="openai"
python llm_demo.py
```

---

## Troubleshooting

### LLM Not Available

**Symptom:** Coordinator falls back to rule-based parsing

**Solutions:**
1. Check API key is set: `echo $OPENAI_API_KEY`
2. Verify package installation: `pip list | grep openai`
3. Check network connectivity for API calls
4. Use `llm_type="none"` to disable LLM explicitly

### Low Confidence Scores

**Symptom:** LLM results have low confidence (<0.7)

**Solutions:**
1. Rephrase query more explicitly
2. Use more specific operation names
3. System will automatically fallback to rule-based

### Import Errors

**Symptom:** `ImportError: openai package required`

**Solution:**
```bash
pip install openai>=1.0.0
```

### Racket Bridge Errors

**Symptom:** Racket can't call Python LLM service

**Solutions:**
1. Ensure Python is in PATH: `which python3`
2. Verify coordinator package is installed: `pip install -e python-coordinator/`
3. Check subprocess permissions

---

## Performance Considerations

- **OpenAI API**: Fast (~1-2s), but requires internet and API key
- **Local models**: Slower first run (model download), then faster for repeated queries
- **Rule-based fallback**: Instant, deterministic, no external dependencies

**Recommendation:** Use OpenAI for development, local models for production (if GPU available), rule-based as reliable fallback.

---

## Security Notes

- **API Keys**: Never commit API keys to version control. Use environment variables.
- **Local Models**: Models are large (GBs). Ensure sufficient disk space.
- **Network**: OpenAI API calls go over internet. Ensure secure connection.

---

## Future Enhancements

Potential improvements:

- [ ] Caching of LLM responses
- [ ] Fine-tuning on domain-specific queries
- [ ] Multi-model ensemble for higher accuracy
- [ ] Streaming responses for long explanations
- [ ] Custom prompt templates per operation

---

## References

- **SGP-ASLN Documentation**: See `racket-unified/src/nlp/README.md`
- **Python Coordinator**: See `python-coordinator/README.md`
- **Unified Pipeline**: See `racket-unified/src/algorithms/unified-pipeline.rkt`

---

**Last Updated:** 2025-01-31

