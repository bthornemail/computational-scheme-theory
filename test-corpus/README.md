# Test Corpus

This directory contains R5RS Scheme programs used for empirical validation of Computational Scheme Theory.

## Structure

```
test-corpus/
├── baseline/           # 20 programs - Straight-line code (H¹ ≈ 0-1)
├── simple-control/      # 50 programs - Single if/loop (H¹ ≈ 2)
├── recursion/          # 50 programs - Recursive functions
├── complex-control/    # 50 programs - Nested branches/loops
├── functional/         # 50 programs - Higher-order functions
├── call-cc/           # 30 programs - First-class continuations
├── real-programs/     # 100 programs - Open-source Scheme code
└── scripts/           # Corpus generation and validation scripts
```

## Program Format

Each program should have:
1. **Source file** (`.scm`) - R5RS Scheme source code
2. **Metadata file** (`.json`) - Expected values and metadata

### Metadata Schema

```json
{
  "program_id": "baseline-001",
  "category": "baseline",
  "description": "Simple variable definition",
  "expected_h1": 0,
  "expected_vg": 1,
  "features": {
    "recursion": false,
    "call_cc": false,
    "macros": false
  },
  "source": "synthetic",
  "tags": ["simple", "variable"]
}
```

## Corpus Generation

Corpus generation scripts will be added in Month 4, Week 3:

- `scripts/generate_corpus.py` - Generate synthetic programs
- `scripts/fetch_real_programs.py` - Download from GitHub
- `scripts/validate_corpus.py` - Check syntax and metadata
- `scripts/run_validation.py` - Run full validation suite

## Phase 1 Goals

For Month 4, Week 3-4, we aim to create:
- 10 baseline programs
- 20 simple-control programs
- 20 recursion programs
- **Total: 50 programs** for initial validation

Full corpus (350 programs) will be completed in Phase 2-3.

## See Also

- `docs/10 - IMPLEMENTATION/04-test-corpus-design.md` - Detailed corpus design
- `docs/10 - IMPLEMENTATION/00-IMPLEMENTATION-OVERVIEW.md` - Implementation overview

