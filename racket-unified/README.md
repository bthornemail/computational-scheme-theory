# Unified Lisp Substrate

**A complete computational scheme theory implementation in pure Racket**

## 🎯 Vision

**"Everything is Lisp"** - A unified substrate where all components (M/S-expressions, Prolog/Datalog, Y/Z-combinators, algorithms) exist natively in pure Lisp.

## ✨ Features

- ✅ **Pure Lisp Implementation** - No FFI, single runtime
- ✅ **Complete Algorithms** - All 4 algorithms working (100% test success)
- ✅ **M/S-Expressions** - Native homoiconicity
- ✅ **Prolog/Datalog** - Embedded logic engines
- ✅ **Y/Z Combinators** - Native fixed-point recursion
- ✅ **Service Bridges** - Hybrid operation with existing services
- ✅ **Comprehensive Tests** - 100% test success rate
- ✅ **Extensive Documentation** - 22 documentation files

## 🚀 Quick Start

```bash
# Run complete demo
racket src/main.rkt

# Run validation
racket src/validation-demo.rkt

# Run tests
racket test/run-tests.rkt

# Validate corpus (if h1_values.json exists)
racket test/corpus-validation.rkt
```

## 📖 Usage

```racket
(require "src/api.rkt")

;; Compute H¹ from Scheme source
(let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
  (when (pipeline-result-success result)
    (printf "H¹ = ~a\n" (pipeline-result-h1 result))
    (printf "Bindings: ~a\n" (pipeline-result-num-bindings result))))
```

See [USAGE.md](USAGE.md) for detailed documentation.

## 📊 Test Results

**100% Success Rate** ✅

| Test Case | H¹ | Bindings | Status |
|-----------|-----|----------|--------|
| Simple lambda | 0 | 1 | ✅ |
| Let binding | 1 | 2 | ✅ |
| Nested lambdas | 0 | 2 | ✅ |

## 📁 Structure

```
racket-unified/
├── src/
│   ├── core/              # M/S-expressions, combinators
│   ├── algorithms/        # Algorithms 1-4
│   ├── bridge/            # Service bridges
│   └── api.rkt            # Public API
├── test/                  # Test suites
└── docs/                  # Documentation
```

## 🔧 Components

### Core
- **M-expressions**: Meta-language commands
- **S-expressions**: Object-language events
- **Y/Z combinators**: Lazy/eager fixed points

### Algorithms
1. **Binding extraction**: R5RS parser, alpha conversion
2. **Scope topology**: Enhanced visibility regions
3. **Čech complex**: Nerve computation
4. **Cohomology**: H¹ calculation

### Logic Engines
- **Prolog**: Top-down validation (custom, ready for miniKanren)
- **Datalog**: Bottom-up inference (custom with Z-combinator)

### Service Bridges
- **Haskell bridge**: Call existing H¹ service
- **Racket bridge**: Call existing V(G) service

## 📚 Documentation

- [QUICK_START.md](QUICK_START.md) - Get started quickly
- [ARCHITECTURE.md](ARCHITECTURE.md) - System architecture
- [USAGE.md](USAGE.md) - Detailed usage guide
- [DEPLOYMENT.md](DEPLOYMENT.md) - Deployment guide
- [INDEX.md](INDEX.md) - Documentation index

## 🎉 Status

**✅ PRODUCTION READY**

- All algorithms implemented ✅
- All tests passing (100%) ✅
- Complete documentation ✅
- Service bridges working ✅

## 🔮 Future

- [ ] Upgrade Prolog to miniKanren (optional)
- [ ] Performance optimization
- [ ] Web UI integration

## 📄 License

Part of the Computational Scheme Theory project.

---

**"In the beginning was the Word, and the Word was with Lisp, and the Word was Lisp."**
