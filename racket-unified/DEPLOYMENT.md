# Deployment Guide

## Requirements

- **Racket** 7.0 or later
- **No external dependencies** (pure Racket)
- **Optional**: Existing Haskell/Racket services for comparison

## Installation

### Option 1: Standalone (Recommended)

No installation needed! Just use Racket:

```bash
cd racket-unified
racket src/main.rkt
```

### Option 2: As Racket Package

```bash
cd racket-unified
raco pkg install --link
```

## Configuration

### Service URLs (Optional)

If using service bridges, configure URLs:

```racket
(require "src/bridge/haskell-bridge.rkt")
(require "src/bridge/racket-bridge.rkt")

(*haskell-service-url* "http://localhost:8080/api/compute-h1")
(*racket-service-url* "http://localhost:8081/api/compute-vg")
```

### Default Configuration

- Haskell service: `http://localhost:8080/api/compute-h1`
- Racket service: `http://localhost:8081/api/compute-vg`

## Running

### Interactive REPL

```bash
racket
> (require "racket-unified/src/api.rkt")
> (compute-h1-from-source-detailed "(lambda (x) x)")
```

### Scripts

```bash
# Main demo
racket src/main.rkt

# Validation
racket src/validation-demo.rkt

# Tests
racket test/run-tests.rkt

# Corpus validation
racket test/corpus-validation.rkt
```

### As Executable

```bash
# Build executable
raco exe src/main.rkt

# Run
./src/main
```

## Docker Deployment (Optional)

```dockerfile
FROM racket/racket:8.11-full
COPY racket-unified/ /app/
WORKDIR /app
CMD ["racket", "src/main.rkt"]
```

## Service Integration

The system works in three modes:

1. **Pure Lisp** (default): All computation in Racket
2. **Hybrid**: Lisp + existing services (for comparison)
3. **Service-only**: Call external services directly

### Enabling Services

1. Start Haskell service:
```bash
# In haskell-core/
stack run
```

2. Start Racket service:
```bash
# In racket-metrics/
racket main.rkt
```

3. Run unified system:
```bash
racket src/main.rkt
```

The system automatically detects and uses available services.

## Performance

### Expected Performance

- Simple programs (< 10 bindings): < 100ms
- Medium programs (10-50 bindings): 100-500ms
- Complex programs (> 50 bindings): 500ms+

### Optimization Tips

1. Use pure Lisp mode (no service calls) for better performance
2. Cache results for repeated computations
3. Use service bridges only for validation

## Troubleshooting

### Services Not Available

If services are unavailable, the system works in pure Lisp mode:

```racket
;; This always works
(compute-h1-from-source-detailed source)

;; This only works if service is up
(when (haskell-service-available?)
  (call-haskell-h1 source))
```

### Parse Errors

If source code doesn't parse:

```racket
(let ([result (compute-h1-from-source-detailed "invalid")])
  (if (pipeline-result-success result)
      (use-result result)
      (printf "Error: ~a\n" (pipeline-result-error result))))
```

### Memory Issues

For large programs:
- Increase Racket heap size: `racket -X 2g src/main.rkt`
- Use streaming for corpus validation

## Production Checklist

- [ ] All tests passing (`racket test/run-tests.rkt`)
- [ ] Documentation reviewed
- [ ] Service URLs configured (if using bridges)
- [ ] Performance tested with expected workload
- [ ] Error handling validated
- [ ] Logging configured (if needed)

## Support

See documentation:
- [USAGE.md](USAGE.md) - Usage guide
- [ARCHITECTURE.md](ARCHITECTURE.md) - System design
- [API Reference](src/api.rkt) - Public API

