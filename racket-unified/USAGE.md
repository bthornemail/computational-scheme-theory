# Usage Guide

## Quick Start

### Run Complete Demo

```bash
cd racket-unified
racket src/main.rkt
```

This demonstrates:
- M/S-expression pipeline
- Complete H¹ computation
- Service bridge integration (if services available)

### Run Validation Demo

```bash
racket src/validation-demo.rkt
```

Compares Lisp results with existing Haskell/Racket services.

### Run Tests

```bash
racket test/run-tests.rkt
```

Or use Racket's test runner:

```bash
raco test test/
```

## Programmatic Usage

### Compute H¹ from Source

```racket
(require "src/algorithms/unified-pipeline.rkt")

;; Simple computation
(let-values ([(h1 error) (compute-h1-from-source "(lambda (x) x)")])
  (if error
      (printf "Error: ~a\n" error)
      (printf "H¹ = ~a\n" h1)))

;; Detailed computation
(let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
  (printf "H¹: ~a\n" (pipeline-result-h1 result))
  (printf "Bindings: ~a\n" (pipeline-result-num-bindings result))
  (printf "0-simplices: ~a\n" (pipeline-result-num-simplices0 result)))
```

### Use Service Bridges

```racket
(require "src/bridge/haskell-bridge.rkt")
(require "src/bridge/racket-bridge.rkt")

;; Check service availability
(if (haskell-service-available?)
    (printf "Haskell service is up\n")
    (printf "Haskell service is down\n"))

;; Call Haskell service
(let-values ([(h1 error) (call-haskell-h1 "(lambda (x) x)")])
  (if error
      (printf "Error: ~a\n" error)
      (printf "Haskell H¹ = ~a\n" h1)))

;; Compare results
(let-values ([(match? diff msg) 
               (compare-h1-results lisp-h1 haskell-h1 0)])
  (printf "~a\n" msg))
```

### Validate Hypothesis

```racket
(require "src/bridge/racket-bridge.rkt")

;; Validate H¹ = V(G) - k
(let-values ([(valid? diff msg) 
               (validate-hypothesis h1-value vg-value 0 0)])
  (if valid?
      (printf "✓ Hypothesis validated: ~a\n" msg)
      (printf "✗ Hypothesis violated: ~a\n" msg)))
```

## Configuration

### Service URLs

Set custom service URLs:

```racket
(require "src/bridge/haskell-bridge.rkt")
(require "src/bridge/racket-bridge.rkt")

(*haskell-service-url* "http://custom-host:8080/api/compute-h1")
(*racket-service-url* "http://custom-host:8081/api/compute-vg")
```

## Examples

### Example 1: Complete Pipeline

```racket
(require "src/algorithms/unified-pipeline.rkt")

(define source "(lambda (x) (lambda (y) (+ x y)))")
(let ([result (compute-h1-from-source-detailed source)])
  (if (pipeline-result-success result)
      (begin
        (printf "H¹ = ~a\n" (pipeline-result-h1 result))
        (printf "β₀ = ~a, β₁ = ~a\n" 
                (pipeline-result-beta0 result)
                (pipeline-result-beta1 result))
        (printf "Simplices: ~a vertices, ~a edges, ~a triangles\n"
                (pipeline-result-num-simplices0 result)
                (pipeline-result-num-simplices1 result)
                (pipeline-result-num-simplices2 result)))
      (printf "Error: ~a\n" (pipeline-result-error result))))
```

### Example 2: Service Comparison

```racket
(require "src/algorithms/unified-pipeline.rkt"
         "src/bridge/haskell-bridge.rkt")

(define source "(lambda (x) x)")
(let ([lisp-result (compute-h1-from-source-detailed source)])
  (when (and (pipeline-result-success lisp-result)
             (haskell-service-available?))
    (let-values ([(haskell-h1 error) (call-haskell-h1 source)])
      (if haskell-h1
          (let-values ([(match? diff msg) 
                         (compare-h1-results
                          (pipeline-result-h1 lisp-result)
                          haskell-h1
                          0)])
            (printf "Lisp H¹: ~a\n" (pipeline-result-h1 lisp-result))
            (printf "Haskell H¹: ~a\n" haskell-h1)
            (printf "Comparison: ~a\n" msg))
          (printf "Haskell error: ~a\n" error)))))
```

### Example 3: Hypothesis Validation

```racket
(require "src/algorithms/unified-pipeline.rkt"
         "src/bridge/racket-bridge.rkt")

(define source "(lambda (x) x)")
(let* ([h1-result (compute-h1-from-source-detailed source)]
       [h1-value (pipeline-result-h1 h1-result)])
  (when (and (pipeline-result-success h1-result)
             (racket-service-available?))
    (let-values ([(vg-value error) (call-racket-vg source)])
      (if vg-value
          (let-values ([(valid? diff msg) 
                         (validate-hypothesis h1-value vg-value 0 0)])
            (printf "H¹ = ~a\n" h1-value)
            (printf "V(G) = ~a\n" vg-value)
            (printf "Validation: ~a\n" msg))
          (printf "Racket error: ~a\n" error)))))
```

## Error Handling

All functions return `(values result error)` or structured results:

- `compute-h1-from-source` → `(values h1 #f)` or `(values #f "error message")`
- `compute-h1-from-source-detailed` → `pipeline-result` struct
- `call-haskell-h1` → `(values h1 #f)` or `(values #f "error message")`
- `call-racket-vg` → `(values vg #f)` or `(values #f "error message")`

Always check for errors:

```racket
(let-values ([(result error) (compute-h1-from-source source)])
  (if error
      (handle-error error)
      (use-result result)))
```

## Troubleshooting

### Services Not Available

If services are unavailable, the system works in pure Lisp mode:

```racket
;; This always works (pure Lisp)
(compute-h1-from-source-detailed "(lambda (x) x)")

;; This only works if service is up
(when (haskell-service-available?)
  (call-haskell-h1 source))
```

### Parse Errors

If source code doesn't parse:

```racket
(let ([result (compute-h1-from-source-detailed "invalid syntax")])
  (if (pipeline-result-success result)
      (use-result result)
      (printf "Parse error: ~a\n" (pipeline-result-error result))))
```

