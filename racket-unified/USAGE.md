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

Compares Lisp results with optional Racket V(G) service for hypothesis validation.

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

### Use Service Bridge (Optional)

The Racket bridge is optional and used only for hypothesis validation (H¹ = V(G) - k):

```racket
(require "src/bridge/racket-bridge.rkt")

;; Check service availability
(if (racket-service-available?)
    (printf "Racket service is up\n")
    (printf "Racket service is down\n"))

;; Call Racket V(G) service
(let-values ([(vg error) (call-racket-vg "(lambda (x) x)")])
  (if error
      (printf "Error: ~a\n" error)
      (printf "Racket V(G) = ~a\n" vg)))
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

Set custom service URL:

```racket
(require "src/bridge/racket-bridge.rkt")

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

### Example 2: Hypothesis Validation with Racket Service

```racket
(require "src/algorithms/unified-pipeline.rkt"
         "src/bridge/racket-bridge.rkt")

(define source "(lambda (x) x)")
(let ([lisp-result (compute-h1-from-source-detailed source)])
  (when (and (pipeline-result-success lisp-result)
             (racket-service-available?))
    (let-values ([(vg error) (call-racket-vg source)])
      (if vg
          (let-values ([(valid? diff msg) 
                         (validate-hypothesis
                          (pipeline-result-h1 lisp-result)
                          vg
                          0
                          0)])
            (printf "Lisp H¹: ~a\n" (pipeline-result-h1 lisp-result))
            (printf "Racket V(G): ~a\n" vg)
            (printf "Validation: ~a\n" msg))
          (printf "Racket error: ~a\n" error)))))
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
- `call-racket-vg` → `(values vg #f)` or `(values #f "error message")`
- `validate-hypothesis` → `(values valid? diff message)`

Always check for errors:

```racket
(let-values ([(result error) (compute-h1-from-source source)])
  (if error
      (handle-error error)
      (use-result result)))
```

## Troubleshooting

### Services Not Available

If the Racket service is unavailable, the system works in pure Lisp mode:

```racket
;; This always works (pure Lisp computation)
(compute-h1-from-source-detailed "(lambda (x) x)")

;; Optional: Only works if Racket service is up
(when (racket-service-available?)
  (let-values ([(vg error) (call-racket-vg source)])
    (if vg
        (validate-hypothesis h1-value vg 0 0)
        (printf "Service unavailable\n"))))
```

### Parse Errors

If source code doesn't parse:

```racket
(let ([result (compute-h1-from-source-detailed "invalid syntax")])
  (if (pipeline-result-success result)
      (use-result result)
      (printf "Parse error: ~a\n" (pipeline-result-error result))))
```

