# Service Bridges

This directory contains bridges to existing Haskell and Racket services for hybrid operation during the migration period.

## Haskell Bridge (`haskell-bridge.rkt`)

Provides HTTP client to call the existing Haskell H¹ computation service:

- `call-haskell-h1 source` - Compute H¹ using Haskell service
- `compare-h1-results lisp-result haskell-result tolerance` - Compare results
- `haskell-service-available?` - Check service health

**Configuration**: Set `*haskell-service-url*` parameter (default: `http://localhost:8080/api/compute-h1`)

## Racket Bridge (`racket-bridge.rkt`)

Provides HTTP client to call the existing Racket V(G) metrics service:

- `call-racket-vg source` - Compute V(G) using Racket service
- `validate-hypothesis h1 vg k tolerance` - Test H¹ = V(G) - k
- `racket-service-available?` - Check service health

**Configuration**: Set `*racket-service-url*` parameter (default: `http://localhost:8081/api/compute-vg`)

## Usage

The bridges are automatically used by the unified pipeline when services are available. Results are compared for validation.

```racket
(require "bridge/haskell-bridge.rkt")
(require "bridge/racket-bridge.rkt")

;; Check services
(haskell-service-available?)  ; → #t if service is up
(racket-service-available?)    ; → #t if service is up

;; Compare results
(let ([source "(lambda (x) x)"])
  (let-values ([(lisp-h1 _) (compute-h1-from-source source)]
               [(haskell-h1 _) (call-haskell-h1 source)]
               [(racket-vg _) (call-racket-vg source)])
    (compare-h1-results lisp-h1 haskell-h1 0)
    (validate-hypothesis lisp-h1 racket-vg 0 0)))
```

## Migration Strategy

During migration:
1. Pure Lisp computation runs first
2. If services available, call them for comparison
3. Results are validated against each other
4. Gradually phase out service calls as Lisp implementation matures

