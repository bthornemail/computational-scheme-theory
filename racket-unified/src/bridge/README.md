# Service Bridges

This directory contains bridges to existing services for hybrid operation during the migration period.

## Racket Bridge (`racket-bridge.rkt`)

Provides HTTP client to call the existing Racket V(G) metrics service:

- `call-racket-vg source` - Compute V(G) using Racket service
- `validate-hypothesis h1 vg k tolerance` - Test H¹ = V(G) - k
- `racket-service-available?` - Check service health

**Configuration**: Set `*racket-service-url*` parameter (default: `http://localhost:8081/api/compute-vg`)

## Usage

The bridge is automatically used by the unified pipeline when the service is available. Results are used for hypothesis validation (H¹ = V(G) - k).

```racket
(require "bridge/racket-bridge.rkt")

;; Check service
(racket-service-available?)  ; → #t if service is up

;; Compare results
(let ([source "(lambda (x) x)"])
  (let-values ([(lisp-h1 _) (compute-h1-from-source source)]
               [(racket-vg _) (call-racket-vg source)])
    (validate-hypothesis lisp-h1 racket-vg 0 0)))
```

## Migration Strategy

During migration:
1. Pure Lisp computation runs first
2. If Racket service available, call it for hypothesis validation
3. Results are validated using H¹ = V(G) - k
4. Gradually phase out service calls as Lisp implementation matures
