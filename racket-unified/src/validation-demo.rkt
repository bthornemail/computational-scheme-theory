#lang racket/base

;; Validation demo: Compare Lisp results with existing services
(require "algorithms/unified-pipeline.rkt"
         "bridge/racket-bridge.rkt")

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         VALIDATION DEMO: Lisp vs Services              ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  (define test-sources
    '("(lambda (x) x)"
      "(let ((x 1) (y 2)) (+ x y))"
      "(lambda (x) (lambda (y) (+ x y)))"))
  
  (displayln "Service Status:")
  (displayln (format "  Racket service: ~a" (if (racket-service-available?) "✓ Available" "✗ Unavailable")))
  (displayln "")
  
  (for ([source test-sources])
    (displayln (format "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
    (displayln (format "Source: ~a" source))
    (displayln "")
    
    ;; Lisp computation
    (let ([result (compute-h1-from-source-detailed source)])
      (if (pipeline-result-success result)
          (begin
            (displayln (format "Lisp Result:"))
            (displayln (format "  H¹ = ~a" (pipeline-result-h1 result)))
            (displayln (format "  Bindings: ~a" (pipeline-result-num-bindings result)))
            (displayln (format "  Simplices: 0:~a, 1:~a, 2:~a"
                               (pipeline-result-num-simplices0 result)
                               (pipeline-result-num-simplices1 result)
                               (pipeline-result-num-simplices2 result)))
            
            ;; Compare with services
            (displayln "")
            (displayln "Service Comparison:")
            
            ;; Racket comparison
            (when (racket-service-available?)
              (let-values ([(racket-vg error) (call-racket-vg source)])
                (if racket-vg
                    (let-values ([(valid? diff msg) (validate-hypothesis
                                                      (pipeline-result-h1 result)
                                                      racket-vg
                                                      0
                                                      0)])
                      (displayln (format "  Racket V(G) = ~a" racket-vg))
                      (displayln (format "    ~a" msg)))
                    (displayln (format "  Racket error: ~a" error)))))
            
            (displayln ""))
          (displayln (format "✗ Error: ~a" (pipeline-result-error result))))))
  
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║              ✓ VALIDATION COMPLETE ✓                    ║")
  (displayln "╚══════════════════════════════════════════════════════════╝"))

