#lang racket/base

;; Public API for the Unified Lisp Substrate
(require "algorithms/unified-pipeline.rkt"
         "bridge/racket-bridge.rkt")

(provide
 ;; Main H¹ computation
 compute-h1-from-source
 compute-h1-from-source-detailed
 pipeline-result?
 pipeline-result-h1-value
 pipeline-result-h1
 pipeline-result-beta0
 pipeline-result-beta1
 pipeline-result-num-bindings
 pipeline-result-num-simplices0
 pipeline-result-num-simplices1
 pipeline-result-num-simplices2
 pipeline-result-success
 pipeline-result-error-message
 pipeline-result-error
 
 ;; Service bridges
 racket-service-available?
 call-racket-vg
 validate-hypothesis

 ;; Configuration
 *racket-service-url*)

;; ============================================================
;; PUBLIC API
;; ============================================================
;;
;; Main entry point:
;;
;;   (compute-h1-from-source-detailed source)
;;     → pipeline-result
;;
;;   (compute-h1-from-source source)
;;     → (values h1-value error-message)
;;
;; Service integration:
;;
;;   (call-racket-vg source)
;;     → (values vg-value error-message)
;;
;; Validation:
;;
;;   (validate-hypothesis h1 vg [k] [tolerance])
;;     → (values valid? diff message)
;;
;; ============================================================

(module+ example
  (displayln "Example usage:")
  (displayln "")
  
  ;; Basic usage
  (displayln "1. Basic H¹ computation:")
  (let ([result (compute-h1-from-source-detailed "(lambda (x) x)")])
    (when (pipeline-result-success result)
      (printf "   H¹ = ~a\n" (pipeline-result-h1 result))
      (printf "   Bindings: ~a\n" (pipeline-result-num-bindings result))))
  
  (displayln "")
  (displayln "2. Hypothesis validation:")
  (when (racket-service-available?)
    (let-values ([(h1 error1) (compute-h1-from-source "(lambda (x) x)")]
                 [(vg error2) (call-racket-vg "(lambda (x) x)")])
      (when (and h1 vg)
        (let-values ([(valid? diff msg) (validate-hypothesis h1 vg 0 0)])
          (printf "   H¹ = ~a, V(G) = ~a\n" h1 vg)
          (printf "   ~a\n" msg))))))

