#lang racket/base

;; Validation suite: Test hypothesis H¹ = V(G) - k
(require "../src/algorithms/unified-pipeline.rkt"
         "../src/bridge/racket-bridge.rkt"
         racket/match)

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         VALIDATION SUITE: H¹ = V(G) - k                ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  (define test-programs
    '(
      ("(lambda (x) x)" "Simple lambda")
      ("(let ((x 1)) x)" "Simple let")
      ("(let ((x 1) (y 2)) (+ x y))" "Multiple bindings")
      ("(lambda (x) (lambda (y) (+ x y)))" "Nested lambdas")
      ("(lambda (x) (let ((y 1)) (+ x y)))" "Lambda with let")
      ("(if #t 1 2)" "Conditional")
    ))
  
  (displayln "Service Status:")
  (displayln (format "  Racket: ~a" (if (racket-service-available?) "✓" "✗")))
  (displayln "")
  
  (define results '())
  
  (for ([test test-programs])
    (let ([source (car test)]
          [name (cadr test)])
      (displayln (format "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
      (displayln (format "Program: ~a" name))
      (displayln (format "Source: ~a" source))
      (displayln "")
      
      ;; Lisp H¹
      (let ([h1-result (compute-h1-from-source-detailed source)])
        (if (pipeline-result-success h1-result)
            (let ([h1-lisp (pipeline-result-h1 h1-result)])
              (displayln (format "Lisp H¹ = ~a" h1-lisp))
              
              ;; Racket V(G)
              (when (racket-service-available?)
                (let-values ([(vg error) (call-racket-vg source)])
                  (if vg
                      (begin
                        (displayln (format "Racket V(G) = ~a" vg))
                        
                        ;; Test hypothesis for k = 0 and k = 1
                        (for ([k (list 0 1)])
                          (let-values ([(valid? diff msg) (validate-hypothesis h1-lisp vg k 0)])
                            (displayln (format "  H¹ = V(G) - ~a: ~a" k (if valid? "✓" "✗")))
                            (displayln (format "    ~a" msg))
                            (set! results (cons (list name h1-lisp vg k valid? diff) results)))))
                      (displayln (format "  Error getting V(G): ~a" error)))))
            (displayln (format "Error computing H¹: ~a" (pipeline-result-error h1-result)))))
      (displayln "")))
  
  ;; Summary
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║                    SUMMARY                              ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  (for ([k (list 0 1)])
    (let ([valid (filter (lambda (r) (and (= (list-ref r 3) k) (list-ref r 4))) results)])
      (displayln (format "H¹ = V(G) - ~a: ~a/~a programs validated" 
                         k 
                         (length valid)
                         (length (filter (lambda (r) (= (list-ref r 3) k)) results))))))
  
  (displayln "")
  (displayln "✓ Validation complete"))

