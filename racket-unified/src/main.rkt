#lang racket/base

(require racket/set
         "m-expression.rkt"
         "s-expression.rkt"
         "datalog-engine.rkt"
         "prolog-engine.rkt"
         "m-s-compiler.rkt"
         "algorithms/unified-pipeline.rkt"
         "bridge/haskell-bridge.rkt"
         "bridge/racket-bridge.rkt")

;; ============================================================
;; THE UNIFIED LISP SUBSTRATE
;; M-expression → Prolog → S-expression → Datalog
;; + Complete H¹ Computation Pipeline
;; + Service Bridges for Hybrid Operation
;; ============================================================

;; Test H¹ computation pipeline
(define (test-h1-pipeline)
  "Test complete H¹ computation pipeline with service comparison"
  (displayln "Testing H¹ computation from Scheme source:")
  (displayln "")
  
  ;; Test cases
  (define test-cases
    '(
      ("(lambda (x) x)" "Simple lambda")
      ("(let ((x 1) (y 2)) (+ x y))" "Let binding")
      ("(lambda (x) (lambda (y) (+ x y)))" "Nested lambdas")
    ))
  
  (for ([test-case test-cases])
    (let ([source (car test-case)]
          [description (cadr test-case)])
      (displayln (format "━━ Test: ~a ━━" description))
      (displayln (format "Source: ~a" source))
      
      (let ([result (compute-h1-from-source-detailed source)])
        (if (pipeline-result-success result)
            (begin
              (displayln (format "  ✓ Success: H¹ = ~a" (pipeline-result-h1 result)))
              (displayln (format "  ✓ Bindings: ~a" (pipeline-result-num-bindings result)))
              (displayln (format "  ✓ Simplices: 0:~a, 1:~a, 2:~a"
                                 (pipeline-result-num-simplices0 result)
                                 (pipeline-result-num-simplices1 result)
                                 (pipeline-result-num-simplices2 result)))
              
              ;; Try Haskell bridge if available
              (displayln "")
              (displayln "  Service Comparison:")
              (when (haskell-service-available?)
                (let-values ([(haskell-h1 error) (call-haskell-h1 source)])
                  (if haskell-h1
                      (let-values ([(match? diff msg) (compare-h1-results
                                                        (pipeline-result-h1 result)
                                                        haskell-h1
                                                        0)])
                        (displayln (format "  ✓ Haskell H¹ = ~a" haskell-h1))
                        (displayln (format "    ~a" msg)))
                      (displayln (format "  ⚠ Haskell service error: ~a" error)))))
              
              ;; Try Racket bridge if available
              (when (racket-service-available?)
                (let-values ([(racket-vg error) (call-racket-vg source)])
                  (if racket-vg
                      (let-values ([(valid? diff msg) (validate-hypothesis
                                                        (pipeline-result-h1 result)
                                                        racket-vg
                                                        0
                                                        0)])
                        (displayln (format "  ✓ Racket V(G) = ~a" racket-vg))
                        (displayln (format "    Hypothesis H¹ = V(G): ~a" msg)))
                      (displayln (format "  ⚠ Racket service error: ~a" error)))))
              
              (when (not (haskell-service-available?))
                (displayln "  ℹ Haskell service not available (skipping comparison)"))
              (when (not (racket-service-available?))
                (displayln "  ℹ Racket service not available (skipping validation)")))
            (displayln (format "  ✗ Error: ~a" (pipeline-result-error result)))))
      
      (displayln ""))))

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║     UNIFIED LISP SUBSTRATE - COMPLETE SYSTEM           ║")
  (displayln "║  M/S-Expressions + Prolog/Datalog + Y/Z-Combinators    ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  ;; Part 1: M→S Pipeline Demo
  (displayln "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (displayln "PART 1: M/S-Expression Pipeline")
  (displayln "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (displayln "")
  
  (define m-command (m-expr 'createBinding '(z scope2)))
  
  (displayln "Step 1: M-expression (User command)")
  (displayln (m-expr->string m-command))
  (displayln "")
  
  ;; Initialize validation rules
  (fact 'scope 'scope1)
  (fact 'scope 'scope2)
  
  (rule '(valid_binding ?X ?S)
        (lambda (bindings)
          (let* ([x (cdr (assoc '?X bindings))]
                 [s (cdr (assoc '?S bindings))])
            ;; Check constraints
            (if (and (not (null? (query-prolog `(shadowed ,x ,s))))
                     (not (null? (query-prolog `(scope ,s)))))
                (list bindings)
                '()))))
  
  ;; Step 2: Validate with Prolog
  (displayln "Step 2: Prolog validation (top-down)")
  (define validation-result (validate-m-expr m-command))
  (displayln validation-result)
  (displayln "")
  
  ;; Step 3: Compile to S-expression
  (when (eq? (car validation-result) 'valid)
    (displayln "Step 3: S-expression (compiled)")
    (define s-command (m-expr->s-expr m-command (cadr validation-result)))
    (displayln s-command)
    (displayln "")
    
    ;; Step 4: Execute S-expression
    (displayln "Step 4: Execute S-expression")
    (execute-s-expr s-command)
    (append-event! s-command)
    (displayln "")
    
    ;; Step 5: Update Datalog and compute fixpoint
    (displayln "Step 5: Datalog derives consequences (bottom-up)")
    (assert-fact! '(binding z))
    (assert-fact! '(scope scope2))
    (assert-fact! '(visible_in z scope2))
    
    ;; Define Datalog rules
    (define-rule
      '(visible_in ?X ?S)
      (lambda (db)
        (define bindings (filter (lambda (f) (eq? (car f) 'binding)) (set->list db)))
        (define scopes (filter (lambda (f) (eq? (car f) 'scope)) (set->list db)))
        (for*/list ([b bindings]
                    [s scopes])
          `(visible_in ,(cadr b) ,(cadr s)))))
    
    (define new-facts (datalog-fixpoint))
    (displayln "Updated fact database:")
    (for ([fact new-facts])
      (when (eq? (car fact) 'visible_in)
        (displayln fact)))
    (displayln ""))
  
  ;; Part 2: H¹ Computation Pipeline
  (displayln "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (displayln "PART 2: H¹ Computation Pipeline")
  (displayln "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (displayln "")
  
  (test-h1-pipeline)
  
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║              ✓ SYSTEM DEMO COMPLETE ✓                   ║")
  (displayln "╚══════════════════════════════════════════════════════════╝"))
