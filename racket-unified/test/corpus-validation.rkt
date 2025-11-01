#lang racket/base

;; Corpus validation: Compare Lisp results with existing system data
(require "../src/algorithms/unified-pipeline.rkt"
         "../src/bridge/racket-bridge.rkt"
         json
         racket/file
         racket/string)

;; Validate using test programs if corpus not available
(define (validate-test-programs)
  "Use built-in test programs for validation"
  (displayln "Validating with test programs:")
  (displayln "")
  
  (define test-programs
    '(
      ("(lambda (x) x)" "Simple lambda" #f)
      ("(let ((x 1)) x)" "Simple let" #f)
      ("(let ((x 1) (y 2)) (+ x y))" "Multiple bindings" #f)
      ("(lambda (x) (lambda (y) (+ x y)))" "Nested lambdas" #f)
      ("(lambda (x) (let ((y 1)) (+ x y)))" "Lambda with let" #f)
    ))
  
  (for ([test test-programs])
    (let ([source (car test)]
          [name (cadr test)])
      (displayln (format "━━ Test: ~a ━━" name))
      (displayln (format "Source: ~a" source))
      
      (let ([result (compute-h1-from-source-detailed source)])
        (if (pipeline-result-success result)
            (begin
              (displayln (format "  ✓ Success: H¹ = ~a" (pipeline-result-h1 result)))
              (displayln (format "    Bindings: ~a" (pipeline-result-num-bindings result)))
              (displayln (format "    Simplices: 0:~a, 1:~a, 2:~a"
                                 (pipeline-result-num-simplices0 result)
                                 (pipeline-result-num-simplices1 result)
                                 (pipeline-result-num-simplices2 result))))
            (displayln (format "  ✗ Error: ~a" (pipeline-result-error result)))))
      (displayln ""))))

;; Validate against corpus JSON file
(define (validate-corpus json-file)
  "Load and validate programs from JSON corpus"
  (displayln (format "Loading corpus from ~a..." json-file))
  
  (with-handlers ([exn? (lambda (e)
                          (displayln (format "Error loading corpus: ~a" (exn-message e)))
                          (validate-test-programs))])
    (let ([corpus (call-with-input-file json-file read-json)])
      (if (hash? corpus)
          (let ([programs (hash-ref corpus 'programs '())])
            (displayln (format "Found ~a programs in corpus" (length programs)))
            (displayln "")
            (process-corpus-programs programs))
          (begin
            (displayln "Invalid corpus format")
            (validate-test-programs))))))

;; Process programs from corpus
(define (process-corpus-programs programs)
  "Process each program and compare results"
  (define stats (hash 'total 0
                      'lisp-success 0
                      'racket-available 0
                      'matches 0
                      'mismatches 0
                      'errors 0))
  
  (for ([program programs])
    (hash-update! stats 'total add1)
    (let ([source (if (hash? program)
                      (hash-ref program 'source (hash-ref program 'program ""))
                      "")]
          [name (if (hash? program)
                    (hash-ref program 'name (hash-ref program 'program_name "unnamed"))
                    "unnamed")]
          [expected-h1 (if (hash? program)
                           (hash-ref program 'h1 #f)
                           #f)])
      
      (when (not (string=? source ""))
        (displayln (format "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
        (displayln (format "Program: ~a" name))
        (displayln (format "Source: ~a" (if (> (string-length source) 80)
                                            (string-append (substring source 0 80) "...")
                                            source)))
        
        ;; Lisp computation
        (let ([lisp-result (compute-h1-from-source-detailed source)])
          (if (pipeline-result-success lisp-result)
              (begin
                (hash-update! stats 'lisp-success add1)
                (let ([h1-lisp (pipeline-result-h1 lisp-result)])
                  (displayln (format "  Lisp H¹ = ~a" h1-lisp))
                  
                  ;; Compare with expected
                  (when expected-h1
                    (if (= h1-lisp expected-h1)
                        (begin
                          (hash-update! stats 'matches add1)
                          (displayln (format "  ✓ Matches expected: ~a" expected-h1)))
                        (begin
                          (hash-update! stats 'mismatches add1)
                          (displayln (format "  ✗ Mismatch: expected ~a, got ~a" expected-h1 h1-lisp)))))
                  
                  ;; Try Racket bridge
                  (when (racket-service-available?)
                    (hash-update! stats 'racket-available add1)
                    (let-values ([(racket-vg error) (call-racket-vg source)])
                      (if racket-vg
                          (let-values ([(valid? diff msg) (validate-hypothesis h1-lisp racket-vg 0 0)])
                            (displayln (format "  Racket V(G) = ~a" racket-vg))
                            (displayln (format "    Hypothesis: ~a" msg)))
                          (displayln (format "  Racket error: ~a" error)))))))
              (begin
                (hash-update! stats 'errors add1)
                (displayln (format "  ✗ Lisp error: ~a" (pipeline-result-error lisp-result))))))
        (displayln ""))))
  
  ;; Print summary
  (displayln "")
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║                    VALIDATION SUMMARY                   ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  (displayln (format "Total programs: ~a" (hash-ref stats 'total)))
  (displayln (format "Lisp success: ~a (~a%)"
                     (hash-ref stats 'lisp-success)
                     (if (> (hash-ref stats 'total) 0)
                         (* 100 (/ (hash-ref stats 'lisp-success) (hash-ref stats 'total)))
                         0)))
  (displayln (format "Matches: ~a" (hash-ref stats 'matches)))
  (displayln (format "Mismatches: ~a" (hash-ref stats 'mismatches)))
  (displayln (format "Errors: ~a" (hash-ref stats 'errors)))
  (displayln (format "Racket service calls: ~a" (hash-ref stats 'racket-available))))

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║         CORPUS VALIDATION & COMPARISON TOOL             ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (displayln "")
  
  ;; Check if corpus file exists
  (define corpus-file "h1_values.json")
  (if (file-exists? corpus-file)
      (validate-corpus corpus-file)
      (begin
        (displayln "Corpus file not found. Using test programs...")
        (validate-test-programs)))
  
  (displayln "")
  (displayln "✓ Validation complete"))
