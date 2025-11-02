#lang racket/base

(require racket/system
         racket/process
         racket/port
         racket/file
         json
         racket/match
         "layer1-interface.rkt")

(provide
 llm-enhanced-parse
 llm-generate-response
 llm-classify-intent
 llm-available?)

;; ============================================================
;; LLM BRIDGE - Call Python LLM Service from Racket
;; ============================================================

;; Check if Python LLM service is available
(define (llm-available?)
  "Check if Python LLM bridge is available and configured"
  (with-handlers ([exn? (lambda (e) #f)])
    ;; Try to import the Python module (quick check)
    (let ([result (parameterize ([current-error-port (open-output-nowhere)])
                     (system/exit-code "python3" "-c" "from coordinator.llm_bridge import LLMInterface; print('ok')"))])
      (= result 0))))

;; Call Python LLM service via subprocess
(define (call-python-llm python-code)
  "Call Python LLM bridge via subprocess"
  (with-handlers ([exn? (lambda (e) 
                         (hash 'error (exn-message e) 'success #f))])
    (let* ([temp-file (make-temporary-file "racket-llm-~a.py" #f (find-system-path 'temp-dir))]
           [project-root (simplify-path (build-path (current-directory) ".." ".."))]
           [full-code (string-append
                      "# -*- coding: utf-8 -*-\n"
                      "import sys\n"
                      "import os\n"
                      "import json\n"
                      "sys.path.insert(0, os.path.join(r'" (path->string project-root) "', 'python-coordinator'))\n"
                      "from coordinator.llm_integration import EnhancedNLPCoordinator\n"
                      python-code)])
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          ;; Write Python code to temp file
          (with-output-to-file temp-file
            (lambda () (display full-code))
            #:exists 'replace)
          
          ;; Execute Python script
          (parameterize ([current-directory project-root])
            (let* ([output-port (open-output-string)]
                   [exit-code (parameterize ([current-output-port output-port]
                                             [current-error-port output-port])
                               (system/exit-code "python3" (path->string temp-file)))]
                   [output (get-output-string output-port)])
              (close-output-port output-port)
              (if (zero? exit-code)
                  (if (string=? output "")
                      (hash 'success #f 'error "Empty response from Python")
                      (begin
                        ;; Try to parse as JSON
                        (with-handlers ([exn? (lambda (e)
                                               (hash 'success #f 
                                                     'raw_response output
                                                     'error (exn-message e)))])
                          (string->jsexpr output))))
                  (hash 'success #f 'error output)))))
        (lambda ()
          ;; Cleanup
          (when (file-exists? temp-file)
            (delete-file temp-file)))))))

;; LLM-enhanced parsing
(define (llm-enhanced-parse nl-query)
  "Parse query using LLM + fallback to rule-based"
  (if (llm-available?)
      (let* ([escaped-query (string-replace (string-replace nl-query "\\" "\\\\") "'" "\\'")]
             [python-code (format "try:\n    coord = EnhancedNLPCoordinator(llm_type='openai')\n    result = coord.process_nl_query('~a')\n    print(json.dumps(result))\nexcept Exception as e:\n    print(json.dumps({'error': str(e), 'success': False}))\n"
                                escaped-query)]
             [result (call-python-llm python-code)])
        (if (and (hash? result) (hash-ref result 'success #t))
            result
            (begin
              ;; Fallback to rule-based if LLM fails
              (let-values ([(m-expr events kg) (process-nl-query nl-query)])
                (hash 'm-expr m-expr 'events events 'kg kg 'fallback #t)))))
      ;; No LLM available - use rule-based
      (let-values ([(m-expr events kg) (process-nl-query nl-query)])
        (hash 'm-expr m-expr 'events events 'kg kg 'fallback #t))))

;; Generate natural language response
(define (llm-generate-response operation result)
  "Generate natural language explanation using LLM"
  (if (llm-available?)
      (let* ([result-json (jsexpr->string result)]
             [escaped-json (string-replace result-json "'" "\\'")]
             [python-code (format "try:\n    coord = EnhancedNLPCoordinator(llm_type='openai')\n    explanation = coord.nlp_engine.generate_explanation('~a', ~a)\n    print(json.dumps({'explanation': explanation, 'success': True}))\nexcept Exception as e:\n    print(json.dumps({'error': str(e), 'success': False}))\n"
                                operation escaped-json)]
             [response (call-python-llm python-code)])
        (if (and (hash? response) (hash-ref response 'success #t))
            (hash-ref response 'explanation (format "Operation ~a completed. Result: ~a" operation result))
            (format "Operation ~a completed. Result: ~a" operation result)))
      (format "Operation ~a completed. Result: ~a" operation result)))

;; LLM intent classification
(define (llm-classify-intent query)
  "Classify user intent using LLM"
  (if (llm-available?)
      (let* ([escaped-query (string-replace (string-replace query "\\" "\\\\") "'" "\\'")]
             [python-code (format "try:\n    coord = EnhancedNLPCoordinator(llm_type='openai')\n    result = coord.nlp_engine.process_query('~a')\n    print(json.dumps(result))\nexcept Exception as e:\n    print(json.dumps({'operation': 'unknown', 'confidence': 0.0, 'error': str(e)}))\n"
                                escaped-query)]
             [result (call-python-llm python-code)])
        (if (hash? result)
            result
            (hash 'operation "unknown" 'confidence 0.0)))
      (hash 'operation "unknown" 'confidence 0.0 'reasoning "LLM not available")))

