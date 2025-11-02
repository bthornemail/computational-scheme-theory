#lang racket/base

(require racket/match
         racket/set
         racket/list
         "algorithm1.rkt")

(provide
 halstead-metrics?
 halstead-metrics-vocabulary
 halstead-metrics-length
 halstead-metrics-volume
 halstead-metrics-difficulty
 halstead-metrics-effort
 compute-halstead-metrics
 information-flow-metrics?
 information-flow-metrics-fan-in
 information-flow-metrics-fan-out
 information-flow-metrics-complexity
 compute-information-flow-metrics
 compute-loc)

;; ============================================================
;; DATA COMPLEXITY METRICS
;; ============================================================

;; Halstead Metrics
(struct halstead-metrics (vocabulary
                          length
                          volume
                          difficulty
                          effort) #:transparent)

;; Information Flow Metrics (Henry & Kafura style)
(struct information-flow-metrics (fan-in
                                   fan-out
                                   complexity) #:transparent)

;; Compute Lines of Code (LOC)
(define (compute-loc source)
  "Compute Lines of Code from source code string"
  (if (string? source)
      (let ([lines (string-split source "\n")])
        ;; Filter out blank lines and comments
        (length (filter (lambda (line)
                          (let ([trimmed (string-trim line)])
                            (and (not (string=? trimmed ""))
                                 (not (string-prefix? trimmed ";")))))
                        lines)))
      0))

;; Compute Halstead Metrics from AST
(define (compute-halstead-metrics ast)
  "Compute Halstead metrics: vocabulary, length, volume, difficulty, effort
  
   Halstead Metrics:
   - n1 = number of distinct operators
   - n2 = number of distinct operands
   - N1 = total number of operators
   - N2 = total number of operands
   - Vocabulary = n1 + n2
   - Length = N1 + N2
   - Volume = Length * log2(Vocabulary)
   - Difficulty = (n1/2) * (N2/n2)
   - Effort = Difficulty * Volume"
  (let*-values ([(operators-set operands-set) (extract-operators-operands ast)]
                [(operators-count operands-count) (count-operators-operands ast operators-set operands-set)])
    (let* ([n1 (set-count operators-set)]
           [n2 (set-count operands-set)]
           [vocabulary (+ n1 n2)]
           [length (+ operators-count operands-count)]
           [volume (if (> vocabulary 0)
                       (* length (log vocabulary 2))
                       0)]
           [difficulty (if (> n2 0)
                           (* (/ n1 2.0) (/ operands-count n2))
                           0)]
           [effort (* difficulty volume)])
      (halstead-metrics vocabulary length volume difficulty effort))))

;; Extract operators and operands from AST
(define (extract-operators-operands ast)
  "Extract distinct operators and operands from AST"
  (define operators (mutable-set))
  (define operands (mutable-set))
  
  (define (process-expr expr)
    (match expr
      ;; Operators: language constructs
      [(? ast-lambda?) (set-add! operators 'lambda)
                       (for ([p (ast-lambda-params expr)])
                         (set-add! operands p))
                       (for ([b (ast-lambda-body expr)])
                         (process-expr b))]
      
      [(? ast-let?) (set-add! operators 'let)
                    (for ([b (ast-let-bindings expr)])
                      (let ([name (car b)])
                        (set-add! operands name)
                        (process-expr (cdr b))))
                    (for ([b (ast-let-body expr)])
                      (process-expr b))]
      
      [(? ast-letrec?) (set-add! operators 'letrec)
                       (for ([b (ast-letrec-bindings expr)])
                         (let ([name (car b)])
                           (set-add! operands name)
                           (process-expr (cdr b))))
                       (for ([b (ast-letrec-body expr)])
                         (process-expr b))]
      
      [(? ast-if?) (set-add! operators 'if)
                   (process-expr (ast-if-test expr))
                   (process-expr (ast-if-then expr))
                   (process-expr (ast-if-else expr))]
      
      [(? ast-define?) (set-add! operators 'define)
                       (set-add! operands (ast-define-name expr))
                       (process-expr (ast-define-value expr))]
      
      [(? ast-app?) (set-add! operators 'apply)
                    (process-expr (ast-app-func expr))
                    (for ([a (ast-app-args expr)])
                      (process-expr a))]
      
      [(? ast-var? var) (set-add! operands (ast-var-name var))]
      
      [(? ast-const? const) (set-add! operands (ast-const-value const))]
      
      [_ #f]))
  
  (process-expr ast)
  (values operators operands))

;; Count total operators and operands
(define (count-operators-operands ast operators-set operands-set)
  "Count total occurrences of operators and operands"
  (define op-count 0)
  (define opd-count 0)
  
  (define (count-expr expr)
    (match expr
      [(? ast-lambda?) (set! op-count (+ op-count 1))
                       (for ([p (ast-lambda-params expr)])
                         (if (set-member? operands-set p)
                             (set! opd-count (+ opd-count 1))
                             #f))
                       (for ([b (ast-lambda-body expr)])
                         (count-expr b))]
      
      [(? ast-let?) (set! op-count (+ op-count 1))
                    (for ([b (ast-let-bindings expr)])
                      (let ([name (car b)])
                        (if (set-member? operands-set name)
                            (set! opd-count (+ opd-count 1))
                            #f)
                        (count-expr (cdr b))))
                    (for ([b (ast-let-body expr)])
                      (count-expr b))]
      
      [(? ast-letrec?) (set! op-count (+ op-count 1))
                       (for ([b (ast-letrec-bindings expr)])
                         (let ([name (car b)])
                           (if (set-member? operands-set name)
                               (set! opd-count (+ opd-count 1))
                               #f)
                           (count-expr (cdr b))))
                       (for ([b (ast-letrec-body expr)])
                         (count-expr b))]
      
      [(? ast-if?) (set! op-count (+ op-count 1))
                   (count-expr (ast-if-test expr))
                   (count-expr (ast-if-then expr))
                   (count-expr (ast-if-else expr))]
      
      [(? ast-define?) (set! op-count (+ op-count 1))
                        (if (set-member? operands-set (ast-define-name expr))
                            (set! opd-count (+ opd-count 1))
                            #f)
                        (count-expr (ast-define-value expr))]
      
      [(? ast-app?) (set! op-count (+ op-count 1))
                    (count-expr (ast-app-func expr))
                    (for ([a (ast-app-args expr)])
                      (count-expr a))]
      
      [(? ast-var? var) (if (set-member? operands-set (ast-var-name var))
                             (set! opd-count (+ opd-count 1))
                             #f)]
      
      [(? ast-const? const) (if (set-member? operands-set (ast-const-value const))
                                  (set! opd-count (+ opd-count 1))
                                  #f)]
      
      [_ #f]))
  
  (count-expr ast)
  (values op-count opd-count))

;; Compute Information Flow Metrics (Henry & Kafura style)
;; Simplified version: count variable references and definitions
(define (compute-information-flow-metrics ast)
  "Compute Information Flow metrics for bindings (simplified)
  
   Information Flow Metrics:
   - fan-in: number of references to this binding
   - fan-out: number of bindings referenced by this binding
   - complexity: simplified as total references"
  (define var-refs (make-hash))  ; count references to each variable
  (define var-defs (make-hash))  ; count definitions
  
  (define (process-expr expr)
    (match expr
      [(? ast-lambda? lambda-expr)
       (for ([p (ast-lambda-params lambda-expr)])
         (hash-update! var-defs p add1 0))
       (for ([b (ast-lambda-body lambda-expr)])
         (process-expr b))]
      
      [(? ast-let? let-expr)
       (for ([b (ast-let-bindings let-expr)])
         (let ([name (car b)])
           (hash-update! var-defs name add1 0)
           (process-expr (cdr b))))
       (for ([b (ast-let-body let-expr)])
         (process-expr b))]
      
      [(? ast-letrec? letrec-expr)
       (for ([b (ast-letrec-bindings letrec-expr)])
         (let ([name (car b)])
           (hash-update! var-defs name add1 0)
           (process-expr (cdr b))))
       (for ([b (ast-letrec-body letrec-expr)])
         (process-expr b))]
      
      [(? ast-define? define-expr)
       (let ([name (ast-define-name define-expr)])
         (hash-update! var-defs name add1 0)
         (process-expr (ast-define-value define-expr)))]
      
      [(? ast-var? var)
       (let ([var-name (ast-var-name var)])
         (hash-update! var-refs var-name add1 0))]
      
      [(? ast-app? app-expr)
       (process-expr (ast-app-func expr))
       (for ([a (ast-app-args expr)])
         (process-expr a))]
      
      [(? ast-if? if-expr)
       (process-expr (ast-if-test expr))
       (process-expr (ast-if-then expr))
       (process-expr (ast-if-else expr))]
      
      [_ #f]))
  
  (process-expr ast)
  
  ;; Compute totals
  (define total-fan-in (apply + (hash-values var-refs)))
  (define total-fan-out (apply + (hash-values var-refs)))  ; Simplified: same as fan-in
  (define complexity (* total-fan-in total-fan-out))  ; Simplified complexity
  
  (information-flow-metrics total-fan-in total-fan-out complexity))

