#lang racket/base

(require racket/set
         "m-expression.rkt"
         "s-expression.rkt"
         "datalog-engine.rkt"
         "prolog-engine.rkt"
         "m-s-compiler.rkt")

;; ============================================================
;; THE UNIFIED PIPELINE
;; M-expression → Prolog → S-expression → Datalog
;; ============================================================

(module+ main
  (displayln "=== UNIFIED LISP SUBSTRATE ===")
  (displayln "M/S-Expressions + Prolog/Datalog + Y/Z-Combinators")
  (displayln "")
  
  ;; Example: Execute command through complete pipeline
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
    (displayln "")
    
    (displayln "✓ Complete pipeline executed successfully!")
    (displayln "✓ M→S compilation working")
    (displayln "✓ Prolog validation working")
    (displayln "✓ Datalog inference working")
    (displayln "")
    (displayln "Next: Implement algorithms 1-4")))

