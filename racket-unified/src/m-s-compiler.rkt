#lang racket/base

(require racket/match
         "m-expression.rkt"
         "s-expression.rkt"
         "prolog-engine.rkt")

(provide
 m-expr->s-expr
 s-expr->m-expr
 validate-m-expr
 compile-m->s)

;; ============================================================
;; M-EXPRESSION ↔ S-EXPRESSION COMPILER
;; ============================================================

;; Compile M-expression to S-expression (with validation)
(define (m-expr->s-expr m proof)
  "Compile M-expression to S-expression with proof"
  (match m
    [(m-expr 'createBinding (list id scope))
     (s-expr 'binding-created
             `((identifier ,id)
               (scope ,scope)
               (timestamp ,(current-inexact-milliseconds))
               (proof ,proof)))]
    [(m-expr 'enterScope (list scope-id))
     (s-expr 'scope-entered
             `((scope-id ,scope-id)
               (parent-scope ,(get-current-scope))
               (timestamp ,(current-inexact-milliseconds))
               (proof ,proof)))]
    [else
     (error "Cannot compile M-expression:" m)]))

;; Decompile S-expression to M-expression (for display/audit)
(define (s-expr->m-expr s)
  "Convert S-expression event back to M-expression command"
  (match s
    [(s-expr 'binding-created data)
     (m-expr 'createBinding
             (list (cadr (assoc 'identifier data))
                   (cadr (assoc 'scope data))))]
    [(s-expr 'scope-entered data)
     (m-expr 'enterScope
             (list (cadr (assoc 'scope-id data))))]
    [else
     (error "Cannot decompile S-expression:" s)]))

;; Validate M-expression using Prolog
(define (validate-m-expr m)
  "Use Prolog to validate M-expression"
  (match m
    [(m-expr 'createBinding (list id scope))
     (let ([valid? (query-prolog `(valid_binding ,id ,scope))])
       (if (not (null? valid?))
           (list 'valid (car valid?))
           (list 'invalid "No proof found")))]
    [(m-expr 'enterScope (list scope-id))
     (let ([valid? (query-prolog `(valid_scope ,scope-id))])
       (if (not (null? valid?))
           (list 'valid (car valid?))
           (list 'invalid "No proof found")))]
    [else
     (list 'invalid "Unknown M-expression")]))

;; Complete M→S compilation pipeline
(define (compile-m->s m-expr)
  "Complete pipeline: Validate → Compile → Return S-expression"
  (let ([validation (validate-m-expr m-expr)])
    (if (eq? (car validation) 'valid)
        (m-expr->s-expr m-expr (cadr validation))
        (error "Validation failed:" (cadr validation)))))

;; Helper: Get current scope (simplified)
(define (get-current-scope)
  "Get current scope from state (simplified)"
  'global)

