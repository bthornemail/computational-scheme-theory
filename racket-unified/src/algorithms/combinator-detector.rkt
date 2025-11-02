#lang racket/base

(require racket/match
         "algorithm1.rkt"
         "../prolog-engine.rkt"
         "../combinators.rkt")

(provide
 detect-y-combinator
 detect-z-combinator
 combinator-pattern?
 y-combinator-pattern?
 detect-all-combinators)

;; ============================================================
;; Y/Z COMBINATOR PATTERN DETECTION USING PROLOG
;; ============================================================

;; Detect Y-combinator pattern in AST
(define (detect-y-combinator ast)
  "Detect Y-combinator pattern using Prolog rules"
  ;; Define Prolog rules for Y-combinator
  (rule '(y_combinator ?expr)
        (lambda (bindings)
          (let ([expr (cdr (assoc '?expr bindings))])
            (y-combinator-pattern? expr))))
  
  ;; Query for Y-combinator
  (query-prolog '(y_combinator ?expr)))

;; Detect Z-combinator pattern
(define (detect-z-combinator ast)
  "Detect Z-combinator pattern (strict evaluation Y-combinator)"
  ;; Z-combinator is Y-combinator with strict evaluation context
  (rule '(z_combinator ?expr)
        (lambda (bindings)
          (let ([expr (cdr (assoc '?expr bindings))])
            (and (y-combinator-pattern? expr)
                 (strict-evaluation-context? expr)))))
  
  (query-prolog '(z_combinator ?expr)))

;; Check if expression matches Y-combinator pattern
;; Y = λf. (λx. f (x x)) (λx. f (x x))
(define (y-combinator-pattern? expr)
  "Check if expression matches Y-combinator pattern"
  (match expr
    [(? ast-lambda? lambda-expr)
     (let ([params (ast-lambda-params lambda-expr)]
           [body (ast-lambda-body lambda-expr)])
       ;; Y takes one parameter (f)
       (and (= (length params) 1)
            (self-application-pattern? body)))]
    [else #f]))

;; Check for self-application pattern: (λx. f (x x)) (λx. f (x x))
(define (self-application-pattern? expr)
  "Check for self-application pattern"
  (match expr
    [(? ast-app? app-expr)
     (let ([func (ast-app-func app-expr)]
           [args (ast-app-args app-expr)])
       (and (ast-lambda? func)
            (ast-lambda? (car args))
            (same-lambda-pattern? func (car args))
            (contains-self-app (ast-lambda-body func))))]
    [else #f]))

;; Check if two lambdas have the same pattern
(define (same-lambda-pattern? lambda1 lambda2)
  "Check if two lambdas have identical structure (modulo variable names)"
  (and (ast-lambda? lambda1)
       (ast-lambda? lambda2)
       (= (length (ast-lambda-params lambda1))
          (length (ast-lambda-params lambda2)))
       (same-body-structure? (ast-lambda-body lambda1)
                            (ast-lambda-body lambda2))))

;; Check if body contains self-application (x x)
(define (contains-self-app body)
  "Check if lambda body contains self-application pattern"
  (for/or ([expr body])
    (match expr
      [(ast-app loc func args)
       (match func
         [(ast-var loc1 func-name)
          (and (not (null? args))
               (match (car args)
                 [(ast-var loc2 arg-name)
                  (equal? func-name arg-name)]
                 [_ #f]))]
         [_ #f])]
      [else #f])))

;; Check if expressions have same structure
(define (same-body-structure? body1 body2)
  "Check if two body expressions have the same structure"
  (and (= (length body1) (length body2))
       (for/and ([e1 body1] [e2 body2])
         (same-expr-structure? e1 e2))))

;; Check if two expressions have same structure (ignoring variable names)
(define (same-expr-structure? expr1 expr2)
  "Check if expressions have same AST structure"
  (match (list expr1 expr2)
    [(list (? ast-const? _) (? ast-const? _)) #t]
    [(list (? ast-var? _) (? ast-var? _)) #t]
    [(list (? ast-lambda? l1) (? ast-lambda? l2))
     (and (= (length (ast-lambda-params l1))
             (length (ast-lambda-params l2)))
          (same-body-structure? (ast-lambda-body l1) (ast-lambda-body l2)))]
    [(list (? ast-app? a1) (? ast-app? a2))
     (and (same-expr-structure? (ast-app-func a1) (ast-app-func a2))
          (= (length (ast-app-args a1)) (length (ast-app-args a2)))
          (for/and ([arg1 (ast-app-args a1)]
                    [arg2 (ast-app-args a2)])
            (same-expr-structure? arg1 arg2)))]
    [else #f]))

;; Check if expression is in strict evaluation context
(define (strict-evaluation-context? expr)
  "Check if expression is in strict (call-by-value) evaluation context"
  ;; For now, assume all contexts are strict in Scheme/Racket
  ;; Could be enhanced to detect lazy evaluation contexts
  #t)

;; Check if any combinator pattern exists
(define (combinator-pattern? expr)
  "Check if expression matches any combinator pattern"
  (or (y-combinator-pattern? expr)
      (and (y-combinator-pattern? expr)
           (strict-evaluation-context? expr))))

;; Detect all combinators in AST and return list of (type location)
(define (detect-all-combinators ast)
  "Detect all Y/Z combinators in AST and return list of (combinator-type location)"
  (define combinators '())
  
  (define (scan-expr expr loc-id)
    (cond
      [(y-combinator-pattern? expr)
       (set! combinators (cons (list 'y-combinator loc-id) combinators))]
      [(and (y-combinator-pattern? expr)
            (strict-evaluation-context? expr))
       (set! combinators (cons (list 'z-combinator loc-id) combinators))])
    
    ;; Recursively scan subexpressions
    (match expr
      [(? ast-define? def-expr)
       (scan-expr (ast-define-value def-expr) loc-id)]
      [(? ast-lambda? lambda-expr)
       (for ([body-expr (ast-lambda-body lambda-expr)])
         (scan-expr body-expr loc-id))]
      [(? ast-let? let-expr)
       (for ([binding (ast-let-bindings let-expr)])
         (scan-expr (cdr binding) loc-id))
       (for ([body-expr (ast-let-body let-expr)])
         (scan-expr body-expr loc-id))]
      [(? ast-letrec? letrec-expr)
       (for ([binding (ast-letrec-bindings letrec-expr)])
         (scan-expr (cdr binding) loc-id))
       (for ([body-expr (ast-letrec-body letrec-expr)])
         (scan-expr body-expr loc-id))]
      [(? ast-app? app-expr)
       (scan-expr (ast-app-func app-expr) loc-id)
       (for ([arg (ast-app-args app-expr)])
         (scan-expr arg loc-id))]
      [(? ast-if? if-expr)
       (scan-expr (ast-if-test if-expr) loc-id)
       (scan-expr (ast-if-then if-expr) loc-id)
       (scan-expr (ast-if-else if-expr) loc-id)]
      [else (void)]))
  
  ;; Scan AST (assuming it's a list of top-level forms)
  (if (list? ast)
      (for ([expr ast])
        (scan-expr expr 'top-level))
      (scan-expr ast 'top-level))
  
  combinators)

