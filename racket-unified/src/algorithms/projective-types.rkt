#lang racket/base

(require racket/match
         racket/set
         "algorithm1.rkt")

(provide
 type-poly?
 type-poly-terms
 type-mono?
 type-mono-base
 type-mono-projective?
 infer-projective-types
 classify-binding-type)

;; ============================================================
;; PROJECTIVE POLYNOMIAL TYPES
;; Types as polynomials: T₁ × T₂ | ⊥ (affine × projective | infinity)
;; ============================================================

;; Type monomial: base type, possibly projective
(struct type-mono (base projective?) #:transparent)
;; base: symbol representing base type
;; projective?: boolean, true if optional/at infinity

;; Type polynomial: sum of monomials (union types, optional types)
(struct type-poly (terms) #:transparent)
;; terms: list of type-mono

;; Create affine type (required)
(define (make-affine-type base)
  (type-poly (list (type-mono base #f))))

;; Create projective type (optional, may be at infinity)
(define (make-projective-type base)
  (type-poly (list (type-mono base #t))))

;; Helper: symbol concatenation
(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

;; Type composition: product of types (T₁ × T₂)
(define (compose-types type1 type2)
  "Compose two types as polynomial product"
  (let ([terms1 (if (type-poly? type1)
                    (type-poly-terms type1)
                    (list type1))]
        [terms2 (if (type-poly? type2)
                    (type-poly-terms type2)
                    (list type2))])
    ;; Product: all combinations
    (type-poly
     (for*/list ([t1 terms1]
                 [t2 terms2])
       (let ([base (symbol-append (type-mono-base t1) '× (type-mono-base t2))]
             [proj (or (type-mono-projective? t1) (type-mono-projective? t2))])
         (type-mono base proj))))))

;; Classify binding type (affine vs projective)
(define (classify-binding-type binding-id ast)
  "Determine if binding is affine (required) or projective (optional)"
  (define binding-context (find-binding-context binding-id ast))
  
  (cond
    ;; Always-defined bindings (affine)
    [(or (member 'define binding-context)
         (member 'lambda-param binding-context)
         (member 'let-binding binding-context))
     'affine]
    
    ;; Conditional bindings (projective - may not execute)
    [(member 'if binding-context)
     'projective]
    
    ;; Error-handling bindings (projective - may error)
    [(member 'catch binding-context)
     'projective]
    
    ;; Default: affine
    [else 'affine]))

;; Find context where binding appears
(define (find-binding-context binding-id expr)
  "Find all contexts where binding appears (if, catch, etc.)"
  (define contexts '())
  
  (define (collect-contexts e path)
    (match e
      [(? ast-if? if-expr)
       ;; Check both branches
       (set! contexts (cons 'if contexts))
       (collect-contexts (ast-if-test if-expr) (cons 'if path))
       (collect-contexts (ast-if-then if-expr) (cons 'if path))
       (collect-contexts (ast-if-else if-expr) (cons 'if path))]
      
      [(? ast-define? define-expr)
       (let ([name (ast-define-name define-expr)]
             [value (ast-define-value define-expr)])
         (when (equal? name binding-id)
           (set! contexts (cons 'define contexts)))
         (collect-contexts value path))]
      
      [(? ast-lambda? lambda-expr)
       (let ([params (ast-lambda-params lambda-expr)])
         (when (member binding-id params)
           (set! contexts (cons 'lambda-param contexts))))
       (for ([body-expr (ast-lambda-body lambda-expr)])
         (collect-contexts body-expr (cons 'lambda path)))]
      
      [(? ast-let? let-expr)
       (let ([bindings (ast-let-bindings let-expr)])
         (for ([binding bindings])
           (when (equal? (car binding) binding-id)
             (set! contexts (cons 'let-binding contexts)))
           (collect-contexts (cdr binding) path)))
       (for ([body-expr (ast-let-body let-expr)])
         (collect-contexts body-expr path))]
      
      [(? ast-app? app-expr)
       (collect-contexts (ast-app-func app-expr) path)
       (for ([arg (ast-app-args app-expr)])
         (collect-contexts arg path))]
      
      [else '()]))
  
  (collect-contexts expr '())
  contexts)

;; Infer projective types from AST
(define (infer-projective-types ast bindings)
  "Infer which bindings are projective (optional/at infinity)"
  (define projective-bindings (mutable-set))
  
  (for ([binding-id (in-set bindings)])
    (when (eq? (classify-binding-type binding-id ast) 'projective)
      (set-add! projective-bindings binding-id)))
  
  projective-bindings)

;; Check if binding is projective
(define (is-projective-binding? binding-id ast)
  "Check if binding is projective"
  (eq? (classify-binding-type binding-id ast) 'projective))

