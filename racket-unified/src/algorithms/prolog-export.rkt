#lang racket/base

(require racket/match
         "algorithm1.rkt"
         "algorithm2.rkt"
         (only-in "../prolog-engine.rkt" fact))

(provide
 export-ast-to-prolog
 ast-to-prolog-facts)

;; ============================================================
;; EXPORT AST TO PROLOG FACTS
;; ============================================================

;; Export AST to Prolog facts
(define (export-ast-to-prolog ast bindings enhanced-scope-map)
  "Convert AST to Prolog facts and add to knowledge base"
  (define facts (ast-to-prolog-facts ast bindings enhanced-scope-map))
  
  ;; Add facts to Prolog knowledge base using fact function
  (for ([fact-tuple facts])
    (when (list? fact-tuple)
      (apply fact fact-tuple)))
  
  facts)

;; Convert AST to list of Prolog fact tuples
(define (ast-to-prolog-facts ast bindings enhanced-scope-map)
  "Generate Prolog facts from AST"
  (define facts '())
  (define scope-counter (box 0))
  
  (define (next-scope-id)
    (let ([id (unbox scope-counter)])
      (set-box! scope-counter (+ id 1))
      (string->symbol (format "scope_~a" id))))
  
  (define (collect-facts expr current-scope)
    (match expr
      [(? ast-define? define-expr)
       (let ([name (ast-define-name define-expr)]
             [value (ast-define-value define-expr)])
         ;; Fact: binding(name, scope, type)
         (set! facts (cons (list 'binding name current-scope 'affine) facts))
         ;; If it's a function definition (lambda), mark it
         (when (ast-lambda? value)
           (set! facts (cons (list 'function name) facts)))
         (collect-facts value current-scope))]
      
      [(? ast-lambda? lambda-expr)
       (let ([params (ast-lambda-params lambda-expr)]
             [body (ast-lambda-body lambda-expr)]
             [lambda-scope (next-scope-id)])
         ;; Scope fact
         (set! facts (cons (list 'scope lambda-scope current-scope) facts))
         ;; Parameter bindings
         (for ([param params])
           (set! facts (cons (list 'binding param lambda-scope 'affine) facts))
           (set! facts (cons (list 'parameter param lambda-scope) facts)))
         ;; Process body
         (for ([body-expr body])
           (collect-facts body-expr lambda-scope)))]
      
      [(? ast-let? let-expr)
       (let ([bindings (ast-let-bindings let-expr)]
             [body (ast-let-body let-expr)]
             [let-scope (next-scope-id)])
         (set! facts (cons (list 'scope let-scope current-scope) facts))
         (for ([binding bindings])
           (let ([name (car binding)]
                 [value (cdr binding)])
             (set! facts (cons (list 'binding name let-scope 'affine) facts))
             (collect-facts value let-scope)))
         (for ([body-expr body])
           (collect-facts body-expr let-scope)))]
      
      [(? ast-letrec? letrec-expr)
       (let ([bindings (ast-letrec-bindings letrec-expr)]
             [body (ast-letrec-body letrec-expr)]
             [letrec-scope (next-scope-id)])
         (set! facts (cons (list 'scope letrec-scope current-scope) facts))
         (for ([binding bindings])
           (let ([name (car binding)]
                 [value (cdr binding)])
             (set! facts (cons (list 'binding name letrec-scope 'affine) facts))
             (set! facts (cons (list 'mutual_recursion name letrec-scope) facts))
             (collect-facts value letrec-scope)))
         (for ([body-expr body])
           (collect-facts body-expr letrec-scope)))]
      
      [(? ast-app? app-expr)
       (let ([func (ast-app-func app-expr)]
             [args (ast-app-args app-expr)])
         ;; Check if this is a function application
         (match func
           [(ast-var loc func-name)
            (set! facts (cons (list 'applies func-name args) facts))
            ;; Check for recursive call (will be enhanced with dependency graph)
            (set! facts (cons (list 'application func current-scope) facts))]
           [_ (void)])
         (collect-facts func current-scope)
         (for ([arg args])
           (collect-facts arg current-scope)))]
      
      [(? ast-if? if-expr)
       (collect-facts (ast-if-test if-expr) current-scope)
       (collect-facts (ast-if-then if-expr) current-scope)
       (collect-facts (ast-if-else if-expr) current-scope)]
      
      [(ast-var loc var-name)
       ;; Variable reference - add usage fact
       (set! facts (cons (list 'references var-name current-scope) facts))]
      
      [else '()]))
  
  (collect-facts ast 'global)
  (reverse facts))

