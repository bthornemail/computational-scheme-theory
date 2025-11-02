#lang racket/base

(require racket/match
         "../m-expression.rkt"
         "../prolog-engine.rkt"
         "../datalog-engine.rkt")

(provide
 m-expr-to-prolog
 m-expr-to-datalog)

;; ============================================================
;; M-EXPRESSION TO PROLOG/DATALOG BRIDGE
;; ============================================================

;; Convert M-expression to Prolog facts
(define (m-expr-to-prolog m)
  "Convert M-expression to Prolog facts"
  (define facts '())
  
  (define (collect-prolog m-expr)
    (when (m-expr? m-expr)
      (let ([op (m-expr-op m-expr)]
            [args (m-expr-args m-expr)])
        (cond
          [(eq? op 'bind)
           (match args
             [(list name value)
              ;; Binding fact
              (set! facts (cons (list 'binding name) facts))
              ;; Also convert value
              (collect-prolog value)]
             [_ '()])]
          
          [(eq? op 'lambda)
           (match args
             [(list params body-exprs)
              ;; Lambda creates scope
              (define scope-id (gensym 'scope))
              (set! facts (cons (list 'scope scope-id) facts))
              ;; Parameters are bindings
              (for ([param params])
                (set! facts (cons (list 'binding param scope-id) facts)))
              ;; Process body
              (if (list? body-exprs)
                  (for ([body-expr body-exprs])
                    (collect-prolog body-expr))
                  (collect-prolog body-exprs))]
             [_ '()])]
          
          [(eq? op 'apply)
           (match args
             [(cons func rest-args)
              ;; Application fact
              (set! facts (cons (list 'application func rest-args) facts))
              (collect-prolog func)
              (for ([arg rest-args])
                (collect-prolog arg))]
             [_ '()])]
          
          [(eq? op 'project)
           (match args
             [(list test branches)
              ;; Projective operation (if/optional)
              (set! facts (cons (list 'projective_operation) facts))
              (collect-prolog test)
              (if (list? branches)
                  (for ([branch branches])
                    (collect-prolog branch))
                  (collect-prolog branches))]
             [_ '()])]
          
          [(eq? op 'typeCompose)
           (match args
             [(list type1 type2)
              ;; Type composition
              (set! facts (cons (list 'type_composition type1 type2) facts))]
             [_ '()])]
          
          [(eq? op 'yCombinator)
           (match args
             [(list func)
              ;; Y-combinator detected
              (set! facts (cons (list 'y_combinator func) facts))
              (collect-prolog func)]
             [_ '()])]
          
          [(eq? op 'var)
           (match args
             [(list name)
              ;; Variable reference
              (set! facts (cons (list 'variable name) facts))]
             [_ '()])]
          
          [(eq? op 'const)
           ;; Constant - no fact needed
           '()]
          
          [else '()]))))
  
  (collect-prolog m)
  (reverse facts))

;; Convert M-expression to Datalog facts
(define (m-expr-to-datalog m)
  "Convert M-expression to Datalog facts"
  (define facts '())
  
  (define (collect-datalog m-expr)
    (when (m-expr? m-expr)
      (let ([op (m-expr-op m-expr)]
            [args (m-expr-args m-expr)])
        (cond
          [(eq? op 'bind)
           (match args
             [(list name value)
              (set! facts (cons (list 'binding name) facts))
              (collect-datalog value)]
             [_ '()])]
          
          [(eq? op 'lambda)
           (match args
             [(list params body)
              (for ([param params])
                (set! facts (cons (list 'binding param) facts)))
              (collect-datalog body)]
             [_ '()])]
          
          [(eq? op 'apply)
           (match args
             [(cons func rest-args)
              (set! facts (cons (list 'application func rest-args) facts))
              (collect-datalog func)
              (for ([arg rest-args])
                (collect-datalog arg))]
             [_ '()])]
          
          [(eq? op 'project)
           (match args
             [(list test branches)
              (set! facts (cons (list 'projective test) facts))
              (collect-datalog test)
              (for ([branch branches])
                (collect-datalog branch))]
             [_ '()])]
          
          [(eq? op 'typeCompose)
           (match args
             [(list type1 type2)
              (set! facts (cons (list 'type_composition type1 type2) facts))]
             [_ '()])]
          
          [(eq? op 'yCombinator)
           (match args
             [(list func)
              (set! facts (cons (list 'y_combinator func) facts))
              (collect-datalog func)]
             [_ '()])]
          
          [(eq? op 'var)
           (match args
             [(list name)
              (set! facts (cons (list 'variable name) facts))]
             [_ '()])]
          
          [else '()]))))
  
  (collect-datalog m)
  (reverse facts))

