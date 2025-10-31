#lang racket

;; R5RS Scheme Parser
;; Parses S-expressions into AST for CFG construction

(require "ast-types.rkt")

(provide parse-r5rs parse-r5rs-string parse-r5rs-port)

;; Main entry point
(define (parse-r5rs source)
  (cond
    [(string? source) (parse-r5rs-string source)]
    [(port? source) (parse-r5rs-port source)]
    [else (error "Invalid source type: expected string or port")]))

;; Parse from string
(define (parse-r5rs-string str)
  (with-input-from-string str
    (λ () (parse-r5rs-port (current-input-port)))))

;; Parse from port
(define (parse-r5rs-port port)
  (let loop ([exprs '()])
    (let ([expr (read port)])
      (if (eof-object? expr)
          (reverse exprs)
          (loop (cons (sexpr->ast expr) exprs))))))

;; Convert S-expression to AST
(define (sexpr->ast sexpr)
  (sexpr->ast-with-loc sexpr (source-loc "" 1 1)))

(define (sexpr->ast-with-loc sexpr loc)
  (match sexpr
    ;; Self-evaluating literals
    [(? number? n) (ast-const loc n)]
    [(? string? s) (ast-const loc s)]
    [(? boolean? b) (ast-const loc b)]
    [(? char? c) (ast-const loc c)]
    ['() (ast-const loc '())]

    ;; Variables
    [(? symbol? sym) (ast-var loc sym)]

    ;; Lambda
    [`(lambda ,params ,body ...)
     (ast-lambda loc (if (list? params) params (list params)) (map sexpr->ast body))]

    ;; Define
    [`(define ,name ,value)
     (ast-define loc name (sexpr->ast value))]

    ;; Define function
    [`(define (,name ,params ...) ,body ...)
     (ast-define loc name (ast-lambda loc params (map sexpr->ast body)))]

    ;; Let
    [`(let ,bindings ,body ...)
     (ast-let loc (map (λ (b) (match b [`(,name ,value) (cons name (sexpr->ast value))])) bindings)
              (map sexpr->ast body))]

    ;; Letrec
    [`(letrec ,bindings ,body ...)
     (ast-letrec loc (map (λ (b) (match b [`(,name ,value) (cons name (sexpr->ast value))])) bindings)
                 (map sexpr->ast body))]

    ;; If
    [`(if ,test ,then)
     (ast-if loc (sexpr->ast test) (sexpr->ast then) (ast-const loc #f))]

    [`(if ,test ,then ,else)
     (ast-if loc (sexpr->ast test) (sexpr->ast then) (sexpr->ast else))]

    ;; Cond
    [`(cond ,clauses ...)
     (ast-cond loc (map (λ (c)
                          (match c
                            [`(else ,body ...)
                             (cond-clause #f (map sexpr->ast body))]
                            [`(,test ,body ...)
                             (cond-clause (sexpr->ast test) (map sexpr->ast body))]))
                        clauses))]

    ;; Begin
    [`(begin ,exprs ...)
     (ast-begin loc (map sexpr->ast exprs))]

    ;; Call/cc
    [`(call-with-current-continuation ,proc)
     (ast-call/cc loc (sexpr->ast proc))]

    [`(call/cc ,proc)
     (ast-call/cc loc (sexpr->ast proc))]

    ;; Application (must come last)
    [`(,operator ,operands ...)
     (ast-app loc (sexpr->ast operator) (map sexpr->ast operands))]

    [_ (error "Unrecognized form:" sexpr)]))

