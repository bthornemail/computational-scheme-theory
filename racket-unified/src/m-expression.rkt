#lang racket/base

(require racket/match
         racket/string
         "algorithms/algorithm1.rkt")

(provide
 m-expr
 m-expr?
 m-expr-op
 m-expr-args
 parse-m-expr
 m-expr->string
 ast-to-m-expr
 type-compose-m-expr
 y-combinator-m-expr)

;; ============================================================
;; M-EXPRESSIONS (Meta-Language)
;; ============================================================

;; M-expressions are LISP lists with special syntax markers
;; Example: createBinding[x; scope1] → (m-expr 'createBinding '(x scope1))

(struct m-expr (op args) #:transparent)

;; Parse M-expression from list notation
;; Input: '(createBinding x scope1)
;; Output: (m-expr 'createBinding '(x scope1))
(define (parse-m-expr expr)
  "Parse M-expression list into m-expr structure"
  (match expr
    [`(,op . ,args)
     (m-expr op args)]
    [else
     (error "Invalid M-expression" expr)]))

;; Convert M-expression to string representation
(define (m-expr->string m)
  "Convert M-expression to readable string"
  (match m
    [(m-expr op args)
     (format "~a[~a]"
             op
             (string-join (map (lambda (a) (format "~a" a)) args)
                          "; "))]))

;; Convert AST node to M-expression
(define (ast-to-m-expr expr)
  "Convert AST expression to M-expression (meta-level representation)"
  (match expr
    [(? ast-define? define-expr)
     (m-expr 'bind
             (list (ast-define-name define-expr)
                   (ast-to-m-expr (ast-define-value define-expr))))]
    
    [(? ast-lambda? lambda-expr)
     (m-expr 'lambda
             (list (ast-lambda-params lambda-expr)
                   (map ast-to-m-expr (ast-lambda-body lambda-expr))))]
    
    [(? ast-app? app-expr)
     (m-expr 'apply
             (cons (ast-to-m-expr (ast-app-func app-expr))
                   (map ast-to-m-expr (ast-app-args app-expr))))]
    
    [(? ast-if? if-expr)
     (m-expr 'project
             (list (ast-to-m-expr (ast-if-test if-expr))
                   (list (ast-to-m-expr (ast-if-then if-expr))
                         (ast-to-m-expr (ast-if-else if-expr)))))]
    
    [(ast-var loc name)
     (m-expr 'var (list name))]
    
    [(ast-const loc value)
     (m-expr 'const (list value))]
    
    [else (m-expr 'unknown (list expr))]))

;; M-expression for type composition
(define (type-compose-m-expr type1 type2)
  "Create M-expression for type composition: typeCompose[T1; T2]"
  (m-expr 'typeCompose (list type1 type2)))

;; M-expression for Y-combinator
(define (y-combinator-m-expr func-expr)
  "Create M-expression for Y-combinator: yCombinator[f]"
  (m-expr 'yCombinator (list func-expr)))

