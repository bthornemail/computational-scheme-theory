#lang racket/base

(require racket/match
         racket/string)

(provide
 m-expr
 m-expr?
 m-expr-op
 m-expr-args
 parse-m-expr
 m-expr->string)

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

