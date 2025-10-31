#lang racket

;; AST type definitions for Racket metrics calculator
;; This module defines the abstract syntax tree for R5RS Scheme programs

(provide (all-defined-out))

;; Source location for error reporting
(struct source-loc (file line col) #:transparent)

;; Core AST node types
(struct ast-node (source-loc) #:transparent)

;; Literals
(struct ast-const ast-node (value) #:transparent)

;; Variables
(struct ast-var ast-node (name) #:transparent)

;; Binding forms
(struct ast-lambda ast-node (params body) #:transparent)
(struct ast-define ast-node (name value) #:transparent)
(struct ast-let ast-node (bindings body) #:transparent)
(struct ast-letrec ast-node (bindings body) #:transparent)

;; Control flow
(struct ast-if ast-node (test then else) #:transparent)
(struct ast-cond ast-node (clauses) #:transparent)
(struct ast-case ast-node (key clauses) #:transparent)

;; Application
(struct ast-app ast-node (operator operands) #:transparent)

;; Sequencing
(struct ast-begin ast-node (exprs) #:transparent)

;; First-class continuations
(struct ast-call/cc ast-node (proc) #:transparent)

;; Primitives
(struct ast-primitive ast-node (name) #:transparent)

;; Cond clause
(struct cond-clause (test body) #:transparent)

;; Check if an expression is a binding form
(define (is-binding-form? expr)
  (or (ast-lambda? expr)
      (ast-let? expr)
      (ast-letrec? expr)
      (ast-define? expr)))
