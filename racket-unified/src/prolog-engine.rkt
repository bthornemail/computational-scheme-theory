#lang racket/base

(require racket/match
         "combinators.rkt")

(provide
 fact
 rule
 query
 query-prolog
 *kb*)

;; ============================================================
;; PROLOG IN LISP (Top-Down Deduction)
;; ============================================================

;; Prolog-style logic programming embedded in Lisp
;; Using custom implementation (will upgrade to miniKanren later)

;; Knowledge base (facts and rules)
(define *kb* (make-parameter (make-hash)))

;; Assert fact
(define (fact predicate . args)
  "Add fact to knowledge base"
  (hash-set! (*kb*) (cons predicate args) #t))

;; Define rule
(define (rule head body)
  "Define Prolog rule: head :- body"
  (hash-set! (*kb*) head body))

;; Unification (simplified)
(define (unify pattern term bindings)
  "Unify pattern with term, returning extended bindings or #f"
  (cond
    [(equal? pattern term) bindings]
    [(and (symbol? pattern) 
          (> (string-length (symbol->string pattern)) 0)
          (eq? (string-ref (symbol->string pattern) 0) #\?))
     (let ([bound (assoc pattern bindings)])
       (if bound
           (unify (cdr bound) term bindings)
           (cons (cons pattern term) bindings)))]
    [(and (pair? pattern) (pair? term))
     (let ([bindings* (unify (car pattern) (car term) bindings)])
       (and bindings* (unify (cdr pattern) (cdr term) bindings*)))]
    [else #f]))

;; Prolog-style query (backward chaining with Y-combinator)
(define (query goal bindings)
  "Top-down proof search (Y-combinator style - potentially infinite)"
  (cond
    ;; Check if goal is a fact
    [(hash-has-key? (*kb*) goal)
     (list bindings)]
    
    ;; Try to unify with rules
    [else
     (apply append
            (for/list ([key (hash-keys (*kb*))])
              (let ([unified (unify key goal bindings)])
                (if unified
                    (let ([body (hash-ref (*kb*) key)])
                      (if (procedure? body)
                          (body unified)
                          (query body unified)))
                    '()))))]))

;; Query interface (wrapper)
(define (query-prolog goal)
  "Execute Prolog query and return results"
  (query goal '()))

