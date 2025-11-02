#lang racket/base

(require racket/match
         racket/set
         racket/list
         "grammar-parser.rkt"
         "semantic-lattice.rkt"
         "../m-expression.rkt")

(provide
 pattern-state
 pattern-state?
 pattern-state-pattern
 pattern-state-dimension
 pattern-state-access-count
 pattern-state-church-numeral
 pattern-state-polynomial-degree
 pattern-automaton-step
 pattern-automaton-parse
 pattern-to-dimension
 pattern-to-church-numeral
 pattern-to-polynomial-degree
 extract-pattern-dimensions
 pattern-dimension-to-h1)

;; ============================================================
;; PATTERN AUTOMATON - Pattern Matching as Fundamental Structure
;; ============================================================

;; Pattern state: connects patterns to dimensional framework
(struct pattern-state (pattern dimension access-count church-numeral polynomial-degree) #:transparent)

;; Pattern automaton states
(define PATTERN-EMPTY 'PatternEmpty)           ; () → dimension 0
(define PATTERN-SINGLE 'PatternSingle)         ; (P) → dimension 1
(define PATTERN-REPEAT 'PatternRepeat)         ; (P ...) → dimension n
(define PATTERN-VECTOR 'PatternVector)         ; #(P ...) → dimension n+1
(define PATTERN-NESTED 'PatternNested)         ; ((P ...) ...) → dimension n+m

;; Map pattern structure to dimension
(define (pattern-to-dimension pattern)
  "Map pattern structure to dimensional depth (Church numeral)"
  (match pattern
    [`() 0]  ; Empty pattern = dimension 0
    [`(,x) 1]  ; Single element = dimension 1
    [`(,x ...) 
     ;; Ellipsis pattern - dimension = access count or repetition count
     (define access-count (count-pattern-accesses x))
     (max 1 access-count)]  ; At least dimension 1
    [`#(,x ...)
     ;; Vector with ellipsis - adds 1 to dimension
     (+ 1 (pattern-to-dimension `(,x ...)))]
    [`((,inner ...) ...)
     ;; Nested ellipsis - sum of dimensions
     (+ (pattern-to-dimension `(,inner ...))
        (pattern-to-dimension `(...)))]
    [else 1]))  ; Default: dimension 1

;; Count pattern accesses (for ellipsis patterns)
(define (count-pattern-accesses pattern-expr)
  "Count how many times pattern variable is accessed (recursion depth)"
  (match pattern-expr
    [(? symbol? sym) 1]  ; Single reference
    [`(,head . ,tail) 
     (+ (count-pattern-accesses head)
        (count-pattern-accesses tail))]
    [else 0]))

;; Map pattern to Church numeral representation
(define (pattern-to-church-numeral pattern)
  "Map pattern to Church numeral (λf. λx. f^n x)"
  (define dim (pattern-to-dimension pattern))
  (match dim
    [0 'λf.λx.x]  ; Church 0
    [1 'λf.λx.fx]  ; Church 1
    [n (format "λf.λx.f^~ax" n)]))

;; Map pattern to polynomial degree
(define (pattern-to-polynomial-degree pattern)
  "Map pattern to polynomial degree (exponent)"
  (pattern-to-dimension pattern))

;; Pattern automaton step: process pattern structure
(define (pattern-automaton-step state token)
  "Process token through pattern automaton"
  (define current-pattern (pattern-state-pattern state))
  (define current-dim (pattern-state-dimension state))
  (define current-access (pattern-state-access-count state))
  
  (cond
    [(and (eq? (token-type token) 'object)
          (equal? (token-value token) "pattern"))
     ;; Start pattern recognition
     (pattern-state '() 0 0 'λf.λx.x 0)]
    
    [(and (eq? (token-type token) 'modifier-keyword)
          (equal? (token-value token) "..."))
     ;; Ellipsis detected - increases dimension
     (let ([new-dim (+ current-dim 1)]
           [ellipsis-pattern '(ellipsis)])
       (pattern-state ellipsis-pattern
                     new-dim
                     current-access
                     (pattern-to-church-numeral ellipsis-pattern)
                     new-dim))]
    
    [(eq? (token-type token) 'identifier)
     ;; Pattern variable - access count
     (let* ([name (token-value token)]
            [new-access-count (+ current-access 1)]
            [new-dim-value (max current-dim new-access-count)])
       (pattern-state `(,name)
                     new-dim-value
                     new-access-count
                     (pattern-to-church-numeral `(,name))
                     new-dim-value))]
    
    [else
     ;; Continue with current pattern
     state]))

;; Parse query for pattern information
(define (pattern-automaton-parse nl-text)
  "Parse query to extract pattern structure and map to dimensions"
  (define tokens (tokenize nl-text))
  (define initial-state (pattern-state '() 0 0 'λf.λx.x 0))
  
  (let loop ([remaining tokens]
             [state initial-state])
    (cond
      [(null? remaining) state]
      [else
       (let ([new-state (pattern-automaton-step state (car remaining))])
         (loop (cdr remaining) new-state))])))

;; Extract pattern dimensions from query
(define (extract-pattern-dimensions nl-text)
  "Extract pattern dimensional information from query"
  (define parsed (pattern-automaton-parse nl-text))
  (hash 'pattern (pattern-state-pattern parsed)
        'dimension (pattern-state-dimension parsed)
        'access-count (pattern-state-access-count parsed)
        'church-numeral (pattern-state-church-numeral parsed)
        'polynomial-degree (pattern-state-polynomial-degree parsed)))

;; Connect pattern to H¹ computation (dimensional framework integration)
(define (pattern-dimension-to-h1 pattern-dim)
  "Map pattern dimension to expected H¹ contribution"
  ;; Dimension > 0 indicates cycle → H¹ > 0
  (if (> pattern-dim 0)
      pattern-dim  ; Dimension directly contributes to H¹
      0))

