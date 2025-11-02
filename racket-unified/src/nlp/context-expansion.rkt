#lang racket/base

(require racket/match
         racket/list)

;; Token structure (local definition to avoid circular dependency)
;; This matches the token struct in grammar-parser.rkt
(struct local-token (type value) #:transparent)

(provide
 expand-with-context
 CONTEXT-PATTERNS
 apply-context-expansion)

;; ============================================================
;; CONTEXT-AWARE EXPANSION - Pure Racket Ambiguity Resolution
;; ============================================================

;; Context patterns: (prev-word current-word) → expansion
(define CONTEXT-PATTERNS
  (hash
   ;; If "cohomology" appears with "compute" → likely means "H1"
   '(compute cohomology) "h1"
   '(calculate cohomology) "h1"
   '(determine cohomology) "h1"
   
   ;; If "complexity" appears with "compute" → likely means "V(G)"
   '(compute complexity) "vg"
   '(calculate complexity) "vg"
   '(measure complexity) "vg"
   '(get complexity) "vg"
   
   ;; If "dimension" appears with "pattern" → pattern dimension
   '(pattern dimension) "pattern-dimension"
   '(pattern dimensions) "pattern-dimension"
   
   ;; If "representation" appears with "polynomial" → polynomial
   '(polynomial representation) "polynomial"
   '(polynomial form) "polynomial"))

;; Helper to extract token type (works with both struct and list representations)
;; For transparent structs, we can pattern match; for lists, we use car
(define (get-token-type tok)
  (cond
    [(pair? tok) (car tok)]  ; List representation: (type value)
    [(struct? tok) 
     ;; Try to extract via match on transparent struct
     (match tok
       [(struct local-token (type val)) type]
       [else #f])]
    [else #f]))

;; Helper to extract token value
(define (get-token-value tok)
  (cond
    [(and (pair? tok) (pair? (cdr tok))) (cadr tok)]  ; List: (type value)
    [(struct? tok)
     ;; Try to extract via match on transparent struct
     (match tok
       [(struct local-token (type val)) val]
       [else #f])]
    [else #f]))

;; Expand tokens based on surrounding context
(define (expand-with-context tokens)
  "Expand ambiguous terms based on surrounding context"
  (for/list ([i (in-range (length tokens))])
    (define token (list-ref tokens i))
    (define prev-token (if (> i 0) (list-ref tokens (- i 1)) #f))
    (define next-token (if (< i (- (length tokens) 1)) 
                           (list-ref tokens (+ i 1)) 
                           #f))
    
    (define token-type (get-token-type token))
    (define token-value (get-token-value token))
    (define prev-token-type (if prev-token (get-token-type prev-token) #f))
    (define prev-token-value (if prev-token (get-token-value prev-token) #f))
    
    (cond
      ;; Expand "cohomology" → "h1" when preceded by compute-like verbs
      [(and (eq? token-type 'object)
            (equal? token-value "cohomology")
            prev-token
            (eq? prev-token-type 'action-verb)
            (member prev-token-value '("compute" "calculate" "determine" "measure")))
       ;; Create token in same format as input (preserve structure)
       (match token
         [(list 'object _) (list 'object "h1")]
         [(struct local-token (type val)) (local-token type "h1")]
         [else (list 'object "h1")])]
      
      ;; Expand "complexity" → "vg" when preceded by compute-like verbs
      [(and (eq? token-type 'object)
            (equal? token-value "complexity")
            prev-token
            (eq? prev-token-type 'action-verb)
            (member prev-token-value '("compute" "calculate" "measure" "get")))
       (match token
         [(list 'object _) (list 'object "vg")]
         [(struct local-token (type val)) (local-token type "vg")]
         [else (list 'object "vg")])]
      
      ;; Expand "dimension" with "pattern" context
      [(and (eq? token-type 'object)
            (equal? token-value "dimension")
            prev-token
            (eq? prev-token-type 'object)
            (member prev-token-value '("pattern" "patterns")))
       (match token
         [(list 'object _) (list 'object "pattern-dimension")]
         [(struct local-token (type val)) (local-token type "pattern-dimension")]
         [else (list 'object "pattern-dimension")])]
      
      ;; Expand "dimensions" similarly
      [(and (eq? token-type 'object)
            (equal? token-value "dimensions")
            prev-token
            (eq? prev-token-type 'object)
            (member prev-token-value '("pattern" "patterns")))
       (match token
         [(list 'object _) (list 'object "pattern-dimension")]
         [(struct local-token (type val)) (local-token type "pattern-dimension")]
         [else (list 'object "pattern-dimension")])]
      
      ;; Default: return token unchanged
      [else token])))

;; Apply context-based expansion to token list
(define (apply-context-expansion tokens)
  "Apply context patterns to token sequence"
  (expand-with-context tokens))

