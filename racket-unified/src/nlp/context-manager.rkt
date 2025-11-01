#lang racket/base

(require racket/match
         racket/list
         racket/hash
         racket/set
         racket/string
         "../m-expression.rkt"
         "semantic-frame.rkt")

(provide
 conversation-context
 conversation-context?
 conversation-context-history
 conversation-context-preferences
 conversation-context-terminology
 conversation-context-patterns
 conversation-context-session-id
 conversation-context-timestamp
 make-conversation-context
 update-context
 get-relevant-context
 add-preference
 get-preference
 add-terminology
 get-terminology
 extract-pattern
 find-similar-patterns)

;; ============================================================
;; CONTEXT MANAGER - Conversation Context
;; ============================================================

;; Interaction record
(struct interaction (query response timestamp m-expression events) #:transparent)

;; Conversation context structure with session tracking
(struct conversation-context (history preferences terminology patterns session-id timestamp) #:transparent)

;; Global context store (session-id -> context)
(define context-store (make-hash))

;; Current session ID
(define current-session-id (box (gensym 'session)))

;; Create empty conversation context
(define (make-conversation-context [session-id #f])
  "Create empty conversation context"
  (let ([sid (or session-id (unbox current-session-id))])
    (conversation-context
     '()           ; history: list of interactions
     (make-hash)   ; preferences: key-value store
     (make-hash)   ; terminology: domain-specific terms
     '()           ; patterns: frequently used query patterns
     sid           ; session-id
     (current-seconds))))

;; Update context with new interaction
(define (update-context context query response m-expr events)
  "Update conversation context with new interaction"
  (let* ([history (conversation-context-history context)]
         [new-interaction (interaction query response (current-seconds) m-expr events)]
         [new-history (cons new-interaction (take history 49))]  ; Keep last 50 interactions
         [extracted-patterns (extract-pattern query m-expr)])
    
    ;; Update patterns based on frequency
    (let* ([all-patterns (append extracted-patterns (conversation-context-patterns context))]
           [pattern-counts (count-patterns all-patterns)]
           [top-patterns (take (sort-by-frequency pattern-counts) 10)])  ; Top 10 patterns
      
      (let ([updated-context (struct-copy conversation-context context
                                           [history new-history]
                                           [patterns top-patterns]
                                           [timestamp (current-seconds)])])
        ;; Store updated context
        (hash-set! context-store (conversation-context-session-id context) updated-context)
        updated-context))))

;; Count pattern frequencies
(define (count-patterns patterns)
  "Count frequency of patterns"
  (let ([counts (make-hash)])
    (for-each (lambda (pattern)
                (hash-set! counts pattern
                           (+ (hash-ref counts pattern (lambda () 0)) 1)))
              patterns)
    counts))

;; Sort patterns by frequency
(define (sort-by-frequency pattern-counts)
  "Sort patterns by frequency (descending)"
  (map car (sort (hash->list pattern-counts)
                 (lambda (a b) (> (cdr a) (cdr b))))))

;; Extract pattern from query and M-expression
(define (extract-pattern query m-expr)
  "Extract pattern from query and resulting M-expression"
  (if m-expr
      (list (format "~a → ~a" query (m-expr-op m-expr)))
      (list (format "~a → failed" query))))

;; Get relevant context for current query
(define (get-relevant-context current-query [session-id #f])
  "Get relevant context for current query"
  (let* ([sid (or session-id (unbox current-session-id))]
         [context (hash-ref context-store sid
                            (lambda () (make-conversation-context sid)))])
    
    ;; Filter history for relevant interactions (similar queries)
    (let ([relevant-history (filter (lambda (interaction)
                                     (is-relevant? (interaction-query interaction) current-query))
                                    (conversation-context-history context))])
      
      ;; Build enhanced context with relevant history
      (struct-copy conversation-context context
                   [history relevant-history]))))

;; Check if interaction is relevant to current query
(define (is-relevant? past-query current-query)
  "Check if past query is relevant to current query"
  (let ([past-words (string-split (string-downcase past-query))]
        [current-words (string-split (string-downcase current-query))])
    ;; Check for shared keywords (simple similarity)
    (> (length (set-intersect (list->set past-words) (list->set current-words))) 0)))

;; Add user preference
(define (add-preference context key value)
  "Add user preference to context"
  (let* ([prefs (conversation-context-preferences context)]
         [new-prefs (hash-set prefs key value)])
    (struct-copy conversation-context context [preferences new-prefs])))

;; Get user preference
(define (get-preference context key [default #f])
  "Get user preference from context"
  (hash-ref (conversation-context-preferences context) key
            (lambda () default)))

;; Add domain terminology
(define (add-terminology context term definition)
  "Add domain terminology to context"
  (let* ([terms (conversation-context-terminology context)]
         [new-terms (hash-set terms term definition)])
    (struct-copy conversation-context context [terminology new-terms])))

;; Get domain terminology
(define (get-terminology context term)
  "Get domain terminology definition"
  (hash-ref (conversation-context-terminology context) term
            (lambda () #f)))

;; Find similar patterns in context
(define (find-similar-patterns context query)
  "Find similar patterns to current query in context"
  (let ([patterns (conversation-context-patterns context)])
    (filter (lambda (pattern)
              (let ([pattern-query (car (string-split pattern " → "))])
                (is-relevant? pattern-query query)))
            patterns)))

;; Get context for specific session
(define (get-session-context session-id)
  "Get context for specific session"
  (hash-ref context-store session-id
            (lambda () (make-conversation-context session-id))))

;; Start new session
(define (start-new-session)
  "Start a new conversation session"
  (let ([new-session-id (gensym 'session)])
    (set-box! current-session-id new-session-id)
    (make-conversation-context new-session-id)))

;; Get current session context
(define (get-current-context)
  "Get current session context"
  (get-session-context (unbox current-session-id)))

;; Merge contexts (for multi-session analysis)
(define (merge-contexts contexts)
  "Merge multiple contexts for analysis"
  (let* ([all-history (flatten (map conversation-context-history contexts))]
         [all-preferences (foldl (lambda (ctx acc)
                                   (hash-union acc (conversation-context-preferences ctx)
                                               #:combine (lambda (a b) a)))
                                 (make-hash)
                                 contexts)]
         [all-patterns (flatten (map conversation-context-patterns contexts))]
         [top-patterns (take (sort-by-frequency (count-patterns all-patterns)) 20)])
    
    (conversation-context
     (take all-history 100)  ; Limit merged history
     all-preferences
     (foldl (lambda (ctx acc)
              (hash-union acc (conversation-context-terminology ctx)
                          #:combine (lambda (a b) a)))
            (make-hash)
            contexts)
     top-patterns
     #f  ; No session ID for merged context
     (current-seconds))))

;; Export additional functions
(provide
 get-session-context
 start-new-session
 get-current-context
 merge-contexts
 interaction
 interaction?
 interaction-query
 interaction-response
 interaction-timestamp
 interaction-m-expression
 interaction-events)
