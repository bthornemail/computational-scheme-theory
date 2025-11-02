#lang racket/base

(require racket/match
         racket/set
         racket/list
         racket/string
         "../s-expression.rkt"
         "parse-events.rkt"
         "grammar-parser.rkt"
         "semantic-frame.rkt")

(provide
 nfa-state
 nfa-state?
 nfa-state-id
 nfa-state-tokens
 nfa-state-frame
 nfa-state-path
 make-initial-nfa-state
 nfa-epsilon-closure
 nfa-transition
 nfa-parse-query
 parse-configuration
 parse-configuration?
 parse-configurations)

;; ============================================================
;; NFA-ε - Nondeterministic Finite Automaton with Epsilon
;; ============================================================

;; NFA State: (Q, Σ, δ, q₀, F) where δ: Q × (Σ ∪ {ε}) → P(Q)
;; P(Q) denotes power set (multiple possible next states)

;; NFA state structure
(struct nfa-state (id tokens frame path) #:transparent)

;; Parse configuration: represents one possible interpretation
(struct parse-configuration (state frame events confidence) #:transparent #:mutable)

(provide
 parse-configuration
 parse-configuration?
 parse-configuration-state
 parse-configuration-frame
 parse-configuration-events
 parse-configuration-confidence)

;; NFA States (Q)
(define NFA-START 'NFAStart)
(define NFA-EXPECTING-INTENT 'NFAExpectingIntent)
(define NFA-EXPECTING-VERB 'NFAExpectingVerb)
(define NFA-RESOLVING-ENTITY 'NFAResolvingEntity)
(define NFA-BUILDING-LATTICE 'NFABuildingLattice)
(define NFA-COMPLETE 'NFAComplete)
(define NFA-ERROR 'NFAError)

;; Create initial NFA state
(define (make-initial-nfa-state tokens)
  "Create initial NFA state"
  (nfa-state NFA-START tokens (make-semantic-frame) '()))

;; Epsilon closure: ε-closure(q) = all states reachable via ε-transitions
(define (nfa-epsilon-closure state)
  "Compute epsilon closure of NFA state - all states reachable via ε"
  (define state-id (nfa-state-id state))
  (define result-configs (set))
  
  (let loop ([visited (set)]
             [current-configs (set (parse-configuration state (nfa-state-frame state) '() 1.0))])
    (cond
      [(set-empty? current-configs)
       ;; No more ε-transitions possible
       (set->list visited)]
      [else
       ;; Explore all ε-transitions from current configs
       (define next-configs (set))
       (for ([config (in-set current-configs)])
         (define state (parse-configuration-state config))
         (define state-id (nfa-state-id state))
         
         ;; Find all ε-transitions from this state
         (define epsilon-targets (get-epsilon-transitions state-id))
         
         (for ([target-id (in-list epsilon-targets)])
           (unless (set-member? visited config)
             (define new-state (nfa-state target-id 
                                         (nfa-state-tokens state)
                                         (nfa-state-frame state)
                                         (cons state-id (nfa-state-path state))))
             (define new-config (parse-configuration new-state
                                                    (nfa-state-frame state)
                                                    (parse-configuration-events config)
                                                    (parse-configuration-confidence config)))
             (set! next-configs (set-add next-configs new-config)))))
       
       (loop (set-union visited current-configs)
             next-configs)])))

;; Get epsilon transitions: δ(q, ε) → {q₁, q₂, ...}
(define (get-epsilon-transitions state-id)
  "Get all states reachable via epsilon from state-id"
  (match state-id
    ;; From START, can epsilon-transition to EXPECTING-INTENT
    ['NFAStart '(NFAExpectingIntent)]
    
    ;; From EXPECTING-INTENT, can epsilon-transition to expecting verb
    ['NFAExpectingIntent '()]
    
    ;; From RESOLVING-ENTITY, can epsilon-transition to BUILDING-LATTICE or COMPLETE
    ['NFAResolvingEntity '(NFABuildingLattice NFAComplete)]
    
    ;; From BUILDING-LATTICE, can epsilon-transition to COMPLETE
    ['NFABuildingLattice '(NFAComplete)]
    
    ;; Terminal states have no epsilon transitions
    ['NFAComplete '()]
    ['NFAError '()]
    [else '()]))

;; NFA Transition: δ(q, σ) → P(Q) where P(Q) is power set
(define (nfa-transition state token)
  "Execute NFA transition on token - returns SET of possible next states"
  (define state-id (nfa-state-id state))
  (define tokens (nfa-state-tokens state))
  (define frame (nfa-state-frame state))
  (define path (nfa-state-path state))
  
  (match state-id
    ['NFAStart
     ;; Can transition to EXPECTING-INTENT on ε (handled separately)
     (set)]
    
    ['NFAExpectingIntent
     ;; Transition on action-verb token
     (cond
       [(eq? (token-type token) 'action-verb)
        (let* ([verb (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [intent-type verb]
                                      [concepts (cons `(action-verb ,verb) (semantic-frame-concepts frame))])])
          (set (nfa-state NFA-EXPECTING-VERB tokens new-frame (cons NFA-EXPECTING-INTENT path))))]
       [(and (eq? (token-type token) 'identifier)
             (string-contains? (string-downcase (token-value token)) "comp"))
        ;; Alternative interpretation: identifier might be verb
        (let ([alt-frame (struct-copy semantic-frame frame
                                     [intent-type "compute"]
                                     [concepts (cons `(action-verb "compute") (semantic-frame-concepts frame))])])
          (set (nfa-state NFA-EXPECTING-VERB tokens alt-frame (cons NFA-EXPECTING-INTENT path))))]
       [else
        ;; No valid transition
        (set)])]
    
    ['NFAExpectingVerb
     ;; Can transition on object OR can interpret modifier/identifier as object
     (define result-states (set))
     (cond
       [(eq? (token-type token) 'object)
        ;; Direct transition to RESOLVING-ENTITY
        (let* ([obj (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [concepts (cons `(object ,obj) (semantic-frame-concepts frame))])])
          (set-add result-states 
                   (nfa-state NFA-RESOLVING-ENTITY tokens new-frame (cons NFA-EXPECTING-VERB path))))]
       [(eq? (token-type token) 'identifier)
        ;; Alternative: identifier could be object (e.g., program name)
        (let* ([id-value (token-value token)]
               [alt-frame (struct-copy semantic-frame frame
                                      [concepts (append (list `(object ,id-value)
                                                               `(entity "program" ,id-value))
                                                        (semantic-frame-concepts frame))])])
          (set-add result-states
                   (nfa-state NFA-RESOLVING-ENTITY tokens alt-frame (cons NFA-EXPECTING-VERB path))))]
       [else (set)])
     
     result-states]
    
    ['NFAResolvingEntity
     ;; Can transition on modifier, parameter, OR epsilon to complete
     (define result-states (set))
     (cond
       [(eq? (token-type token) 'modifier-keyword)
        (let* ([mod-value (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [modifiers (cons `(,mod-value #t) (semantic-frame-modifiers frame))])])
          (set-add result-states
                   (nfa-state NFA-BUILDING-LATTICE tokens new-frame (cons NFA-RESOLVING-ENTITY path))))]
       [(eq? (token-type token) 'parameter)
        (let* ([param-value (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [modifiers (cons `(parameter ,param-value) (semantic-frame-modifiers frame))])])
          (set-add result-states
                   (nfa-state NFA-BUILDING-LATTICE tokens new-frame (cons NFA-RESOLVING-ENTITY path))))]
       [(eq? (token-type token) 'identifier)
        ;; Alternative: identifier could be entity name (e.g., program name after "for")
        (let* ([entity-name-value (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [concepts (append (list `(entity "program" ,entity-name-value))
                                                        (semantic-frame-concepts frame))])])
          (set-add result-states
                   (nfa-state NFA-BUILDING-LATTICE tokens new-frame (cons NFA-RESOLVING-ENTITY path))))]
       [else
        ;; Could also epsilon-transition to complete (handled separately)
        (set)])
     result-states]
    
    ['NFABuildingLattice
     ;; Generally transitions to COMPLETE (can consume remaining tokens or epsilon)
     (if (eq? (token-type token) 'identifier)
        ;; Consume identifier as additional entity
        (let* ([entity-name-value (token-value token)]
               [new-frame (struct-copy semantic-frame frame
                                      [concepts (append (list `(entity "program" ,entity-name-value))
                                                        (semantic-frame-concepts frame))])])
          (set (nfa-state NFA-COMPLETE tokens new-frame (cons NFA-BUILDING-LATTICE path))))
        ;; Epsilon transition to complete
        (set))]
    
    ['NFAComplete
     ;; Terminal state - no transitions
     (set)]
    
    ['NFAError
     ;; Error state - no transitions
     (set)]
    
    [else (set)]))

;; Parse query using NFA-ε - returns ALL possible interpretations
(define (nfa-parse-query nl-text)
  "Parse query using NFA-ε, exploring all possible interpretations"
  (define tokens (tokenize nl-text))
  (define initial-state (make-initial-nfa-state tokens))
  
  ;; Start with initial configuration
  (define initial-config (parse-configuration initial-state 
                                             (make-semantic-frame) 
                                             '() 
                                             1.0))
  
  ;; Compute epsilon closure of initial state
  (define start-configs (list->set (list initial-config)))
  
  ;; Active configurations: set of (state, frame, events, confidence)
  (define active-configs start-configs)
  (define completed-configs (set))
  
  ;; Process tokens sequentially
  (for ([token (in-list tokens)])
    (define next-configs (set))
    
    ;; For each active configuration
    (for ([config (in-set active-configs)])
      (define state (parse-configuration-state config))
      
      ;; Compute epsilon closure first
      (define epsilon-configs (list->set (nfa-epsilon-closure state)))
      
      ;; Then apply token transition on each epsilon-closed configuration
      (for ([epsilon-config (in-set epsilon-configs)])
        (define epsilon-state (parse-configuration-state epsilon-config))
        (define possible-next-states (nfa-transition epsilon-state token))
        
        ;; Each possible next state creates a new configuration
        (for ([next-state (in-set possible-next-states)])
          (define next-frame (nfa-state-frame next-state))
          (define next-events (parse-configuration-events epsilon-config))
          (define next-confidence (* (parse-configuration-confidence epsilon-config) 0.9)) ; Slight confidence decay for alternatives
          
          (when (eq? (nfa-state-id next-state) NFA-COMPLETE)
            (set! completed-configs (set-add completed-configs 
                                             (parse-configuration next-state next-frame next-events next-confidence))))
          
          (set! next-configs (set-add next-configs 
                                     (parse-configuration next-state next-frame next-events next-confidence)))))
      
       ;; Also allow skipping token (epsilon transition)
       (define epsilon-closed-state (nfa-epsilon-closure state))
       (when (not (null? epsilon-closed-state))
         (for ([epsilon-config (in-list epsilon-closed-state)])
           (set! next-configs (set-add next-configs epsilon-config)))))
    
    (set! active-configs next-configs))
  
  ;; Apply final epsilon closure to all active configs
  (define final-active-configs (set))
  (for ([config (in-set active-configs)])
    (define epsilon-configs (nfa-epsilon-closure (parse-configuration-state config)))
    (for ([epsilon-config (in-list epsilon-configs)])
      (set! final-active-configs (set-add final-active-configs epsilon-config))
      (when (eq? (nfa-state-id (parse-configuration-state epsilon-config)) NFA-COMPLETE)
        (set! completed-configs (set-add completed-configs epsilon-config)))))
  
  ;; Return all completed configurations (all valid interpretations)
  (set->list completed-configs))

;; Helper: Get all configurations
(define (parse-configurations)
  "Get all parse configurations (for testing/debugging)"
  '())

