#lang racket/base

(require racket/match
         racket/set
         "../s-expression.rkt"
         "parse-events.rkt"
         "grammar-parser.rkt"
         "semantic-frame.rkt")

(provide
 parse-state
 parse-state?
 parse-state-current-state
 parse-state-tokens
 parse-state-semantic-stack
 parse-state-knowledge-graph
 make-initial-parse-state
 parse-step
 parse-query-fsm
 parse-transition
 parse-transition?
 parse-transition-from-state
 parse-transition-input
 parse-transition-to-state
 parse-transition-event)

;; ============================================================
;; PARSING FSM - Deterministic Finite State Transducer
;; ============================================================

;; FSM States
(define START-PARSE 'StartParse)
(define EXPECTING-INTENT 'ExpectingIntent)
(define EXPECTING-VERB 'ExpectingVerb)
(define RESOLVING-ENTITY 'ResolvingEntity)
(define BUILDING-LATTICE 'BuildingLattice)
(define PARSE-COMPLETE 'ParseComplete)
(define PARSE-ERROR 'ParseError)

;; Parse state structure
(struct parse-state (current-state tokens semantic-stack knowledge-graph) #:transparent)

;; Parse transition structure
(struct parse-transition (from-state input to-state event) #:transparent)

;; Create initial parse state
(define (make-initial-parse-state tokens)
  "Create initial parse state"
  (parse-state START-PARSE tokens '() '()))

;; FSM Transition function: δ: Q × Σ → Q
(define (parse-step state token)
  "Execute single FSM transition step"
  (let ([current-state (parse-state-current-state state)]
        [tokens (parse-state-tokens state)]
        [stack (parse-state-semantic-stack state)]
        [kg (parse-state-knowledge-graph state)])
    (cond
      [(eq? current-state START-PARSE)
       ;; Transition: δ(q₀, ε) = q₁
       (values (parse-state EXPECTING-INTENT tokens stack kg) #f)]
      
      [(eq? current-state EXPECTING-INTENT)
       ;; Transition: δ(q₁, <ActionVerb>) = q₂
       (if (eq? (token-type token) 'action-verb)
           (let* ([verb (token-value token)]
                  [event (verb-parsed-event verb)]
                  [new-state (parse-state EXPECTING-VERB tokens stack kg)])
             (values new-state event))
           (let ([event (parse-failed-event token "Expected action verb")])
             (values (parse-state PARSE-ERROR tokens stack kg) event)))]
      
      [(eq? current-state EXPECTING-VERB)
       ;; Transition: δ(q₂, <Object>) = q₃
       (if (eq? (token-type token) 'object)
           (let* ([event (parse-step-event EXPECTING-VERB RESOLVING-ENTITY token)]
                  [new-state (parse-state RESOLVING-ENTITY tokens stack kg)])
             (values new-state event))
           (let ([event (parse-failed-event token "Expected object")])
             (values (parse-state PARSE-ERROR tokens stack kg) event)))]
      
      [(eq? current-state RESOLVING-ENTITY)
       ;; Transition: δ(q₃, <Modifier> | <Parameter> | ε) = q₄ or q₅
       (cond
         [(eq? (token-type token) 'modifier-keyword)
          (let* ([event (parse-step-event RESOLVING-ENTITY BUILDING-LATTICE token)]
                 [new-state (parse-state BUILDING-LATTICE tokens stack kg)])
            (values new-state event))]
         [(eq? (token-type token) 'parameter)
          (let* ([event (parse-step-event RESOLVING-ENTITY BUILDING-LATTICE token)]
                 [new-state (parse-state BUILDING-LATTICE tokens stack kg)])
            (values new-state event))]
         [else
          ;; No modifier/parameter - complete here (object was processed)
          (let* ([event (parse-step-event RESOLVING-ENTITY PARSE-COMPLETE token)]
                 [new-state (parse-state PARSE-COMPLETE tokens stack kg)])
            (values new-state event))])]
      
      [(eq? current-state BUILDING-LATTICE)
       ;; Transition: δ(q₄, ε) = q₅ (after processing modifier/parameter)
       (let* ([event (parse-step-event BUILDING-LATTICE PARSE-COMPLETE token)]
              [new-state (parse-state PARSE-COMPLETE tokens stack kg)])
         (values new-state event))]
      
      [(eq? current-state PARSE-COMPLETE)
       ;; Already complete - no transitions
       (values state #f)]
      
      [(eq? current-state PARSE-ERROR)
       ;; Error state - no transitions
       (values state #f)]
      
      [else
       (error "Unknown parse state:" current-state)])))

;; Parse entire query through FSM
(define (parse-query-fsm nl-text)
  "Parse natural language query using FSM"
  (define tokens (tokenize nl-text))
  ;; Start in EXPECTING-INTENT state (skip START-PARSE)
  (define initial-state (parse-state EXPECTING-INTENT tokens '() '()))
  (define events '())
  (define current-state-box (box initial-state))
  
  ;; Process each token sequentially
  (let loop ([remaining-tokens tokens]
             [current-state initial-state])
    (cond
      [(null? remaining-tokens)
       ;; No more tokens - check if we can complete
       (let* ([state-name (parse-state-current-state current-state)]
              [final-state
               (cond
                 [(eq? state-name RESOLVING-ENTITY)
                  ;; Can complete after resolving entity
                  (parse-state PARSE-COMPLETE '() '() '())]
                 [(eq? state-name BUILDING-LATTICE)
                  ;; Can complete after building lattice
                  (parse-state PARSE-COMPLETE '() '() '())]
                 [(eq? state-name EXPECTING-VERB)
                  ;; Error - expected object but none found
                  (parse-state PARSE-ERROR '() '() '())]
                 [else
                  current-state])])
         (set-box! current-state-box final-state))]
      [else
       ;; Process next token
       (let ([tok (car remaining-tokens)])
         (let-values ([(new-state event) (parse-step current-state tok)])
           (set-box! current-state-box new-state)
           (when event
             (set! events (cons event events)))
           ;; Continue with remaining tokens if not in error or complete state
           (let ([next-state-name (parse-state-current-state new-state)])
             (unless (or (eq? next-state-name PARSE-ERROR)
                         (eq? next-state-name PARSE-COMPLETE))
               (loop (cdr remaining-tokens) new-state)))))]))
  
  ;; If complete, create final query-parsed event
  (when (eq? (parse-state-current-state (unbox current-state-box)) PARSE-COMPLETE)
    (define frame (make-semantic-frame))
    (set! events (cons (query-parsed-event 'complete frame) events)))
  
  (values (unbox current-state-box) (reverse events)))

;; Apply single parse event (helper - internal)
(define (apply-parse-event kg event)
  "Apply single parse event to knowledge graph"
  (match (s-expr-type event)
    ['query-parsed kg]
    ['verb-parsed kg]
    ['entity-resolved kg]
    ['parse-step kg]
    ['parse-failed kg]
    [else kg]))

