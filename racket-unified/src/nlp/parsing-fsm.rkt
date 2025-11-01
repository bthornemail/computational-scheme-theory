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
  (match state
    [(parse-state START-PARSE tokens stack kg)
     ;; Transition: δ(q₀, ε) = q₁
     (let ([new-state (parse-state EXPECTING-INTENT tokens stack kg)])
       ;; Recursively call parse-step with new state
       (if (null? tokens)
           (values new-state #f)
           (parse-step new-state token)))]
    
    [(parse-state EXPECTING-INTENT tokens stack kg)
     ;; Transition: δ(q₁, <ActionVerb>) = q₂
     (if (eq? (token-type token) 'action-verb)
         (let* ([verb (token-value token)]
                [event (verb-parsed-event verb)]
                [new-state (parse-state EXPECTING-VERB tokens stack kg)])
           (values new-state event))
         (let ([event (parse-failed-event token "Expected action verb")])
           (values (parse-state PARSE-ERROR tokens stack kg) event)))]
    
    [(parse-state EXPECTING-VERB tokens stack kg)
     ;; Transition: δ(q₂, <Object>) = q₃
     (if (eq? (token-type token) 'object)
         (let* ([event (parse-step-event EXPECTING-VERB RESOLVING-ENTITY token)]
                [new-state (parse-state RESOLVING-ENTITY tokens stack kg)])
           (values new-state event))
         (let ([event (parse-failed-event token "Expected object")])
           (values (parse-state PARSE-ERROR tokens stack kg) event)))]
    
    [(parse-state RESOLVING-ENTITY tokens stack kg)
     ;; Transition: δ(q₃, <Modifier> | <Parameter>) = q₄
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
        ;; Optional modifier/parameter - can complete here
        (let* ([event (parse-step-event RESOLVING-ENTITY PARSE-COMPLETE token)]
               [new-state (parse-state PARSE-COMPLETE tokens stack kg)])
          (values new-state event))])]
    
    [(parse-state BUILDING-LATTICE tokens stack kg)
     ;; Transition: δ(q₄, ε) = q₅
     (let* ([event (parse-step-event BUILDING-LATTICE PARSE-COMPLETE token)]
            [new-state (parse-state PARSE-COMPLETE tokens stack kg)])
       (values new-state event))]
    
    [(parse-state PARSE-COMPLETE tokens stack kg)
     ;; Already complete - no transitions
     (values state #f)]
    
    [(parse-state PARSE-ERROR tokens stack kg)
     ;; Error state - no transitions
     (values state #f)]
    
    [else
     (error "Unknown parse state:" state)]))

;; Parse entire query through FSM
(define (parse-query-fsm nl-text)
  "Parse natural language query using FSM"
  (define tokens (tokenize nl-text))
  ;; Start in EXPECTING-INTENT state (skip START-PARSE)
  (define initial-state (parse-state EXPECTING-INTENT tokens '() '()))
  (define events '())
  (define current-state-box (box initial-state))
  
  ;; Process each token
  (for ([tok (in-list tokens)]
        #:break (eq? (parse-state-current-state (unbox current-state-box)) PARSE-ERROR))
    (let-values ([(new-state event) (parse-step (unbox current-state-box) tok)])
      (set-box! current-state-box new-state)
      (when event
        (set! events (cons event events)))))
  
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

