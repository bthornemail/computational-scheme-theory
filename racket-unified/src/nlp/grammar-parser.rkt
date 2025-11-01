#lang racket/base

(require racket/match
         racket/string
         racket/list
         "semantic-frame.rkt")

(provide
 parse-query
 tokenize
 apply-production
 token
 token?
 token-type
 token-value)

;; ============================================================
;; GRAMMAR PARSER - EBNF Grammar Implementation
;; ============================================================

;; Grammar non-terminals and terminals from Formalized Grammar spec
(define ACTION-VERBS '("compute" "validate" "analyze" "compare" "explain" "show"))
;; Objects: include lowercase variants since tokenizer lowercases input
(define OBJECTS '("H1" "H¹" "h1" "h¹" "V(G)" "v(g)" "cohomology" "complexity" "binding algebra" "topology" "beta1"))
(define MODIFIER-KEYWORDS '("for" "with" "against" "using"))
(define ENTITY-TYPES '("program" "corpus" "hypothesis"))

;; Token structure
(struct token (type value) #:transparent)

;; Tokenize natural language text
(define (tokenize nl-text)
  "Tokenize natural language text into tokens"
  (define words (string-split (string-downcase nl-text) #:repeat? #t))
  (map (lambda (word)
         (cond
           [(member word ACTION-VERBS)
            (token 'action-verb word)]
           [(member word OBJECTS)
            (token 'object word)]
           [(member word MODIFIER-KEYWORDS)
            (token 'modifier-keyword word)]
           [(member word ENTITY-TYPES)
            (token 'entity-type word)]
           [(regexp-match? #px"^k=\\d+$" word)
            (token 'parameter word)]
           [(regexp-match? #px"^[a-zA-Z0-9_]+$" word)
            (token 'identifier word)]
           [(regexp-match? #px"^\\d+$" word)
            (token 'number word)]
           [else
            (token 'word word)]))
       words))

;; Parse query using EBNF production rules
(define (parse-query nl-text)
  "Parse natural language query into parsed intent and semantic frame"
  (define tokens (tokenize nl-text))
  (define-values (remaining intent-frame)
    (parse-intent tokens (semantic-frame '() '() '() #f)))
  
  (if (null? remaining)
      (values intent-frame intent-frame)
      (error "Parse failed: unconsumed tokens" remaining)))

;; Parse <Intent> ::= <ActionVerb> <Object> [<Parameter>*]
(define (parse-intent tokens frame)
  "Parse intent production rule"
  (cond
    [(null? tokens)
     (values tokens frame)]
    [else
     (define-values (tokens1 frame1)
       (parse-action-verb tokens frame))
     (if (semantic-frame-intent-type frame1)
         (parse-object tokens1 frame1)
         (values tokens frame))]))

;; Parse <ActionVerb> ::= "compute" | "validate" | "analyze" | ...
(define (parse-action-verb tokens frame)
  "Parse action verb production rule"
  (match tokens
    [`(,(token 'action-verb verb) . ,rest)
     (define concepts (cons (list 'action-verb verb) (semantic-frame-concepts frame)))
     (values rest (struct-copy semantic-frame frame
                                [concepts concepts]
                                [intent-type verb]))]
    [else
     (values tokens frame)]))

;; Parse <Object> ::= "H1" | "V(G)" | "cohomology" | ...
(define (parse-object tokens frame)
  "Parse object production rule"
  (match tokens
    [`(,(token 'object obj) . ,rest)
     (define concepts (cons (list 'object obj) (semantic-frame-concepts frame)))
     (values rest (struct-copy semantic-frame frame [concepts concepts]))]
    [else
     (values tokens frame)]))

;; Parse <Modifier> ::= "for" <Entity> | "with" <Parameter> | "against" <Entity>
(define (parse-modifier tokens frame)
  "Parse modifier production rule"
  (match tokens
    [`(,(token 'modifier-keyword mod) . ,rest)
     (let-values ([(tokens1 frame1)
                   (case mod
                     [("for" "against")
                      (parse-entity rest frame)]
                     [("with")
                      (parse-parameter rest frame)]
                     [else
                      (values rest frame)])])
       (define modifiers (cons (list mod tokens1) (semantic-frame-modifiers frame)))
       (values tokens1 (struct-copy semantic-frame frame1 [modifiers modifiers])))]
    [else
     (values tokens frame)]))

;; Parse <Entity> ::= "program" <Identifier> | "corpus" | "hypothesis"
(define (parse-entity tokens frame)
  "Parse entity production rule"
  (match tokens
    [`(,(token 'entity-type entity-type) . ,rest)
     (case entity-type
       [("program")
        (match rest
          [`(,(token 'identifier id) . ,rest2)
           (define concepts (cons (list 'entity entity-type id) (semantic-frame-concepts frame)))
           (values rest2 (struct-copy semantic-frame frame [concepts concepts]))]
          [else
           (values rest frame)])]
       [("corpus" "hypothesis")
        (define concepts (cons (list 'entity entity-type) (semantic-frame-concepts frame)))
        (values rest (struct-copy semantic-frame frame [concepts concepts]))]
       [else
        (values tokens frame)])]
    [else
     (values tokens frame)]))

;; Parse <Parameter> ::= "k=" <Number> | "p>=" <Number> | "language=" <LangId>
(define (parse-parameter tokens frame)
  "Parse parameter production rule"
  (match tokens
    [`(,(token 'parameter param) . ,rest)
     (define modifiers (cons (list 'parameter param) (semantic-frame-modifiers frame)))
     (values rest (struct-copy semantic-frame frame [modifiers modifiers]))]
    [`(,(token 'number num) . ,rest)
     (define modifiers (cons (list 'number num) (semantic-frame-modifiers frame)))
     (values rest (struct-copy semantic-frame frame [modifiers modifiers]))]
    [else
     (values tokens frame)]))

;; Apply production rule (helper for FSM integration)
(define (apply-production rule-name tokens frame)
  "Apply a specific production rule"
  (case rule-name
    [('intent)
     (parse-intent tokens frame)]
    [('action-verb)
     (parse-action-verb tokens frame)]
    [('object)
     (parse-object tokens frame)]
    [('modifier)
     (parse-modifier tokens frame)]
    [('entity)
     (parse-entity tokens frame)]
    [('parameter)
     (parse-parameter tokens frame)]
    [else
     (values tokens frame)]))

;; Helper to create empty semantic frame
(define (make-semantic-frame)
  (semantic-frame '() '() '() #f))

