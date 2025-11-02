#lang racket/base

(require racket/match
         racket/string
         racket/list
         "semantic-frame.rkt"
         "synonyms.rkt"
         "fuzzy-matching.rkt"
         "context-expansion.rkt")

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
(define ACTION-VERBS '("compute" "validate" "analyze" "compare" "explain" "show" "export" "get"))
;; Objects: include lowercase variants since tokenizer lowercases input
(define OBJECTS '("H1" "H¹" "h1" "h¹" "V(G)" "v(g)" "cohomology" "complexity" "binding algebra" "topology" "beta1"
                  "polynomial" "polynomials" "pattern" "patterns" "dimension" "dimensions"))
(define MODIFIER-KEYWORDS '("for" "with" "against" "using"))
(define ENTITY-TYPES '("program" "corpus" "hypothesis"))

;; Token structure
(struct token (type value) #:transparent)

;; Tokenize natural language text (basic version)
(define (tokenize nl-text)
  "Tokenize natural language text into tokens (basic version)"
  (tokenize-enhanced nl-text))

;; Enhanced tokenization with synonym expansion, fuzzy matching, and context
(define (tokenize-enhanced nl-text)
  "Enhanced tokenization with synonyms, fuzzy matching, and context awareness"
  ;; Step 1: Preprocess multi-word phrases
  (define preprocessed (preprocess-phrases nl-text))
  
  ;; Step 2: Split into words
  (define words (string-split (string-downcase preprocessed) #:repeat? #t))
  
  ;; Step 3: Tokenize with synonym normalization
  (define tokens-with-synonyms
    (map (lambda (word)
           ;; Normalize via synonyms first
           (define normalized-verb (normalize-action-verb word))
           (define normalized-obj (normalize-object word))
           
           (cond
             ;; Check normalized action verbs
             [(or (member normalized-verb ACTION-VERBS)
                  (member word ACTION-VERBS))
              (token 'action-verb (if (not (equal? normalized-verb word)) 
                                     normalized-verb 
                                     word))]
             
             ;; Check normalized objects
             [(or (member normalized-obj OBJECTS)
                  (member word OBJECTS))
              (token 'object (if (not (equal? normalized-obj word))
                                normalized-obj
                                word))]
             
             [(member word MODIFIER-KEYWORDS)
              (token 'modifier-keyword word)]
             
             [(member word ENTITY-TYPES)
              (token 'entity-type word)]
             
             [(regexp-match? #px"^k=\\d+$" word)
              (token 'parameter word)]
             
             [(regexp-match? #px"^[a-zA-Z0-9_-]+$" word)
              ;; Try fuzzy matching for unknown words
              (let-values ([(verb-match verb-score) 
                            (fuzzy-match-list word ACTION-VERBS 0.75)]
                           [(obj-match obj-score) 
                            (fuzzy-match-list word OBJECTS 0.75)])
                (cond
                  [(and verb-match (> verb-score 0.75))
                   (token 'action-verb verb-match)]
                  [(and obj-match (> obj-score 0.75))
                   (token 'object obj-match)]
                  [else
                   (token 'identifier word)]))]
             
             [(regexp-match? #px"^\\d+$" word)
              (token 'number word)]
             
             [else
              (token 'word word)]))
         words))
  
  ;; Step 4: Apply context-aware expansion
  (apply-context-expansion tokens-with-synonyms))

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

