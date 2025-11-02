#lang racket/base

(require racket/hash
         racket/string)

(provide
 ACTION-VERB-SYNONYMS
 OBJECT-SYNONYMS
 normalize-action-verb
 normalize-object
 expand-synonyms
 PHRASE-PATTERNS
 preprocess-phrases)

;; ============================================================
;; SYNONYM MAPPINGS - Pure Racket Word Expansion
;; ============================================================

;; Synonym mappings: alternative words → canonical forms
;; All mappings are bidirectional-aware (can check both directions)
(define ACTION-VERB-SYNONYMS
  (hash
   ;; Synonyms for "compute"
   "calculate" "compute"
   "determine" "compute"
   "measure" "compute"
   "evaluate" "compute"
   "figure" "compute"
   "find" "compute"
   "work" "compute"
   "solve" "compute"
   
   ;; Synonyms for "get"
   "retrieve" "get"
   "fetch" "get"
   "obtain" "get"
   "acquire" "get"
   "extract" "get"
   "grab" "get"
   "pull" "get"
   
   ;; Synonyms for "export"
   "output" "export"
   "generate" "export"
   "dump" "export"
   "save" "export"
   "write" "export"
   "produce" "export"
   
   ;; Synonyms for "validate"
   "verify" "validate"
   "check" "validate"
   "confirm" "validate"
   "test" "validate"
   "prove" "validate"
   
   ;; Synonyms for "analyze"
   "examine" "analyze"
   "study" "analyze"
   "inspect" "analyze"
   "review" "analyze"
   
   ;; Synonyms for "show"
   "display" "show"
   "present" "show"
   "list" "show"
   "print" "show"))

;; Object/concept synonyms
(define OBJECT-SYNONYMS
  (hash
   ;; Synonyms for "H1" / "cohomology"
   "cohomology" "h1"
   "first cohomology" "h1"
   "beta1" "h1"
   "betti number" "h1"
   "first betti" "h1"
   "h one" "h1"
   
   ;; Synonyms for "V(G)" / complexity
   "cyclomatic complexity" "vg"
   "complexity" "vg"
   "mccabe complexity" "vg"
   "v of g" "vg"
   
   ;; Synonyms for "polynomial"
   "poly" "polynomial"
   "polynomial representation" "polynomial"
   "polynomial form" "polynomial"
   
   ;; Synonyms for "dimension"
   "dimensionality" "dimension"
   "dim" "dimension"
   "dimensional" "dimension"
   
   ;; Synonyms for "pattern"
   "structure" "pattern"
   "form" "pattern"
   "shape" "pattern"))

;; Multi-word phrase patterns (need special handling)
(define PHRASE-PATTERNS
  (hash
   "pattern dimension" "pattern-dimension"
   "pattern dimensions" "pattern-dimension"
   "polynomial representation" "polynomial"
   "polynomial form" "polynomial"
   "first cohomology" "h1"
   "cyclomatic complexity" "vg"
   "binding algebra" "binding-algebra"
   "pattern matching" "pattern"))

;; Normalize action verb using synonym dictionary
(define (normalize-action-verb word)
  "Normalize action verb to canonical form via synonyms"
  (hash-ref ACTION-VERB-SYNONYMS (string-downcase word) word))

;; Normalize object/concept using synonym dictionary
(define (normalize-object word)
  "Normalize object to canonical form via synonyms"
  (hash-ref OBJECT-SYNONYMS (string-downcase word) word))

;; Expand word with all known synonyms (for search/recognition)
(define (expand-synonyms canonical-word)
  "Get all synonyms for a canonical word (reverse lookup)"
  (define alternatives (list canonical-word))
  (for ([(synonym canonical) (in-hash ACTION-VERB-SYNONYMS)])
    (when (equal? canonical canonical-word)
      (set! alternatives (cons synonym alternatives))))
  (for ([(synonym canonical) (in-hash OBJECT-SYNONYMS)])
    (when (equal? canonical canonical-word)
      (set! alternatives (cons synonym alternatives))))
  alternatives)

;; Preprocess text to handle multi-word phrases
(define (preprocess-phrases text)
  "Replace multi-word phrases with normalized single tokens"
  (let ([lower (string-downcase text)])
    ;; Sort phrases by length (longest first) to handle overlapping phrases
    (define sorted-phrases
      (sort (hash->list PHRASE-PATTERNS)
            (lambda (a b) (> (string-length (car a)) (string-length (car b))))))
    
    (for/fold ([processed lower])
              ([phrase-replacement (in-list sorted-phrases)])
      (let ([phrase (car phrase-replacement)]
            [replacement (cdr phrase-replacement)])
        (string-replace processed phrase replacement #:all? #t)))))

;; Check if word matches any synonym of target
(define (matches-synonym? word target)
  "Check if word is a synonym of target (canonical or alternative)"
  (or (equal? (string-downcase word) (string-downcase target))
      (equal? (normalize-action-verb word) target)
      (equal? (normalize-object word) target)
      (member (string-downcase word) 
              (map string-downcase (expand-synonyms target)))))


