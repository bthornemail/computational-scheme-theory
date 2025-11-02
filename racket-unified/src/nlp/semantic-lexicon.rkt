#lang racket/base

(require racket/match
         racket/set
         "semantic-lattice.rkt")

(provide
 DOMAIN-CONCEPTS
 build-domain-lattice
 initialize-domain-knowledge
 find-concept-in-lattice
 get-concept-hierarchy-lexicon)

;; ============================================================
;; SEMANTIC LEXICON - Domain Knowledge in Pure Racket
;; ============================================================

;; Domain-specific semantic relationships (hypernym → hyponyms)
;; Format: (concept-id parent-concept-ids properties)
(define DOMAIN-CONCEPTS
  '((topology () (type "mathematics"))
    (mathematics () (type "domain"))
    (cohomology (topology mathematics) (type "mathematical-concept"))
    (h1 (cohomology) (type "metric" synonym "first-cohomology"))
    (complexity (mathematics) (type "metric"))
    (vg (complexity) (type "metric" full-name "cyclomatic-complexity"))
    (representation (mathematics) (type "abstraction"))
    (polynomial (representation) (type "representation-form"))
    (measurement (mathematics) (type "property"))
    (dimension (measurement) (type "property" plural "dimensions"))
    (structure (mathematics) (type "form"))
    (pattern (structure) (type "structure-form"))
    (operation () (type "action"))
    (computation (operation) (type "action-category"))
    (compute (computation) (synonyms "calculate" "determine" "measure"))
    (export (computation) (synonyms "output" "generate" "dump"))
    (get (operation) (synonyms "retrieve" "fetch" "obtain"))
    (validate (operation) (synonyms "verify" "check" "confirm"))
    (analyze (operation) (synonyms "examine" "study" "inspect"))))

;; Build semantic lattice from domain concept hierarchy
(define (build-domain-lattice)
  "Build semantic lattice from domain concept hierarchy"
  (define lattice (empty-lattice))
  (define concept-hash (make-hash DOMAIN-CONCEPTS))
  
  ;; First pass: create all nodes
  (define node-map (make-hash))
  (for ([(concept-id props) (in-hash concept-hash)])
    (let ([parents (if (pair? props) (car props) '())]
          [properties (if (and (pair? props) (> (length props) 1)) 
                          (cadr props) 
                          '())])
      (let ([node (lattice-node concept-id 'concept properties '() '())])
        (hash-set! node-map concept-id node))))
  
  ;; Second pass: establish parent-child relationships
  (for/fold ([current-lattice lattice])
            ([(concept-id props) (in-hash concept-hash)])
    (let ([parents (if (pair? props) (car props) '())]
          [node (hash-ref node-map concept-id)])
      (if (null? parents)
          (semantic-lattice (set-add (semantic-lattice-nodes current-lattice) node))
          (let ([node-with-parents
                 (foldl (lambda (parent-id n)
                          (lattice-node-add-parent n parent-id))
                        node
                        parents)])
            ;; Update lattice with node and all parent nodes
            (let ([nodes (semantic-lattice-nodes current-lattice)])
              (semantic-lattice 
               (foldl (lambda (parent-id acc)
                        (set-add acc (hash-ref node-map parent-id)))
                      (set-add nodes node-with-parents)
                      parents))))))))

;; Initialize domain knowledge lattice
(define (initialize-domain-knowledge)
  "Initialize knowledge graph with domain-specific relationships"
  (build-domain-lattice))

;; Find concept in lattice by ID or by synonym/property
(define (find-concept-in-lattice lattice concept-name)
  "Find lattice node for concept (by ID or synonym)"
  (define nodes (semantic-lattice-nodes lattice))
  
  ;; Try direct ID match first
  (or (for/first ([node (in-set nodes)]
                  #:when (eq? (lattice-node-id node) concept-name))
        node)
      
      ;; Try property/synonym match
      (for/first ([node (in-set nodes)]
                  #:when (let ([props (lattice-node-properties node)])
                           (ormap (lambda (prop)
                                    (match prop
                                      [`(synonym ,syn) (equal? syn concept-name)]
                                      [`(full-name ,name) (equal? name concept-name)]
                                      [`(type ,t) (equal? t concept-name)]
                                      [else #f]))
                                  props)))
        node)
      
      #f))

;; Get concept hierarchy (ancestors and descendants)
(define (get-concept-hierarchy-lexicon lattice concept-name)
  "Get full hierarchy for concept: ancestors and descendants"
  (define node (find-concept-in-lattice lattice concept-name))
  (if node
      (let ([ancestors (get-all-ancestors lattice (lattice-node-id node))]
            [descendants (get-all-descendants lattice (lattice-node-id node))])
        (hash 'concept concept-name
              'ancestors ancestors
              'descendants descendants
              'node node))
      #f))

;; Get all ancestors recursively
(define (get-all-ancestors lattice node-id)
  "Get all ancestor nodes recursively"
  (define node (for/first ([n (in-set (semantic-lattice-nodes lattice))]
                           #:when (eq? (lattice-node-id n) node-id))
                 n))
  (if node
      (let ([parents (lattice-node-parents node)])
        (apply set-union
               (set node-id)
               (map (lambda (p-id) (get-all-ancestors lattice p-id)) parents)))
      (set node-id)))

;; Get all descendants recursively
(define (get-all-descendants lattice node-id)
  "Get all descendant nodes recursively"
  (define node (for/first ([n (in-set (semantic-lattice-nodes lattice))]
                           #:when (eq? (lattice-node-id n) node-id))
                 n))
  (if node
      (let ([children (lattice-node-children node)])
        (apply set-union
               (set node-id)
               (map (lambda (c-id) (get-all-descendants lattice c-id)) children)))
      (set node-id)))

