#lang racket/base

(require racket/match
         racket/set
         racket/list
         racket/hash
         "semantic-lattice.rkt"
         "semantic-frame.rkt"
         "nfa-epsilon.rkt"
         "knowledge-graph.rkt"
         "semantic-lexicon.rkt")

(provide
 interpretation
 interpretation?
 interpretation-frame
 interpretation-confidence
 interpretation-semantic-score
 interpretation-rank
 explore-interpretations
 rank-interpretations
 best-interpretation
 all-interpretations)

;; ============================================================
;; INTERPRETATION EXPLORER - Generate and Rank Interpretations
;; ============================================================

;; Interpretation structure
(struct interpretation (frame confidence semantic-score rank explanation) #:transparent)

;; Explore all possible interpretations of a query
(define (explore-interpretations nl-text lattice)
  "Explore all possible interpretations of natural language query"
  ;; Step 1: Get all parse configurations from NFA-ε
  (define configs (nfa-parse-query nl-text))
  
  ;; Step 2: Convert configurations to interpretations
  (define interpretations
    (map (lambda (config)
           (let ([frame (parse-configuration-frame config)]
                 [conf (parse-configuration-confidence config)])
             ;; Compute semantic score using lattice
             (define sem-score (compute-semantic-score frame lattice))
             (interpretation frame conf sem-score 0.0 ""))) ; Rank computed later
         configs))
  
  ;; Step 3: Rank interpretations by confidence and semantic coherence
  (rank-interpretations interpretations lattice))

;; Compute semantic score for a frame using lattice
(define (compute-semantic-score frame lattice)
  "Compute semantic coherence score using lattice relationships"
  (define concepts (semantic-frame-concepts frame))
  (define score 0.0)
  
  (cond
    [(null? concepts) 0.0]
    [(= (length concepts) 1) 
     ;; Single concept - check if it exists in lattice
     (let ([concept-id (cadr (car concepts))])
       (if (find-concept-in-lattice lattice concept-id)
           0.7  ; Exists but no relationships to score
           0.3))] ; Doesn't exist - lower score
    [else
     ;; Multiple concepts - compute relationship strength
     (let loop ([remaining concepts]
                [total 0.0]
                [count 0])
       (cond
         [(null? remaining) (/ total (max count 1))]
         [(null? (cdr remaining)) 
          ;; Last concept - check if it relates to others
          (let ([concept-id (cadr (car remaining))])
            (if (find-concept-in-lattice lattice concept-id)
                (+ total 0.5)
                total))]
         [else
          ;; Check relationship between first two concepts
          (let ([concept1-id (cadr (car remaining))]
                [concept2-id (cadr (cadr remaining))])
            (define related (check-concept-relationship lattice concept1-id concept2-id))
            (loop (cdr remaining)
                  (+ total (if related 1.0 0.3))
                  (+ count 1)))]))]))

;; Check if two concepts are related in lattice
(define (check-concept-relationship lattice id1 id2)
  "Check if concepts are related (ancestor/descendant or siblings)"
  (define node1 (find-concept-in-lattice lattice id1))
  (define node2 (find-concept-in-lattice lattice id2))
  
  (if (and node1 node2)
      (let ([parents1 (lattice-node-parents node1)]
            [parents2 (lattice-node-parents node2)])
        ;; Check if they share a parent (siblings) or one is ancestor of other
        (or (not (set-empty? (set-intersect (list->set parents1) (list->set parents2))))
            (set-member? (list->set parents2) (lattice-node-id node1))
            (set-member? (list->set parents1) (lattice-node-id node2))))
      #f))

;; Find concept in lattice (helper)
(define (find-concept-in-lattice lattice concept-name)
  "Find concept in lattice by name"
  (for/first ([node (in-set (semantic-lattice-nodes lattice))]
              #:when (eq? (lattice-node-id node) concept-name))
    node))

;; Rank interpretations by combined score
(define (rank-interpretations interpretations lattice)
  "Rank interpretations by confidence × semantic score"
  (define scored
    (map (lambda (interp)
           (define combined-score 
             (* (interpretation-confidence interp)
                (interpretation-semantic-score interp)))
           (struct-copy interpretation interp
                        [rank combined-score]
                        [explanation (format "Confidence: ~a, Semantic: ~a, Combined: ~a"
                                             (interpretation-confidence interp)
                                             (interpretation-semantic-score interp)
                                             combined-score)]))
         interpretations))
  
  ;; Sort by rank (highest first)
  (sort scored
        (lambda (a b)
          (> (interpretation-rank a) (interpretation-rank b)))))

;; Get best interpretation (highest ranked)
(define (best-interpretation interpretations)
  "Get highest-ranked interpretation"
  (if (null? interpretations)
      #f
      (car (rank-interpretations interpretations (empty-lattice)))))

;; Get all interpretations sorted by rank
(define (all-interpretations interpretations)
  "Get all interpretations sorted by rank"
  (rank-interpretations interpretations (empty-lattice)))

;; Use imported find-concept-in-lattice from semantic-lexicon

