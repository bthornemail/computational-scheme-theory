#lang racket/base

(require racket/match
         racket/set
         racket/list
         "semantic-lattice.rkt"
         "semantic-frame.rkt"
         "interpretation-explorer.rkt"
         "nfa-epsilon.rkt"
         "knowledge-graph.rkt")

(provide
 branch-point
 branch-point?
 branch-point-query
 branch-point-interpretations
 branch-point-chosen
 identify-branch-points
 resolve-branch-point
 choose-best-branch)

;; ============================================================
;; BRANCH POINT RESOLVER - Handle Ambiguous Queries
;; ============================================================

;; Branch point: where polynomial has multiple roots = multiple valid interpretations
(struct branch-point (query interpretations chosen explanation) #:transparent)

;; Identify branch points in query (where multiple interpretations exist)
(define (identify-branch-points nl-text lattice)
  "Identify all branch points (ambiguous query positions) in natural language query"
  ;; Step 1: Explore all interpretations
  (define interpretations (explore-interpretations nl-text lattice))
  
  ;; Step 2: Check if multiple interpretations exist
  (if (> (length interpretations) 1)
      ;; Multiple interpretations = branch point
      (let ([branch (branch-point nl-text 
                                  interpretations
                                  #f  ; Not yet chosen
                                  (format "Query has ~a valid interpretations" (length interpretations)))])
        (list branch))
      ;; Single interpretation = no branch point
      '()))

;; Resolve branch point by choosing best interpretation
(define (resolve-branch-point branch lattice)
  "Resolve branch point by selecting best interpretation"
  (define interpretations (branch-point-interpretations branch))
  
  (if (null? interpretations)
      branch
      (let ([best (best-interpretation interpretations)])
        (struct-copy branch-point branch
                    [chosen best]
                    [explanation (format "Chose interpretation with rank ~a (confidence: ~a, semantic: ~a)"
                                        (interpretation-rank best)
                                        (interpretation-confidence best)
                                        (interpretation-semantic-score best))]))))

;; Choose best branch from multiple interpretations
(define (choose-best-branch interpretations lattice)
  "Choose best interpretation branch based on semantic coherence"
  (if (null? interpretations)
      #f
      (let ([ranked (rank-interpretations interpretations lattice)])
        (car ranked))))

;; Analyze branch point: understand why ambiguity exists
(define (analyze-branch-point branch)
  "Analyze why branch point exists (what causes ambiguity)"
  (define interpretations (branch-point-interpretations branch))
  (define query (branch-point-query branch))
  
  (cond
    [(= (length interpretations) 1)
     "No ambiguity - single interpretation"]
    [(= (length interpretations) 2)
     (let ([frame1 (interpretation-frame (car interpretations))]
           [frame2 (interpretation-frame (cadr interpretations))])
       ;; Compare frames to identify difference
       (define diff (compare-frames frame1 frame2))
       (format "Two interpretations differ in: ~a" diff))]
    [else
     (format "Multiple interpretations (~a) - complex ambiguity" (length interpretations))]))

;; Compare two semantic frames to find differences
(define (compare-frames frame1 frame2)
  "Compare two frames to identify key differences"
  (define concepts1 (semantic-frame-concepts frame1))
  (define concepts2 (semantic-frame-concepts frame2))
  (define intent1 (semantic-frame-intent-type frame1))
  (define intent2 (semantic-frame-intent-type frame2))
  
  (cond
    [(not (equal? intent1 intent2))
     (format "Intent: ~a vs ~a" intent1 intent2)]
    [(not (equal? concepts1 concepts2))
     (format "Concepts: different object interpretations")]
    [else
     "Subtle differences in modifier/entity resolution"]))

;; Map branch points to polynomial roots (conceptual)
(define (branch-points-to-polynomial-roots branches)
  "Conceptual mapping: branch points → polynomial roots"
  (map (lambda (branch)
         (define interpretations (branch-point-interpretations branch))
         (list 'root 
               (length interpretations)  ; Multiplicity
               (map interpretation-frame interpretations)))  ; Root values
       branches))

