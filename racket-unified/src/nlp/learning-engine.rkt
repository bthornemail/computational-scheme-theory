#lang racket/base

(require racket/match
         racket/set
         racket/hash
         racket/string
         racket/list
         "knowledge-graph.rkt"
         "semantic-frame.rkt"
         "parse-events.rkt"
         "semantic-lattice.rkt")

(provide
 update-from-interaction
 learn-new-concepts
 refine-lattice-structure
 rule-performance
 rule-performance?
 rule-performance-rule-id
 rule-performance-success-count
 rule-performance-failure-count
 rule-performance-total-count
 rule-performance-success-rate
 get-rule-performance
 track-rule-success
 track-rule-failure
 get-all-rule-performance
 learning-stats
 learning-stats?
 learning-stats-total-interactions
 learning-stats-learned-concepts
 learning-stats-rule-count
 learning-stats-concept-usage
 get-learning-stats
 track-parse-rule
 analyze-rule-performance)

;; ============================================================
;; LEARNING ENGINE - Continuous Learning System
;; ============================================================

;; Rule performance tracking with success rate
(struct rule-performance (rule-id success-count failure-count total-count success-rate) #:transparent)

;; Learning statistics
(struct learning-stats (total-interactions learned-concepts rule-count concept-usage) #:transparent)

;; Global performance tracking (in-memory, can be persisted)
(define rule-performance-store (make-hash))

;; Global interaction counter
(define interaction-counter (box 0))

;; Global learned concepts set
(define learned-concepts (make-hash))

;; Concept usage tracking (concept-id -> usage-count)
(define concept-usage-tracker (make-hash))

;; Initialize rule performance tracker
(define (init-rule-performance rule-id)
  "Initialize performance tracking for a rule"
  (hash-set! rule-performance-store rule-id
             (rule-performance rule-id 0 0 0 0.0)))

;; Update rule performance on success
(define (track-rule-success rule-id)
  "Track successful rule application"
  (let ([current (hash-ref rule-performance-store rule-id
                           (lambda () (init-rule-performance rule-id)))])
    (let* ([new-success (+ (rule-performance-success-count current) 1)]
           [new-total (+ (rule-performance-total-count current) 1)]
           [new-rate (/ new-success new-total)])
      (hash-set! rule-performance-store rule-id
                 (rule-performance rule-id new-success
                                   (rule-performance-failure-count current)
                                   new-total new-rate)))))

;; Update rule performance on failure
(define (track-rule-failure rule-id)
  "Track failed rule application"
  (let ([current (hash-ref rule-performance-store rule-id
                           (lambda () (init-rule-performance rule-id)))])
    (let* ([new-failure (+ (rule-performance-failure-count current) 1)]
           [new-total (+ (rule-performance-total-count current) 1)]
           [new-rate (/ (rule-performance-success-count current) new-total)])
      (hash-set! rule-performance-store rule-id
                 (rule-performance rule-id (rule-performance-success-count current)
                                   new-failure new-total new-rate)))))

;; Get performance for specific rule
(define (get-rule-performance rule-id)
  "Get performance statistics for a rule"
  (hash-ref rule-performance-store rule-id
            (lambda () #f)))

;; Get all rule performance statistics
(define (get-all-rule-performance)
  "Get all rule performance statistics"
  (hash-values rule-performance-store))

;; Track concept usage
(define (track-concept-usage concept-id)
  "Track usage of a concept"
  (hash-set! concept-usage-tracker concept-id
             (+ (hash-ref concept-usage-tracker concept-id (lambda () 0)) 1)))

;; Update from user interaction with feedback
(define (update-from-interaction user-input response feedback kg)
  "Learn from user interaction with explicit feedback"
  ;; Increment interaction counter
  (set-box! interaction-counter (+ (unbox interaction-counter) 1))
  
  ;; Extract concepts from interaction
  (let* ([concepts (extract-concepts-from-input user-input)]
         [response-concepts (if response (extract-concepts-from-response response) '())])
    
    ;; Track concept usage
    (for-each track-concept-usage (append concepts response-concepts))
    
    ;; Update knowledge graph with new concepts if positive feedback
    (if (and feedback (eq? feedback 'positive))
        (foldl (lambda (concept new-kg)
                 (let ([existing (find-concept-in-kg new-kg concept)])
                   (if existing
                       new-kg
                       (add-concept new-kg 'learned concept '()))))
               kg
               (append concepts response-concepts))
        kg)))

;; Learn new concepts from interaction
(define (learn-new-concepts input response kg)
  "Extract and integrate new concepts from interaction"
  (let* ([input-concepts (extract-concepts-from-input input)]
         [response-concepts (if response (extract-concepts-from-response response) '())]
         [all-concepts (append input-concepts response-concepts)])
    
    ;; Track concept usage
    (for-each track-concept-usage all-concepts)
    
    ;; Add new concepts to knowledge graph
    (foldl (lambda (concept new-kg)
             (let ([existing (find-concept-in-kg new-kg concept)])
               (if existing
                   (begin
                     (hash-set! learned-concepts concept #t)
                     new-kg)
                   (begin
                     (hash-set! learned-concepts concept #t)
                     (add-concept new-kg 'learned concept '())))))
           kg
           all-concepts)))

;; Extract concepts from input (simplified - can be enhanced)
(define (extract-concepts-from-input input)
  "Extract concepts from user input"
  (cond
    [(string? input)
     (let ([words (string-split (string-downcase input))])
       ;; Filter for potential concepts (non-stopwords)
       (filter (lambda (w) 
                 (and (> (string-length w) 2)
                      (not (member w '("the" "for" "with" "compute" "validate")))))
               words))]
    [(list? input)
     (flatten (map extract-concepts-from-input input))]
    [else '()]))

;; Extract concepts from response (simplified)
(define (extract-concepts-from-response response)
  "Extract concepts from system response"
  (if (string? response)
      (extract-concepts-from-input response)
      '()))

;; Find concept in knowledge graph  
(define (find-concept-in-kg kg concept)
  "Check if concept exists in knowledge graph"
  (let ([vertices (set->list (knowledge-graph-vertices kg))])
    (ormap (lambda (vertex)
             (let ([props (if (lattice-node? vertex)
                             (lattice-node-properties vertex)
                             '())])
               (ormap (lambda (prop)
                        (match prop
                          [`(name ,name) (string=? (string-downcase (format "~a" name))
                                                   (string-downcase (format "~a" concept)))]
                          [else #f]))
                      props)))
           vertices)))

;; Refine lattice structure based on usage patterns
(define (refine-lattice-structure usage-patterns kg)
  "Refine lattice structure based on usage patterns"
  ;; Analyze usage frequency and adjust hierarchy
  (let* ([frequent-concepts (get-frequent-concepts 5)]  ; Top 5 most used
         [rare-concepts (get-rare-concepts)]           ; Rarely used
         [updated-kg (promote-frequent-concepts kg frequent-concepts)])
    
    ;; Optionally demote or merge rare concepts
    updated-kg))

;; Get frequently used concepts
(define (get-frequent-concepts n)
  "Get top N most frequently used concepts"
  (let* ([sorted (sort (hash->list concept-usage-tracker)
                       (lambda (a b) (> (cdr a) (cdr b))))]
         [top-n (take sorted n)])
    (map car top-n)))

;; Get rarely used concepts
(define (get-rare-concepts)
  "Get concepts used rarely (threshold: 1 or 0)"
  (filter (lambda (entry) (<= (cdr entry) 1))
          (hash->list concept-usage-tracker)))

;; Promote frequent concepts (move up in hierarchy)
(define (promote-frequent-concepts kg concept-ids)
  "Promote frequently used concepts in hierarchy"
  ;; For now, just return kg unchanged - can be enhanced with actual promotion logic
  ;; This would involve adjusting parent-child relationships
  kg)

;; Get learning statistics
(define (get-learning-stats)
  "Get overall learning statistics"
  (learning-stats
   (unbox interaction-counter)
   (hash-count learned-concepts)
   (hash-count rule-performance-store)
   (hash->list concept-usage-tracker)))

;; Track parsing rule usage (called from FSM)
(define (track-parse-rule rule-name success?)
  "Track usage of a parsing rule"
  (if success?
      (track-rule-success rule-name)
      (track-rule-failure rule-name)))

;; Analyze rule performance for optimization
(define (analyze-rule-performance)
  "Analyze rule performance and suggest optimizations"
  (let* ([all-rules (get-all-rule-performance)]
         [low-performers (filter (lambda (r) 
                                   (< (rule-performance-success-rate r) 0.5))
                                 all-rules)]
         [high-performers (filter (lambda (r)
                                    (> (rule-performance-success-rate r) 0.9))
                                  all-rules)])
    `((low-performers ,(map rule-performance-rule-id low-performers))
      (high-performers ,(map rule-performance-rule-id high-performers))
      (total-rules ,(length all-rules)))))
