#lang racket/base

(require racket/match
         racket/hash
         racket/list
         racket/string
         "learning-engine.rkt"
         "context-manager.rkt"
         "semantic-frame.rkt"
         "parse-events.rkt")

(provide
 feedback
 feedback?
 feedback-type
 feedback-rating
 feedback-query
 feedback-response
 feedback-timestamp
 feedback-notes
 submit-feedback
 process-feedback
 apply-feedback-to-learning
 get-feedback-history
 feedback-stats
 feedback-stats?
 feedback-stats-total
 feedback-stats-positive
 feedback-stats-negative
 feedback-stats-neutral
 feedback-stats-average-rating)

;; ============================================================
;; FEEDBACK SYSTEM - User Feedback Integration
;; ============================================================

;; Feedback types
(define POSITIVE-FEEDBACK 'positive)
(define NEGATIVE-FEEDBACK 'negative)
(define NEUTRAL-FEEDBACK 'neutral)

;; Feedback structure
(struct feedback (type rating query response timestamp notes) #:transparent)

;; Feedback statistics
(struct feedback-stats (total positive negative neutral average-rating) #:transparent)

;; Feedback store (timestamp -> feedback)
(define feedback-store (make-hash))

;; Rating store for averaging
(define rating-store '())

;; Submit feedback
(define (submit-feedback type rating query response [notes #f])
  "Submit user feedback"
  (let* ([timestamp (current-seconds)]
         [fb (feedback type rating query response timestamp notes)])
    (hash-set! feedback-store timestamp fb)
    (when (number? rating)
      (set! rating-store (cons rating rating-store)))
    fb))

;; Process feedback and integrate with learning
(define (process-feedback fb kg context)
  "Process feedback and update learning systems"
  (let* ([type (feedback-type fb)]
         [rating (feedback-rating fb)]
         [query (feedback-query fb)]
         [response (feedback-response fb)])
    
    ;; Update learning engine
    (apply-feedback-to-learning query response type kg)
    
    ;; Update context with feedback
    (update-context context query response #f '())
    
    ;; Return updated knowledge graph
    kg))

;; Apply feedback to learning engine
(define (apply-feedback-to-learning query response feedback-type kg)
  "Apply feedback to learning engine and knowledge graph"
  (cond
    [(eq? feedback-type POSITIVE-FEEDBACK)
     ;; Positive feedback: reinforce successful patterns
     (let ([updated-kg (learn-new-concepts query response kg)])
       (when (number? response)
         ;; Track success if response contains M-expression
         (track-rule-success 'nl-parse-rule))
       updated-kg)]
    
    [(eq? feedback-type NEGATIVE-FEEDBACK)
     ;; Negative feedback: track failure, don't reinforce
     (track-rule-failure 'nl-parse-rule)
     kg]
    
    [else
     ;; Neutral feedback: just track interaction
     (update-from-interaction query response 'neutral kg)]))

;; Get feedback history
(define (get-feedback-history [limit #f])
  "Get feedback history, optionally limited to last N entries"
  (let ([all-feedback (sort (hash-values feedback-store)
                             (lambda (a b) 
                               (> (feedback-timestamp a) (feedback-timestamp b))))])
    (if limit
        (take all-feedback limit)
        all-feedback)))

;; Get feedback statistics
(define (get-feedback-stats)
  "Get feedback statistics"
  (let* ([all-feedback (hash-values feedback-store)]
         [total (length all-feedback)]
         [positive (length (filter (lambda (f) (eq? (feedback-type f) POSITIVE-FEEDBACK))
                                   all-feedback))]
         [negative (length (filter (lambda (f) (eq? (feedback-type f) NEGATIVE-FEEDBACK))
                                   all-feedback))]
         [neutral (length (filter (lambda (f) (eq? (feedback-type f) NEUTRAL-FEEDBACK))
                                   all-feedback))]
         [average-rating (if (null? rating-store)
                             0.0
                             (/ (apply + rating-store) (length rating-store)))])
    (feedback-stats total positive negative neutral average-rating)))

;; Get feedback for specific query pattern
(define (get-feedback-for-query pattern)
  "Get feedback for queries matching pattern"
  (filter (lambda (fb)
            (let ([query (feedback-query fb)])
              (if (string? query)
                  (string-contains? (string-downcase query)
                                    (string-downcase pattern))
                  #f)))
          (hash-values feedback-store)))

;; Analyze feedback trends
(define (analyze-feedback-trends [days 7])
  "Analyze feedback trends over specified days"
  (let* ([cutoff (- (current-seconds) (* days 24 60 60))]
         [recent-feedback (filter (lambda (fb)
                                     (> (feedback-timestamp fb) cutoff))
                                   (hash-values feedback-store))])
    (let* ([positive (length (filter (lambda (f) (eq? (feedback-type f) POSITIVE-FEEDBACK))
                                      recent-feedback))]
           [negative (length (filter (lambda (f) (eq? (feedback-type f) NEGATIVE-FEEDBACK))
                                      recent-feedback))]
           [neutral (length (filter (lambda (f) (eq? (feedback-type f) NEUTRAL-FEEDBACK))
                                     recent-feedback))])
      `((period-days ,days)
        (positive ,positive)
        (negative ,negative)
        (neutral ,neutral)
        (total ,(length recent-feedback))
        (satisfaction-rate ,(if (> (+ positive negative neutral) 0)
                                (/ positive (+ positive negative neutral))
                                0.0))))))

;; Get actionable feedback (negative feedback with notes)
(define (get-actionable-feedback)
  "Get negative feedback with notes for improvement"
  (filter (lambda (fb)
            (and (eq? (feedback-type fb) NEGATIVE-FEEDBACK)
                 (feedback-notes fb)))
          (hash-values feedback-store)))

;; Export feedback types
(provide
 POSITIVE-FEEDBACK
 NEGATIVE-FEEDBACK
 NEUTRAL-FEEDBACK
 get-feedback-for-query
 analyze-feedback-trends
 get-actionable-feedback)
