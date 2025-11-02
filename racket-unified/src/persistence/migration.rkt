#lang racket/base

(require racket/match
         "redis-store.rkt"
         "init.rkt"
         "../nlp/knowledge-graph.rkt"
         "../nlp/knowledge-graph-redis.rkt"
         "../nlp/learning-engine.rkt"
         "../nlp/learning-engine-redis.rkt"
         "../nlp/feedback-system.rkt"
         "../nlp/feedback-system-redis.rkt")

;; Import get-redis-conn from adapter
(define-syntax-rule (get-redis-conn)
  ((dynamic-require "persistence/adapter.rkt" 'get-redis-conn)))

(provide
 migrate-knowledge-graph-to-redis
 migrate-learning-engine-to-redis
 migrate-feedback-system-to-redis
 migrate-all-to-redis)

;; ============================================================
;; PERSISTENCE MIGRATION UTILITIES
;; ============================================================

;; Migrate in-memory knowledge graph to Redis
(define (migrate-knowledge-graph-to-redis graph-memory)
  "Migrate in-memory knowledge graph to Redis"
  (let ([conn (get-redis-connection)])
    (if conn
        (begin
          (save-knowledge-graph-to-redis graph-memory conn)
          (printf "✓ Migrated knowledge graph to Redis\n")
          (empty-knowledge-graph-redis conn))
        (begin
          (printf "⚠️  Redis not available, keeping in-memory\n")
          graph-memory))))

;; Migrate learning engine data to Redis (manual migration needed)
(define (migrate-learning-engine-to-redis)
  "Migrate learning engine data to Redis"
  (let* ([get-backend (dynamic-require "persistence/adapter.rkt" 'get-learning-engine-backend)]
         [conn (get-redis-connection)]
         [engine-redis (get-backend)])
    (if (and conn engine-redis)
        (begin
          ;; Migrate rule performance data
          (let* ([rule-perfs (get-all-rule-performance)])
            (for-each (lambda (perf)
                        (match perf
                          [(list 'rule-performance rule-id success failure total rate)
                           (track-rule-success-redis engine-redis rule-id)]
                          [else #f]))
                      rule-perfs))
          (printf "✓ Migrated learning engine to Redis\n")
          engine-redis)
        (begin
          (printf "⚠️  Redis not available for learning engine\n")
          #f))))

;; Migrate feedback system data to Redis
(define (migrate-feedback-system-to-redis)
  "Migrate feedback system data to Redis"
  (let* ([get-backend (dynamic-require "persistence/adapter.rkt" 'get-feedback-system-backend)]
         [conn (get-redis-connection)]
         [feedback-redis (get-backend)]
        [all-feedback (get-feedback-history)])
    (if (and conn feedback-redis)
        (begin
          ;; Re-submit all feedback entries
          (for-each (lambda (fb)
                      (match fb
                        [(struct feedback (type rating query response timestamp notes))
                         (submit-feedback-redis feedback-redis type rating query response notes)]
                        [else #f]))
                    all-feedback)
          (printf "✓ Migrated ~a feedback entries to Redis\n" (length all-feedback))
          feedback-redis)
        (begin
          (printf "⚠️  Redis not available for feedback system\n")
          #f))))

;; Migrate all systems to Redis
(define (migrate-all-to-redis [kg-memory #f])
  "Migrate all in-memory systems to Redis"
  (printf "Starting migration to Redis...\n")
  (let ([kg-result (if kg-memory
                        (migrate-knowledge-graph-to-redis kg-memory)
                        (printf "⚠️  No knowledge graph provided for migration\n"))]
        [le-result (migrate-learning-engine-to-redis)]
        [fb-result (migrate-feedback-system-to-redis)])
    (printf "\n✓ Migration complete\n")
    (list 'migration-results
          `(knowledge-graph ,(if kg-result 'success 'skipped))
          `(learning-engine ,(if le-result 'success 'skipped))
          `(feedback-system ,(if fb-result 'success 'skipped)))))

;; Import functions from original modules
(define-syntax-rule (import-from module-id names ...)
  (begin
    (define-values (names ...)
      (dynamic-require (string->path (format "../nlp/~a.rkt" module-id))
                       (list 'names ...)))))

;; Import get-all-rule-performance and get-feedback-history
(define (get-all-rule-performance)
  (dynamic-require "../nlp/learning-engine.rkt" 'get-all-rule-performance))

(define (get-feedback-history)
  (dynamic-require "../nlp/feedback-system.rkt" 'get-feedback-history))

