#lang racket/base

(require "redis-store.rkt"
         "config.rkt"
         "../nlp/knowledge-graph-redis.rkt"
         "../nlp/learning-engine-redis.rkt"
         "../nlp/feedback-system-redis.rkt")

(provide
 get-knowledge-graph-backend
 get-learning-engine-backend
 get-feedback-system-backend
 use-redis-backends?
 initialize-backends)

;; ============================================================
;; PERSISTENCE ADAPTER - Unified Backend Access
;; ============================================================

;; Backend preference: 'redis, 'memory, or 'auto (use Redis if available)
(define backend-preference (make-parameter 'auto))

;; Check if Redis backends should be used
(define (use-redis-backends?)
  "Check if Redis backends should be used based on mode and availability"
  (let* ([mode (get-persistence-mode)]
         [conn (get-redis-conn)])
    (or (eq? mode 'redis)
        (and (or (eq? mode 'hybrid) (eq? (backend-preference) 'auto))
             conn))))

;; Get knowledge graph backend (Redis or memory)
(define (get-knowledge-graph-backend)
  "Get knowledge graph backend (Redis if available, otherwise memory)"
  (if (use-redis-backends?)
      (let ([conn (get-redis-conn)])
        (if conn
            (empty-knowledge-graph-redis conn)
            #f))
      #f))

;; Get learning engine backend (Redis or memory)
(define (get-learning-engine-backend)
  "Get learning engine backend (Redis if available, otherwise memory)"
  (if (use-redis-backends?)
      (let ([conn (get-redis-conn)])
        (if conn
            (initialize-learning-engine-redis conn)
            #f))
      #f))

;; Get feedback system backend (Redis or memory)
(define (get-feedback-system-backend)
  "Get feedback system backend (Redis if available, otherwise memory)"
  (if (use-redis-backends?)
      (let ([conn (get-redis-conn)])
        (if conn
            (initialize-feedback-system-redis conn)
            #f))
      #f))

;; Initialize all backends
(define (initialize-backends)
  "Initialize all persistence backends and return status"
  (let ([kg (get-knowledge-graph-backend)]
        [le (get-learning-engine-backend)]
        [fb (get-feedback-system-backend)])
    (list 'backends
          `(knowledge-graph ,(if kg 'redis 'memory))
          `(learning-engine ,(if le 'redis 'memory))
          `(feedback-system ,(if fb 'redis 'memory)))))

;; Access redis-conn parameter from init module
(define-syntax-rule (get-redis-conn)
  ((dynamic-require "persistence/init.rkt" 'redis-conn)))

