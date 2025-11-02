#lang racket/base

(require "redis-store.rkt"
         "config.rkt"
         "../s-expression.rkt")

(provide
 initialize-persistence
 persistence-health-check
 redis-conn)

;; ============================================================
;; PERSISTENCE INITIALIZATION
;; ============================================================

;; Global Redis connection
(define redis-conn (make-parameter #f))

;; Initialize all persistence systems
(define (initialize-persistence)
  "Initialize file-based and Redis persistence systems"
  ;; Initialize configuration
  (initialize-persistence-config)
  
  ;; Initialize event store from file
  (let ([event-count (initialize-event-store)])
    (printf "✓ Loaded ~a events from file\n" event-count))
  
  ;; Initialize Redis connection if mode includes Redis
  (let ([mode (get-persistence-mode)])
    (when (or (eq? mode 'redis) (eq? mode 'hybrid))
      (let ([conn (redis-connect)])
        (when conn
          (redis-conn conn)
          (let ([pong (redis-ping conn)])
            (if (equal? pong "PONG")
                (printf "✓ Redis connected successfully\n")
                (printf "⚠️  Redis connection warning: ~a\n" pong)))))))
  
  #t)

;; Health check for persistence systems
(define (persistence-health-check)
  "Check health of persistence systems"
  (let ([results '()])
    ;; Check file-based event store
    (let ([log-path (get-event-log-path)])
      (set! results (cons `(file-store ,(file-exists? log-path)) results)))
    
    ;; Check Redis if connected
    (let ([conn (redis-conn)])
      (if conn
          (let ([pong (redis-ping conn)])
            (set! results (cons `(redis ,(equal? pong "PONG")) results)))
          (set! results (cons '(redis #f) results))))
    
    results))

