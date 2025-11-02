#lang racket/base

(require racket/string)

(provide
 get-redis-host
 get-redis-port
 get-redis-password
 get-event-log-path
 get-persistence-mode
 initialize-persistence-config)

;; ============================================================
;; PERSISTENCE CONFIGURATION
;; ============================================================

;; Redis configuration
(define redis-host (make-parameter "localhost"))
(define redis-port (make-parameter 6379))
(define redis-password (make-parameter #f))

;; Event log configuration
(define event-log-path (make-parameter "data/events.log"))

;; Persistence mode: 'file, 'redis, or 'hybrid
(define persistence-mode (make-parameter 'hybrid))

;; Initialize configuration from environment variables
(define (initialize-persistence-config)
  "Initialize persistence configuration from environment variables"
  (let ([host (getenv "REDIS_HOST")]
        [port-str (getenv "REDIS_PORT")]
        [password (getenv "REDIS_PASSWORD")]
        [log-path (getenv "EVENT_LOG_PATH")]
        [mode-str (getenv "PERSISTENCE_MODE")])
    
    (when host
      (redis-host host))
    
    (when port-str
      (let ([port-num (string->number port-str)])
        (when port-num
          (redis-port port-num))))
    
    (when password
      (redis-password password))
    
    (when log-path
      (event-log-path log-path))
    
    (when mode-str
      (let ([mode-sym (string->symbol (string-downcase mode-str))])
        (when (member mode-sym '(file redis hybrid))
          (persistence-mode mode-sym))))))

;; Getters
(define (get-redis-host) (redis-host))
(define (get-redis-port) (redis-port))
(define (get-redis-password) (redis-password))
(define (get-event-log-path) (event-log-path))
(define (get-persistence-mode) (persistence-mode))

