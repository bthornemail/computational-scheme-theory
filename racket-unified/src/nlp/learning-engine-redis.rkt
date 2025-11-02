#lang racket/base

(require racket/match
         "../persistence/redis-store.rkt"
         "../persistence/config.rkt")

(provide
 learning-engine-redis
 learning-engine-redis?
 initialize-learning-engine-redis
 track-rule-success-redis
 track-rule-failure-redis
 get-rule-performance-redis
 get-all-rule-performance-redis
 track-concept-usage-redis
 get-concept-usage-redis
 increment-interaction-counter-redis
 get-interaction-count-redis
 save-learned-concept-redis
 get-learned-concepts-redis)

;; ============================================================
;; LEARNING ENGINE REDIS BACKEND
;; ============================================================

;; Learning engine with Redis backend
(struct learning-engine-redis (conn) #:transparent)

;; Initialize learning engine with Redis
(define (initialize-learning-engine-redis conn)
  "Initialize learning engine with Redis connection"
  (learning-engine-redis conn))

;; Track rule performance on success
(define (track-rule-success-redis engine rule-id)
  "Track successful rule application in Redis"
  (let ([conn (learning-engine-redis-conn engine)]
        [rule-key (format "learning:rule:~a" rule-id)])
    (let* ([current-success (string->number (or (redis-hget conn rule-key "success") "0"))]
           [current-total (string->number (or (redis-hget conn rule-key "total") "0"))]
           [new-success (+ current-success 1)]
           [new-total (+ current-total 1)]
           [new-rate (/ new-success new-total)])
      (redis-hset conn rule-key "rule-id" (format "~a" rule-id))
      (redis-hset conn rule-key "success" (format "~a" new-success))
      (redis-hset conn rule-key "total" (format "~a" new-total))
      (redis-hset conn rule-key "rate" (format "~a" new-rate))
      (redis-sadd conn "learning:rules" rule-id))))

;; Track rule performance on failure
(define (track-rule-failure-redis engine rule-id)
  "Track failed rule application in Redis"
  (let ([conn (learning-engine-redis-conn engine)]
        [rule-key (format "learning:rule:~a" rule-id)])
    (let* ([current-success (string->number (or (redis-hget conn rule-key "success") "0"))]
           [current-failure (string->number (or (redis-hget conn rule-key "failure") "0"))]
           [current-total (string->number (or (redis-hget conn rule-key "total") "0"))]
           [new-failure (+ current-failure 1)]
           [new-total (+ current-total 1)]
           [new-rate (/ current-success new-total)])
      (redis-hset conn rule-key "rule-id" (format "~a" rule-id))
      (redis-hset conn rule-key "success" (format "~a" current-success))
      (redis-hset conn rule-key "failure" (format "~a" new-failure))
      (redis-hset conn rule-key "total" (format "~a" new-total))
      (redis-hset conn rule-key "rate" (format "~a" new-rate))
      (redis-sadd conn "learning:rules" rule-id))))

;; Get performance for specific rule
(define (get-rule-performance-redis engine rule-id)
  "Get performance statistics for a rule from Redis"
  (let* ([conn (learning-engine-redis-conn engine)]
         [rule-key (format "learning:rule:~a" rule-id)]
         [hash-data (redis-hgetall conn rule-key)])
    (if (and hash-data (not (null? hash-data)))
        (let ([success-str (assoc-ref hash-data "success")]
              [failure-str (assoc-ref hash-data "failure")]
              [total-str (assoc-ref hash-data "total")]
              [rate-str (assoc-ref hash-data "rate")])
          (if (and success-str failure-str total-str rate-str)
              (list 'rule-performance
                    rule-id
                    (string->number success-str)
                    (string->number failure-str)
                    (string->number total-str)
                    (string->number rate-str))
              #f))
        #f)))

;; Get all rule performance statistics
(define (get-all-rule-performance-redis engine)
  "Get all rule performance statistics from Redis"
  (let* ([conn (learning-engine-redis-conn engine)]
         [rule-ids (redis-smembers conn "learning:rules")])
    (filter-map (lambda (rule-id)
                  (get-rule-performance-redis engine rule-id))
                rule-ids)))

;; Track concept usage
(define (track-concept-usage-redis engine concept-id)
  "Track usage of a concept in Redis"
  (let ([conn (learning-engine-redis-conn engine)]
        [concept-key (format "learning:concept:~a" concept-id)])
    (let ([current-count (string->number (or (redis-get conn concept-key) "0"))])
      (redis-set conn concept-key (format "~a" (+ current-count 1)))
      (redis-sadd conn "learning:concepts" concept-id))))

;; Get concept usage count
(define (get-concept-usage-redis engine concept-id)
  "Get usage count for a concept from Redis"
  (let ([conn (learning-engine-redis-conn engine)]
        [concept-key (format "learning:concept:~a" concept-id)])
    (let ([count-str (redis-get conn concept-key)])
      (if count-str (string->number count-str) 0))))

;; Increment interaction counter
(define (increment-interaction-counter-redis engine)
  "Increment interaction counter in Redis"
  (let ([conn (learning-engine-redis-conn engine)])
    (let ([current (string->number (or (redis-get conn "learning:interactions") "0"))])
      (redis-set conn "learning:interactions" (format "~a" (+ current 1))))))

;; Get interaction count
(define (get-interaction-count-redis engine)
  "Get total interaction count from Redis"
  (let ([conn (learning-engine-redis-conn engine)])
    (let ([count-str (redis-get conn "learning:interactions")])
      (if count-str (string->number count-str) 0))))

;; Save learned concept
(define (save-learned-concept-redis engine concept-id concept-data)
  "Save learned concept to Redis"
  (let ([conn (learning-engine-redis-conn engine)]
        [concept-key (format "learning:learned:~a" concept-id)])
    (redis-hset conn concept-key "id" (format "~a" concept-id))
    (redis-hset conn concept-key "data" (format "~s" concept-data))
    (redis-sadd conn "learning:learned:concepts" concept-id)))

;; Get learned concepts
(define (get-learned-concepts-redis engine)
  "Get all learned concept IDs from Redis"
  (let ([conn (learning-engine-redis-conn engine)])
    (redis-smembers conn "learning:learned:concepts")))

;; Helper: Get value from association list
(define (assoc-ref alist key)
  "Get value from association list by key"
  (let ([pair (assoc key alist)])
    (if pair (cdr pair) #f)))

;; Helper: filter-map
(define (filter-map fn lst)
  "Map and filter out #f values"
  (filter (lambda (x) x) (map fn lst)))

