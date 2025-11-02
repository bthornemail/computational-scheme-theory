#lang racket/base

(require racket/match
         racket/list
         "../persistence/redis-store.rkt"
         "../persistence/config.rkt")

(provide
 feedback-system-redis
 feedback-system-redis?
 initialize-feedback-system-redis
 submit-feedback-redis
 get-feedback-history-redis
 get-feedback-stats-redis
 get-feedback-for-query-redis
 analyze-feedback-trends-redis
 get-actionable-feedback-redis)

;; ============================================================
;; FEEDBACK SYSTEM REDIS BACKEND
;; ============================================================

;; Feedback system with Redis backend
(struct feedback-system-redis (conn) #:transparent)

;; Initialize feedback system with Redis
(define (initialize-feedback-system-redis conn)
  "Initialize feedback system with Redis connection"
  (feedback-system-redis conn))

;; Serialize feedback to Redis hash
(define (serialize-feedback type rating query response timestamp notes)
  "Serialize feedback data for Redis"
  `((type ,(format "~a" type))
    (rating ,(if (number? rating) (format "~a" rating) ""))
    (query ,query)
    (response ,(if response response ""))
    (timestamp ,(format "~a" timestamp))
    (notes ,(if notes notes ""))))

;; Deserialize feedback from Redis hash
(define (deserialize-feedback timestamp hash-data)
  "Deserialize feedback from Redis hash data"
  (let ([type-str (assoc-ref hash-data "type")]
        [rating-str (assoc-ref hash-data "rating")]
        [query (assoc-ref hash-data "query")]
        [response (assoc-ref hash-data "response")]
        [notes (assoc-ref hash-data "notes")])
    (list 'feedback
          (string->symbol type-str)
          (if (and rating-str (not (equal? rating-str "")))
              (string->number rating-str)
              #f)
          query
          (if (and response (not (equal? response ""))) response #f)
          (string->number timestamp)
          (if (and notes (not (equal? notes ""))) notes #f))))

;; Submit feedback to Redis
(define (submit-feedback-redis system type rating query response [notes #f])
  "Submit user feedback to Redis"
  (let* ([conn (feedback-system-redis-conn system)]
         [timestamp (current-seconds)]
         [feedback-key (format "feedback:~a" timestamp)]
         [fb-data (serialize-feedback type rating query response timestamp notes)])
    ;; Store feedback as hash
    (for-each (lambda (pair)
                (redis-hset conn feedback-key (car pair) (cadr pair)))
              fb-data)
    
    ;; Add to timestamp index (sorted set)
    (redis-sadd conn (format "feedback:index:timestamp") (format "~a" timestamp))
    
    ;; Index by query pattern (simplified - use query hash)
    (when query
      (let ([query-hash (format "~a" (hash query))])
        (redis-sadd conn (format "feedback:index:query:~a" query-hash) (format "~a" timestamp))))
    
    ;; Update statistics
    (update-feedback-stats-redis conn type rating)
    
    ;; Return feedback representation
    (list 'feedback type rating query response timestamp notes)))

;; Update feedback statistics in Redis
(define (update-feedback-stats-redis conn type rating)
  "Update aggregated feedback statistics"
  (let* ([stats-key "feedback:stats"]
         ;; Increment counters
         [total (string->number (or (redis-hget conn stats-key "total") "0"))]
         [positive (string->number (or (redis-hget conn stats-key "positive") "0"))]
         [negative (string->number (or (redis-hget conn stats-key "negative") "0"))]
         [neutral (string->number (or (redis-hget conn stats-key "neutral") "0"))]
         [rating-sum (string->number (or (redis-hget conn stats-key "rating-sum") "0"))]
         [rating-count (string->number (or (redis-hget conn stats-key "rating-count") "0"))])
    
    (redis-hset conn stats-key "total" (format "~a" (+ total 1)))
    
    (cond
      [(eq? type 'positive)
       (redis-hset conn stats-key "positive" (format "~a" (+ positive 1)))]
      [(eq? type 'negative)
       (redis-hset conn stats-key "negative" (format "~a" (+ negative 1)))]
      [else
       (redis-hset conn stats-key "neutral" (format "~a" (+ neutral 1)))])
    
    (when (number? rating)
      (redis-hset conn stats-key "rating-sum" (format "~a" (+ rating-sum rating)))
      (redis-hset conn stats-key "rating-count" (format "~a" (+ rating-count 1))))))

;; Get feedback history from Redis
(define (get-feedback-history-redis system [limit #f])
  "Get feedback history from Redis, optionally limited"
  (let* ([conn (feedback-system-redis-conn system)]
         [timestamp-keys (redis-smembers conn "feedback:index:timestamp")]
         [all-feedback
          (filter-map (lambda (timestamp-str)
                        (let* ([feedback-key (format "feedback:~a" timestamp-str)]
                               [hash-data (redis-hgetall conn feedback-key)])
                          (if hash-data
                              (deserialize-feedback timestamp-str hash-data)
                              #f)))
                      timestamp-keys)]
         [sorted (sort all-feedback
                      (lambda (a b)
                        (> (list-ref a 5) (list-ref b 5))))])
    (if limit
        (take sorted (min limit (length sorted)))
        sorted)))

;; Get feedback statistics from Redis
(define (get-feedback-stats-redis system)
  "Get feedback statistics from Redis"
  (let ([conn (feedback-system-redis-conn system)]
        [stats-key "feedback:stats"])
    (let ([hash-data (redis-hgetall conn stats-key)])
      (if hash-data
          (let ([total-str (assoc-ref hash-data "total")]
                [positive-str (assoc-ref hash-data "positive")]
                [negative-str (assoc-ref hash-data "negative")]
                [neutral-str (assoc-ref hash-data "neutral")]
                [rating-sum-str (assoc-ref hash-data "rating-sum")]
                [rating-count-str (assoc-ref hash-data "rating-count")])
            (let ([total (if total-str (string->number total-str) 0)]
                  [positive (if positive-str (string->number positive-str) 0)]
                  [negative (if negative-str (string->number negative-str) 0)]
                  [neutral (if neutral-str (string->number neutral-str) 0)]
                  [rating-sum (if rating-sum-str (string->number rating-sum-str) 0)]
                  [rating-count (if rating-count-str (string->number rating-count-str) 0)])
              (let ([avg-rating (if (> rating-count 0) (/ rating-sum rating-count) 0.0)])
                (list 'feedback-stats total positive negative neutral avg-rating))))
          (list 'feedback-stats 0 0 0 0 0.0)))))

;; Get feedback for specific query pattern
(define (get-feedback-for-query-redis system pattern)
  "Get feedback for queries matching pattern"
  (let* ([conn (feedback-system-redis-conn system)]
         [query-hash (format "~a" (hash pattern))]
         [timestamp-keys (redis-smembers conn (format "feedback:index:query:~a" query-hash))])
    (filter-map (lambda (timestamp-str)
                  (let* ([feedback-key (format "feedback:~a" timestamp-str)]
                         [hash-data (redis-hgetall conn feedback-key)])
                    (if hash-data
                        (deserialize-feedback timestamp-str hash-data)
                        #f)))
                timestamp-keys)))

;; Analyze feedback trends over time
(define (analyze-feedback-trends-redis system [days 7])
  "Analyze feedback trends over specified days"
  (let ([conn (feedback-system-redis-conn system)]
        [cutoff (- (current-seconds) (* days 24 60 60))])
    (let ([all-feedback (get-feedback-history-redis system)])
      (let ([recent (filter (lambda (fb)
                              (> (list-ref fb 5) cutoff))
                            all-feedback)])
        (let* ([positive (length (filter (lambda (f) (eq? (list-ref f 1) 'positive)) recent))]
               [negative (length (filter (lambda (f) (eq? (list-ref f 1) 'negative)) recent))]
               [neutral (length (filter (lambda (f) (eq? (list-ref f 1) 'neutral)) recent))])
          (list 'feedback-trends days (length recent) positive negative neutral))))))

;; Get actionable feedback (negative feedback with notes)
(define (get-actionable-feedback-redis system)
  "Get negative feedback with notes for improvement"
  (let ([all-feedback (get-feedback-history-redis system)])
    (filter (lambda (fb)
              (and (eq? (list-ref fb 1) 'negative)
                   (list-ref fb 6)))  ; Has notes
            all-feedback)))

;; Helper: Get value from association list
(define (assoc-ref alist key)
  "Get value from association list by key"
  (let ([pair (assoc key alist)])
    (if pair (cdr pair) #f)))

;; Helper: filter-map
(define (filter-map fn lst)
  "Map and filter out #f values"
  (filter (lambda (x) x) (map fn lst)))

;; Helper: hash function for strings
(define (hash str)
  "Simple hash function for strings"
  (let ([bytes (string->bytes/utf-8 str)])
    (for/fold ([acc 0]) ([b (in-bytes bytes)])
      (+ (* acc 31) b))))

