#lang racket/base

(require racket/match
         racket/hash
         racket/list
         racket/string
         "learning-engine.rkt"
         "parsing-fsm.rkt"
         "layer4-core.rkt")


(provide
 performance-metrics
 performance-metrics?
 performance-metrics-query-count
 performance-metrics-avg-parse-time
 performance-metrics-success-rate
 performance-metrics-avg-events-per-query
 get-performance-metrics
 track-query-performance
 track-parse-time
 get-query-analytics
 performance-alert
 performance-alert?
 performance-alert-type
 performance-alert-message
 performance-alert-severity
 check-performance-thresholds
 generate-performance-report)

;; ============================================================
;; PERFORMANCE MONITORING AND ANALYTICS
;; ============================================================

;; Performance metrics structure
(struct performance-metrics (query-count avg-parse-time success-rate avg-events-per-query) #:transparent)

;; Performance alert structure
(struct performance-alert (type message severity timestamp) #:transparent)

;; Query performance tracking
(define query-performance-log (make-hash))  ; timestamp -> query metrics

;; Parse time tracking
(define parse-time-log '())  ; List of (timestamp . parse-time-ms)

;; Success/failure tracking
(define success-count (box 0))
(define failure-count (box 0))

;; Performance thresholds
(define MAX_PARSE_TIME_MS 1000)  ; 1 second
(define MIN_SUCCESS_RATE 0.8)    ; 80%
(define MAX_AVG_EVENTS 10)       ; Average events per query

;; Track query performance
(define (track-query-performance query m-expr events parse-time success?)
  "Track performance metrics for a query"
  (let* ([timestamp (current-seconds)]
         [metrics `((query ,query)
                    (m-expression ,(if m-expr #t #f))
                    (event-count ,(length events))
                    (parse-time-ms ,parse-time)
                    (success ,success?)
                    (timestamp ,timestamp))])
    (hash-set! query-performance-log timestamp metrics)
    
    ;; Update counters
    (if success?
        (set-box! success-count (+ (unbox success-count) 1))
        (set-box! failure-count (+ (unbox failure-count) 1)))
    
    ;; Track parse time
    (track-parse-time parse-time)))

;; Track parse time
(define (track-parse-time parse-time-ms)
  "Track parse time for analytics"
  (set! parse-time-log (cons (cons (current-seconds) parse-time-ms) parse-time-log))
  ;; Keep only last 1000 entries
  (when (> (length parse-time-log) 1000)
    (set! parse-time-log (take parse-time-log 1000))))

;; Get performance metrics
(define (get-performance-metrics)
  "Get current performance metrics"
  (let* ([total-queries (+ (unbox success-count) (unbox failure-count))]
         [success-rate (if (> total-queries 0)
                          (/ (unbox success-count) total-queries)
                          0.0)]
         [parse-times (map cdr parse-time-log)]
         [avg-parse-time (if (null? parse-times)
                            0.0
                            (/ (apply + parse-times) (length parse-times)))]
         [all-metrics (hash-values query-performance-log)]
         [avg-events (if (null? all-metrics)
                        0.0
                        (/ (apply + (map (lambda (m) (cadr (assq 'event-count m))) all-metrics))
                           (length all-metrics)))])
    (performance-metrics total-queries avg-parse-time success-rate avg-events)))

;; Get query analytics
(define (get-query-analytics [days 7])
  "Get query analytics for specified period"
  (let* ([cutoff (- (current-seconds) (* days 24 60 60))]
         [recent-queries (filter (lambda (entry)
                                   (> (car entry) cutoff))
                                 (hash->list query-performance-log))]
         [total (length recent-queries)]
         [successful (length (filter (lambda (entry)
                                       (cadr (assq 'success (cdr entry))))
                                     recent-queries))]
         [failed (- total successful)]
         [parse-times (map (lambda (entry)
                             (cadr (assq 'parse-time-ms (cdr entry))))
                           recent-queries)]
         [avg-parse-time (if (null? parse-times)
                            0.0
                            (/ (apply + parse-times) (length parse-times)))]
         [event-counts (map (lambda (entry)
                              (cadr (assq 'event-count (cdr entry))))
                            recent-queries)]
         [avg-events (if (null? event-counts)
                       0.0
                       (/ (apply + event-counts) (length event-counts)))])
    `((period-days ,days)
      (total-queries ,total)
      (successful ,successful)
      (failed ,failed)
      (success-rate ,(if (> total 0) (/ successful total) 0.0))
      (avg-parse-time-ms ,avg-parse-time)
      (avg-events-per-query ,avg-events)
      (min-parse-time ,(if (null? parse-times) 0 (apply min parse-times)))
      (max-parse-time ,(if (null? parse-times) 0 (apply max parse-times))))))

;; Check performance thresholds and generate alerts
(define (check-performance-thresholds)
  "Check performance against thresholds and generate alerts"
  (let ([metrics (get-performance-metrics)]
        [alerts '()])
    
    ;; Check parse time
    (when (> (performance-metrics-avg-parse-time metrics) MAX_PARSE_TIME_MS)
      (set! alerts (cons (performance-alert 'slow-parse
                                             (format "Average parse time (~a ms) exceeds threshold (~a ms)"
                                                     (performance-metrics-avg-parse-time metrics)
                                                     MAX_PARSE_TIME_MS)
                                             'warning
                                             (current-seconds))
                         alerts)))
    
    ;; Check success rate
    (when (< (performance-metrics-success-rate metrics) MIN_SUCCESS_RATE)
      (set! alerts (cons (performance-alert 'low-success-rate
                                             (format "Success rate (~a%) below threshold (~a%)"
                                                     (* (performance-metrics-success-rate metrics) 100)
                                                     (* MIN_SUCCESS_RATE 100))
                                             'error
                                             (current-seconds))
                         alerts)))
    
    ;; Check event count
    (when (> (performance-metrics-avg-events-per-query metrics) MAX_AVG_EVENTS)
      (set! alerts (cons (performance-alert 'high-event-count
                                             (format "Average events per query (~a) exceeds threshold (~a)"
                                                     (performance-metrics-avg-events-per-query metrics)
                                                     MAX_AVG_EVENTS)
                                             'warning
                                             (current-seconds))
                         alerts)))
    
    alerts))

;; Generate performance report
(define (generate-performance-report)
  "Generate comprehensive performance report"
  (let* ([metrics (get-performance-metrics)]
         [learning-stats (get-learning-stats)]
         [rule-performance (get-all-rule-performance)]
         [analytics (get-query-analytics 30)]
         [alerts (check-performance-thresholds)])
    
    `((timestamp ,(current-seconds))
      (performance-metrics
       (query-count ,(performance-metrics-query-count metrics))
       (avg-parse-time-ms ,(performance-metrics-avg-parse-time metrics))
       (success-rate ,(performance-metrics-success-rate metrics))
       (avg-events-per-query ,(performance-metrics-avg-events-per-query metrics)))
      (learning-stats
       (total-interactions ,(learning-stats-total-interactions learning-stats))
       (learned-concepts ,(learning-stats-learned-concepts learning-stats))
       (rule-count ,(learning-stats-rule-count learning-stats)))
      (rule-performance
       ,@(map (lambda (rp)
                `((rule-id ,(rule-performance-rule-id rp))
                  (success-count ,(rule-performance-success-count rp))
                  (failure-count ,(rule-performance-failure-count rp))
                  (success-rate ,(rule-performance-success-rate rp))))
              rule-performance))
      (analytics ,analytics)
      (alerts ,(length alerts))
      (alert-details ,(map (lambda (alert)
                             `((type ,(performance-alert-type alert))
                               (message ,(performance-alert-message alert))
                               (severity ,(performance-alert-severity alert))))
                           alerts)))))

;; Get top queries by frequency
(define (get-top-queries [n 10])
  "Get top N most frequently executed queries"
  (let* ([query-counts (make-hash)]
         [all-metrics (hash-values query-performance-log)])
    (for-each (lambda (m)
                (let ([query (cadr (assq 'query m))])
                  (hash-set! query-counts query
                             (+ (hash-ref query-counts query (lambda () 0)) 1))))
              all-metrics)
    (take (sort (hash->list query-counts)
                (lambda (a b) (> (cdr a) (cdr b))))
          n)))

;; Get slowest queries
(define (get-slowest-queries [n 10])
  "Get N slowest queries"
  (let* ([all-metrics (hash-values query-performance-log)]
         [query-times (map (lambda (m)
                            (cons (cadr (assq 'query m))
                                  (cadr (assq 'parse-time-ms m))))
                          all-metrics)])
    (take (sort query-times (lambda (a b) (> (cdr a) (cdr b))))
          n)))

;; Get query success rate by pattern
(define (get-success-rate-by-pattern)
  "Get success rate grouped by query patterns"
  (let* ([pattern-stats (make-hash)]
         [all-metrics (hash-values query-performance-log)])
    (for-each (lambda (m)
                (let* ([query (cadr (assq 'query m))]
                       [pattern (extract-query-pattern query)]
                       [success (cadr (assq 'success m))])
                  (let ([current (hash-ref pattern-stats pattern
                                           (lambda () '(0 0)))])
                    (hash-set! pattern-stats pattern
                               (if success
                                   (list (+ (car current) 1) (cdr current))
                                   (list (car current) (+ (cdr current) 1)))))))
              all-metrics)
    (hash->list pattern-stats)))

;; Extract query pattern (simplified - first word + object)
(define (extract-query-pattern query)
  "Extract pattern from query for grouping"
  (if (string? query)
      (let ([words (string-split query)])
        (if (> (length words) 1)
            (format "~a ~a" (car words) (cadr words))
            query))
      query))

;; Export additional functions
(provide
 get-top-queries
 get-slowest-queries
 get-success-rate-by-pattern)

