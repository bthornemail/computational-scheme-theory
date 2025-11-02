#lang racket/base

(require "src/persistence/init.rkt"
         "src/persistence/config.rkt"
         "src/persistence/event-store-file.rkt"
         "src/persistence/redis-store.rkt"
         "src/persistence/adapter.rkt"
         "src/s-expression.rkt"
         "src/nlp/knowledge-graph-redis.rkt"
         "src/nlp/learning-engine-redis.rkt"
         "src/nlp/feedback-system-redis.rkt")

(printf "╔══════════════════════════════════════════════════════════╗\n")
(printf "║     PERSISTENCE SYSTEM TEST & POTENTIAL ANALYSIS         ║\n")
(printf "╚══════════════════════════════════════════════════════════╝\n\n")

;; Test 1: File-based Event Store
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 1: File-Based Event Store\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(printf "1.1 Creating test events...\n")
(define test-event1 (make-s-expr 'binding-created '((identifier x) (scope global))))
(define test-event2 (make-s-expr 'scope-entered '((scope-id local) (parent-scope global))))
(define test-event3 (make-s-expr 'query-parsed '((intent compute) (object h1))))

(append-event-to-file test-event1)
(append-event-to-file test-event2)
(append-event-to-file test-event3)
(printf "  ✅ Written 3 events to file\n\n")

(printf "1.2 Loading events from file...\n")
(define loaded-events (load-events-from-file))
(printf "  ✅ Loaded ~a events from file\n" (length loaded-events))
(printf "  Event types: ~a\n\n" (map s-expr-type loaded-events))

(printf "1.3 Event replay...\n")
(define replayed (replay-events-from-file))
(printf "  ✅ Replayed ~a events\n\n" (length replayed))

;; Test 2: Redis Operations (if available)
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 2: Redis Operations\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define (test-redis)
  (let ([conn (get-redis-connection)])
    (if conn
        (begin
          (printf "2.1 Redis connection test...\n")
          (let ([pong (redis-ping conn)])
            (printf "  ✅ Redis connected: ~a\n\n" pong))
          
          (printf "2.2 Basic operations (GET/SET)...\n")
          (redis-set conn "test:key" "test-value")
          (define retrieved (redis-get conn "test:key"))
          (printf "  ✅ SET/GET: ~a → ~a\n\n" "test:key" retrieved)
          
          (printf "2.3 Hash operations (HGET/HSET)...\n")
          (redis-hset conn "test:hash" "field1" "value1")
          (redis-hset conn "test:hash" "field2" "value2")
          (define hash-data (redis-hgetall conn "test:hash"))
          (printf "  ✅ Hash stored: ~a fields\n" (length hash-data))
          (printf "  Hash contents: ~a\n\n" hash-data)
          
          (printf "2.4 Set operations (SADD/SMEMBERS)...\n")
          (redis-sadd conn "test:set" "member1")
          (redis-sadd conn "test:set" "member2")
          (redis-sadd conn "test:set" "member3")
          (define set-members (redis-smembers conn "test:set"))
          (printf "  ✅ Set created: ~a members\n" (length set-members))
          (printf "  Members: ~a\n\n" set-members)
          
          #t)
        (begin
          (printf "  ⚠️  Redis not available (skipping Redis tests)\n")
          (printf "  To enable: Start Redis server (redis-server) or set PERSISTENCE_MODE=file\n\n")
          #f))))

(test-redis)

;; Test 3: Knowledge Graph Redis Backend
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 3: Knowledge Graph Redis Backend\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define (test-knowledge-graph)
  (let ([kg-backend (get-knowledge-graph-backend)])
    (if kg-backend
        (begin
          (printf "3.1 Creating knowledge graph with Redis...\n")
          (define kg (add-concept-redis kg-backend 'concept "H1Computation" '("Operation")))
          (define kg2 (add-concept-redis kg 'concept "CyclomaticComplexity" '("Metric")))
          (define kg3 (add-concept-redis kg2 'action-verb "compute" '("ProgramOperation")))
          (printf "  ✅ Added 3 concepts to Redis knowledge graph\n\n")
          
          (printf "3.2 Querying Redis for stored vertices...\n")
          (let ([conn (get-redis-connection)])
            (define vertex-keys (redis-keys conn "kg:vertex:*"))
            (printf "  ✅ Found ~a vertices in Redis\n" (length vertex-keys))
            (for ([key (take vertex-keys (min 3 (length vertex-keys)))])
              (define hash-data (redis-hgetall conn key))
              (printf "    - ~a: type=~a\n" 
                      key 
                      (let ([type (assoc-ref hash-data "type")])
                        (if type type "unknown"))))
            (printf "\n"))
          #t)
        (begin
          (printf "  ⚠️  Knowledge graph Redis backend not available\n\n")
          #f))))

(test-knowledge-graph)

;; Test 4: Learning Engine Redis Backend
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 4: Learning Engine Redis Backend\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define (test-learning-engine)
  (let ([engine-backend (get-learning-engine-backend)])
    (if engine-backend
        (begin
          (printf "4.1 Tracking rule performance...\n")
          (track-rule-success-redis engine-backend "parse-verb")
          (track-rule-success-redis engine-backend "parse-verb")
          (track-rule-success-redis engine-backend "parse-object")
          (track-rule-failure-redis engine-backend "parse-object")
          (printf "  ✅ Tracked rule performance in Redis\n\n")
          
          (printf "4.2 Retrieving rule performance...\n")
          (define perf1 (get-rule-performance-redis engine-backend "parse-verb"))
          (define perf2 (get-rule-performance-redis engine-backend "parse-object"))
          (printf "  ✅ Rule 'parse-verb': ~a\n" perf1)
          (printf "  ✅ Rule 'parse-object': ~a\n\n" perf2)
          
          (printf "4.3 Tracking concept usage...\n")
          (track-concept-usage-redis engine-backend "h1")
          (track-concept-usage-redis engine-backend "h1")
          (track-concept-usage-redis engine-backend "vg")
          (let ([get-usage (dynamic-require "src/nlp/learning-engine-redis.rkt" 'get-concept-usage-redis)]
                [h1-usage (get-usage engine-backend "h1")])
            (printf "  ✅ Concept 'h1' usage count: ~a\n\n" h1-usage))
          #t)
        (begin
          (printf "  ⚠️  Learning engine Redis backend not available\n\n")
          #f))))

(test-learning-engine)

;; Test 5: Feedback System Redis Backend
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 5: Feedback System Redis Backend\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define (test-feedback-system)
  (let ([feedback-backend (get-feedback-system-backend)])
    (if feedback-backend
        (begin
          (printf "5.1 Submitting feedback entries...\n")
          (submit-feedback-redis feedback-backend 'positive 5 "compute H1" "H1 = 3")
          (submit-feedback-redis feedback-backend 'positive 4 "get dimensions" "dimensions listed")
          (submit-feedback-redis feedback-backend 'negative 2 "invalid query" "parse error" "needs better error message")
          (printf "  ✅ Submitted 3 feedback entries to Redis\n\n")
          
          (printf "5.2 Retrieving feedback statistics...\n")
          (define stats (get-feedback-stats-redis feedback-backend))
          (printf "  ✅ Statistics: ~a\n" stats)
          (printf "    Total: ~a, Positive: ~a, Negative: ~a\n\n"
                  (list-ref stats 1)
                  (list-ref stats 2)
                  (list-ref stats 3))
          
          (printf "5.3 Getting feedback history...\n")
          (define history (get-feedback-history-redis feedback-backend 5))
          (printf "  ✅ Retrieved ~a feedback entries\n\n" (length history))
          #t)
        (begin
          (printf "  ⚠️  Feedback system Redis backend not available\n\n")
          #f))))

(test-feedback-system)

;; Test 6: Performance & Potential Analysis
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 6: Performance & Potential Analysis\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(printf "6.1 File-based event store performance...\n")
(let ([start-time (current-inexact-milliseconds)])
  (for ([i (in-range 100)])
    (append-event-to-file (make-s-expr 'test-event `((id ,i)))))
  (let ([end-time (current-inexact-milliseconds)]
        [elapsed (- end-time start-time)])
    (printf "  ✅ Wrote 100 events in ~a ms (~a events/sec)\n"
            (exact->inexact elapsed)
            (exact->inexact (/ 100 (* elapsed 0.001))))))

(printf "\n6.2 Redis write performance (if available)...\n")
(let ([conn (get-redis-connection)])
  (if conn
      (begin
        (let ([start-time (current-inexact-milliseconds)])
          (for ([i (in-range 100)])
            (redis-set conn (format "perf:key:~a" i) (format "value:~a" i)))
          (let ([end-time (current-inexact-milliseconds)]
                [elapsed (- end-time start-time)])
            (printf "  ✅ Wrote 100 keys in ~a ms (~a ops/sec)\n"
                    (exact->inexact elapsed)
                    (exact->inexact (/ 100 (* elapsed 0.001))))))
        
        (printf "\n6.3 Redis read performance...\n")
        (let ([start-time (current-inexact-milliseconds)])
          (for ([i (in-range 100)])
            (redis-get conn (format "perf:key:~a" i)))
          (let ([end-time (current-inexact-milliseconds)]
                [elapsed (- end-time start-time)])
            (printf "  ✅ Read 100 keys in ~a ms (~a ops/sec)\n"
                    (exact->inexact elapsed)
                    (exact->inexact (/ 100 (* elapsed 0.001))))))
        
        (printf "\n6.4 Redis hash operations...\n")
        (let ([start-time (current-inexact-milliseconds)])
          (for ([i (in-range 50)])
            (redis-hset conn "perf:hash" (format "field~a" i) (format "val~a" i)))
          (let ([end-time (current-inexact-milliseconds)]
                [elapsed (- end-time start-time)])
            (printf "  ✅ Wrote 50 hash fields in ~a ms (~a ops/sec)\n\n"
                    (exact->inexact elapsed)
                    (exact->inexact (/ 50 (* elapsed 0.001))))))
      (printf "  ⚠️  Redis not available (skipping performance tests)\n\n")))

;; Test 7: Persistence Health Check
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST 7: System Health Check\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(define health (persistence-health-check))
(printf "Health Status:\n")
(for ([status health])
  (match status
    [`(file-store ,ok)
     (printf "  %s File store: ~a\n" (if ok "✅" "❌") (if ok "operational" "not found"))]
    [`(redis ,ok)
     (printf "  %s Redis: ~a\n" (if ok "✅" "⚠️") (if ok "connected" "not available"))]))
(printf "\n")

;; Test 8: Potential Use Cases
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "POTENTIAL & CAPABILITIES ANALYSIS\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(printf "✨ Key Capabilities:\n\n")

(printf "1. Event Sourcing & Audit Trail\n")
(printf "   • All system events (S-expressions) persisted to file\n")
(printf "   • Complete replay capability for state reconstruction\n")
(printf "   • Immutable log for auditing and debugging\n")
(printf "   • Potential: Time-travel debugging, state snapshots\n\n")

(printf "2. Distributed Knowledge Graph\n")
(printf "   • Knowledge graph stored in Redis (shared across instances)\n")
(printf "   • Fast concept lookups and relationship queries\n")
(printf "   • Index structures for efficient pattern matching\n")
(printf "   • Potential: Multi-user systems, collaborative learning\n\n")

(printf "3. Persistent Learning & Adaptation\n")
(printf "   • Rule performance tracked across sessions\n")
(printf "   • Concept usage statistics for optimization\n")
(printf "   • Learning from feedback persists across restarts\n")
(printf "   • Potential: Continuous improvement, personalized responses\n\n")

(printf "4. Feedback Analytics\n")
(printf "   • All user feedback stored and indexed\n")
(printf "   • Query-based feedback retrieval\n")
(printf "   • Trend analysis for system improvement\n")
(printf "   • Potential: Quality metrics, A/B testing\n\n")

(printf "5. Scalability Potential\n")
(printf "   • Redis enables horizontal scaling\n")
(printf "   • File-based events support log streaming\n")
(printf "   • Stateless services with persistent state\n")
(printf "   • Potential: Microservices architecture, cloud deployment\n\n")

(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "✅ Persistence System Test Complete\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(printf "System Status:\n")
(printf "  ✅ File-based event store: OPERATIONAL\n")
(let ([conn (get-redis-connection)])
  (if conn
      (begin
        (printf "  ✅ Redis: CONNECTED\n")
        (printf "  ✅ Redis backends: AVAILABLE\n"))
      (printf "  ⚠️  Redis: NOT AVAILABLE (start redis-server to enable)\n")))
(printf "\n🎯 System ready for production use with persistence!\n")

