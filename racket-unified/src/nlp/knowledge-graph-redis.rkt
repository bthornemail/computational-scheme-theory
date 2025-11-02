#lang racket/base

(require racket/match
         racket/set
         "../persistence/redis-store.rkt"
         "../persistence/config.rkt"
         "../s-expression.rkt"
         "semantic-lattice.rkt"
         "semantic-frame.rkt")

(provide
 knowledge-graph-redis
 knowledge-graph-redis?
 empty-knowledge-graph-redis
 add-concept-redis
 enrich-semantic-frame-redis
 update-graph-from-event-redis
 load-knowledge-graph-from-redis
 save-knowledge-graph-to-redis
 migrate-in-memory-to-redis)

;; ============================================================
;; KNOWLEDGE GRAPH REDIS BACKEND
;; ============================================================

;; Knowledge graph with Redis backend (structure for API compatibility)
(struct knowledge-graph-redis (conn) #:transparent)

;; Create empty knowledge graph with Redis backend
(define (empty-knowledge-graph-redis conn)
  "Create empty knowledge graph with Redis connection"
  (knowledge-graph-redis conn))

;; Serialize lattice node to Redis hash
(define (serialize-lattice-node node)
  "Serialize lattice node to key-value pairs for Redis hash"
  (let ([id (symbol->string (lattice-node-id node))]
        [type (symbol->string (lattice-node-type node))]
        [props (lattice-node-properties node)]
        [parents (map symbol->string (lattice-node-parents node))]
        [children (map symbol->string (lattice-node-children node))])
    `((id ,id)
      (type ,type)
      (properties ,(if (list? props) (format "~s" props) ""))
      (parents ,(format "~s" parents))
      (children ,(format "~s" children)))))

;; Deserialize lattice node from Redis hash
(define (deserialize-lattice-node vertex-id hash-data)
  "Deserialize lattice node from Redis hash data"
  (let ([id (string->symbol vertex-id)]
        [type (string->symbol (or (assoc-ref hash-data "type") "concept"))]
        [props-str (or (assoc-ref hash-data "properties") "()")]
        [parents-str (or (assoc-ref hash-data "parents") "()")]
        [children-str (or (assoc-ref hash-data "children") "()")])
    (let* ([props (read (open-input-string props-str))]
           [parents (map string->symbol (read (open-input-string parents-str)))]
           [children (map string->symbol (read (open-input-string children-str)))])
      (lattice-node id type props parents children))))

;; Helper: Get value from association list
(define (assoc-ref alist key)
  "Get value from association list by key"
  (let ([pair (assoc key alist)])
    (if pair (cdr pair) #f)))

;; Add concept to knowledge graph (Redis backend)
(define (add-concept-redis graph concept-type name parents)
  "Add concept to knowledge graph using Redis"
  (let ([conn (knowledge-graph-redis-conn graph)])
    (define vertex-id (format "~a-~a" concept-type name))
    (define vertex-id-key (format "kg:vertex:~a" vertex-id))
    
    ;; Store vertex as hash
    (redis-hset conn vertex-id-key "id" vertex-id)
    (redis-hset conn vertex-id-key "type" (symbol->string concept-type))
    (redis-hset conn vertex-id-key "name" name)
    (redis-hset conn vertex-id-key "parents" (format "~s" (map symbol->string parents)))
    
    ;; Add to type index
    (redis-sadd conn (format "kg:index:type:~a" concept-type) vertex-id)
    
    ;; Add parent edges
    (for-each (lambda (parent-id)
                (define edge-key (format "kg:edge:~a->~a" vertex-id parent-id))
                (redis-sadd conn edge-key 'subsumes)
                (redis-sadd conn (format "kg:edges:from:~a" vertex-id) edge-key)
                (redis-sadd conn (format "kg:edges:to:~a" parent-id) edge-key))
              parents)
    
    ;; Store label
    (redis-hset conn (format "kg:label:~a" vertex-id) "type" (symbol->string concept-type))
    
    graph))

;; Load knowledge graph from Redis
(define (load-knowledge-graph-from-redis conn)
  "Load all vertices and edges from Redis"
  (let ([vertex-keys (redis-keys conn "kg:vertex:*")]
        [vertices (set)])
    (for ([key vertex-keys])
      (let* ([vertex-id (string-replace key "kg:vertex:" "")]
             [hash-data (redis-hgetall conn key)])
        (when hash-data
          (let ([node (deserialize-lattice-node vertex-id hash-data)])
            (set! vertices (set-add vertices node))))))
    vertices))

;; Save in-memory knowledge graph to Redis
(define (save-knowledge-graph-to-redis graph-memory conn)
  "Migrate in-memory knowledge graph to Redis"
  (let ([vertices (knowledge-graph-vertices graph-memory)]
        [edges (knowledge-graph-edges graph-memory)]
        [labels (knowledge-graph-labels graph-memory)])
    
    ;; Save vertices
    (for ([vertex (in-set vertices)])
      (let* ([id (lattice-node-id vertex)]
             [vertex-key (format "kg:vertex:~a" id)]
             [node-data (serialize-lattice-node vertex)])
        (for-each (lambda (pair)
                    (redis-hset conn vertex-key (car pair) (cadr pair)))
                  node-data)
        (redis-sadd conn (format "kg:index:type:~a" (lattice-node-type vertex)) (symbol->string id))))
    
    ;; Save edges
    (for ([edge (in-set edges)])
      (match edge
        [(list from to label)
         (let ([edge-key (format "kg:edge:~a->~a" from to)])
           (redis-sadd conn edge-key label)
           (redis-sadd conn (format "kg:edges:from:~a" from) edge-key)
           (redis-sadd conn (format "kg:edges:to:~a" to) edge-key))]))
    
    ;; Save labels
    (for ([(vertex-id label-type) (in-hash labels)])
      (redis-hset conn (format "kg:label:~a" vertex-id) "type" (symbol->string label-type))))
  
  (empty-knowledge-graph-redis conn))

;; Migrate in-memory graph to Redis
(define (migrate-in-memory-to-redis graph-memory conn)
  "Migrate in-memory knowledge graph to Redis backend"
  (save-knowledge-graph-to-redis graph-memory conn))

;; Enrich semantic frame with knowledge graph (Redis)
(define (enrich-semantic-frame-redis frame graph)
  "Enrich semantic frame with knowledge graph data from Redis"
  (let ([conn (knowledge-graph-redis-conn graph)]
        [concepts (semantic-frame-concepts frame)])
    (define enriched-concepts
      (map (lambda (concept)
             (let ([concept-name (if (pair? concept) (cadr concept) concept)])
               ;; Look up concept in Redis
               (let ([matching-keys (redis-keys conn (format "kg:vertex:*~a*" concept-name))])
                 (if (not (null? matching-keys))
                     (let* ([key (car matching-keys)]
                            [vertex-id (string-replace key "kg:vertex:" "")]
                            [hash-data (redis-hgetall conn key)])
                       (if hash-data
                           (deserialize-lattice-node vertex-id hash-data)
                           concept))
                     concept))))
           concepts))
    (struct-copy semantic-frame frame [concepts enriched-concepts])))

;; Update graph from parse event (Redis backend)
(define (update-graph-from-event-redis graph event)
  "Update knowledge graph from parse event using Redis"
  (match (s-expr-type event)
    ['query-parsed
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'semantic-frame event-data))
           (let ([semantic-frame (cadr (assoc 'semantic-frame event-data))])
             (add-concepts-from-frame-redis graph semantic-frame))
           graph))]
    ['verb-parsed
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'verb event-data))
           (let ([verb (cadr (assoc 'verb event-data))])
             (add-concept-redis graph 'action-verb verb '("ProgramOperation")))
           graph))]
    ['entity-resolved
     (let ([event-data (s-expr-data event)])
       (if (and (list? event-data) (assoc 'entity event-data))
           (let ([entity (cadr (assoc 'entity event-data))])
             (add-concept-redis graph 'entity entity '("ProgramEntity")))
           graph))]
    [else graph]))

;; Helper: Add concepts from semantic frame (Redis)
(define (add-concepts-from-frame-redis graph frame)
  "Add concepts from semantic frame to knowledge graph using Redis"
  (define concepts (semantic-frame-concepts frame))
  (foldl (lambda (concept g)
           (match concept
             [`(action-verb ,verb)
              (add-concept-redis g 'action-verb verb '("Operation"))]
             [`(object ,obj)
              (add-concept-redis g 'object obj '("Metric"))]
             [`(entity ,type ,id)
              (add-concept-redis g 'entity (format "~a-~a" type id) (list type))]
             [else g]))
         graph
         concepts))

