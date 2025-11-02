#lang racket/base

(require racket/tcp
         racket/port
         racket/string
         "config.rkt")

(provide
 redis-connection
 redis-connection?
 redis-connect
 redis-disconnect
 redis-ping
 redis-get
 redis-set
 redis-del
 redis-exists?
 redis-hget
 redis-hset
 redis-hdel
 redis-hgetall
 redis-sadd
 redis-smembers
 redis-srem
 redis-keys
 redis-flushall)

;; ============================================================
;; REDIS CLIENT - Minimal TCP-based implementation
;; ============================================================

;; Redis connection structure
(struct redis-connection (host port password in out) #:transparent #:mutable)

;; Connect to Redis server
(define (redis-connect [host #f] [port #f] [password #f])
  "Connect to Redis server"
  (let ([h (or host (get-redis-host))]
        [p (or port (get-redis-port))]
        [pass (or password (get-redis-password))])
    (let-values ([(in out) (tcp-connect h p)])
      (let ([conn (redis-connection h p pass in out)])
        ;; Authenticate if password provided
        (when pass
          (redis-send-command conn (list "AUTH" pass)))
        conn))))

;; Disconnect from Redis
(define (redis-disconnect conn)
  "Disconnect from Redis server"
  (when conn
    (close-input-port (redis-connection-in conn))
    (close-output-port (redis-connection-out conn))))

;; Send command to Redis and read response
(define (redis-send-command conn command-args)
  "Send Redis command and return response"
  (define out (redis-connection-out conn))
  (define in (redis-connection-in conn))
  
  ;; Format as Redis protocol: *N\r\n$len\r\narg\r\n...
  (define arg-count (length command-args))
  (fprintf out "*~a\r\n" arg-count)
  (for ([arg command-args])
    (define arg-str (if (string? arg) arg (format "~a" arg)))
    (fprintf out "$~a\r\n~a\r\n" (string-length arg-str) arg-str))
  (flush-output out)
  
  ;; Read response
  (redis-read-response in))

;; Read Redis response
(define (redis-read-response in)
  "Read Redis protocol response"
  (define line (read-line in 'any))
  (when (eof-object? line)
    (error "Unexpected EOF from Redis"))
  
  (cond
    [(string-prefix? line "+")  ; Simple string
     (substring line 1)]
    [(string-prefix? line "-")  ; Error
     (error (format "Redis error: ~a" (substring line 1)))]
    [(string-prefix? line ":")  ; Integer
     (string->number (substring line 1))]
    [(string-prefix? line "$")  ; Bulk string
     (let ([len (string->number (substring line 1))])
       (cond
         [(= len -1) #f]  ; Null
         [else
          (define data (read-bytes len in))
          (read-line in 'any)  ; Consume \r\n
          (bytes->string/utf-8 data)]))]
    [(string-prefix? line "*")  ; Array
     (let ([count (string->number (substring line 1))])
       (cond
         [(= count -1) #f]  ; Null array
         [else
          (for/list ([i (in-range count)])
            (redis-read-response in))]))]
    [else
     (error (format "Unknown Redis response format: ~a" line))]))

;; Basic operations
(define (redis-ping conn)
  "PING Redis server"
  (redis-send-command conn '("PING")))

(define (redis-get conn key)
  "GET key from Redis"
  (redis-send-command conn (list "GET" key)))

(define (redis-set conn key value)
  "SET key-value in Redis"
  (redis-send-command conn (list "SET" key value)))

(define (redis-del conn key)
  "DELETE key from Redis"
  (redis-send-command conn (list "DEL" key)))

(define (redis-exists? conn key)
  "Check if key exists"
  (= 1 (redis-send-command conn (list "EXISTS" key))))

;; Hash operations
(define (redis-hget conn hash-key field)
  "HGET field from hash"
  (redis-send-command conn (list "HGET" hash-key field)))

(define (redis-hset conn hash-key field value)
  "HSET field-value in hash"
  (redis-send-command conn (list "HSET" hash-key field value)))

(define (redis-hdel conn hash-key field)
  "HDEL field from hash"
  (redis-send-command conn (list "HDEL" hash-key field)))

(define (redis-hgetall conn hash-key)
  "HGETALL hash (returns list of field-value pairs)"
  (let ([result (redis-send-command conn (list "HGETALL" hash-key))])
    (if result
        ;; Convert flat list to association list
        (let loop ([remaining result]
                   [acc '()])
          (if (null? remaining)
              (reverse acc)
              (loop (cddr remaining)
                    (cons (cons (car remaining) (cadr remaining)) acc))))
        '())))

;; Set operations
(define (redis-sadd conn set-key member)
  "SADD member to set"
  (redis-send-command conn (list "SADD" set-key (if (string? member) member (format "~a" member)))))

(define (redis-smembers conn set-key)
  "SMEMBERS get all members of set"
  (let ([result (redis-send-command conn (list "SMEMBERS" set-key))])
    (if result result '())))

(define (redis-srem conn set-key member)
  "SREM remove member from set"
  (redis-send-command conn (list "SREM" set-key member)))

;; Key operations
(define (redis-keys conn pattern)
  "KEYS get all keys matching pattern"
  (let ([result (redis-send-command conn (list "KEYS" pattern))])
    (if result result '())))

(define (redis-flushall conn)
  "FLUSHALL delete all keys"
  (redis-send-command conn '("FLUSHALL")))

